(in-package #:deflate-kit)

(defconstant +brotli-default-max-output-bytes+ (* 16 1024 1024))

(define-condition brotli-invalid-data (deflate-error)
  ((reason :initarg :reason :reader brotli-error-reason)
   (detail :initarg :detail :initform nil :reader brotli-error-detail))
  (:report (lambda (condition stream)
             (format stream "Brotli error~@[ at byte ~D~]: ~A"
                     (deflate-error-position condition)
                     (deflate-error-message condition)))))

(define-condition brotli-size-limit-exceeded (deflate-output-limit)
  ((reason :initarg :reason :reader brotli-size-limit-exceeded-reason)
   (observed :initarg :observed
             :reader brotli-size-limit-exceeded-observed)))

(defun %brotli-error (message reason &optional detail)
  (error 'brotli-invalid-data :message message :reason reason :detail detail))

(defun %brotli-octet-vector-p (value)
  (and (vectorp value)
       (every (lambda (octet)
                (and (integerp octet) (<= 0 octet 255)))
              value)))

(defstruct (brotli-bit-reader
             (:constructor %make-brotli-bit-reader (octets)))
  octets
  (position 0)
  (buffer 0)
  (bits 0))

(defun %brotli-read-bits (reader count)
  (unless (and (integerp count) (<= 0 count 32))
    (%brotli-error "A Brotli bit width is invalid." :invalid-bit-width count))
  (loop while (< (brotli-bit-reader-bits reader) count)
        do (let ((position (brotli-bit-reader-position reader))
                 (octets (brotli-bit-reader-octets reader)))
             (when (>= position (length octets))
               (%brotli-error "The Brotli stream ended prematurely."
                              :truncated-input position))
             (setf (brotli-bit-reader-buffer reader)
                   (logior (brotli-bit-reader-buffer reader)
                           (ash (aref octets position)
                                (brotli-bit-reader-bits reader)))
                   (brotli-bit-reader-position reader) (1+ position)
                   (brotli-bit-reader-bits reader)
                   (+ (brotli-bit-reader-bits reader) 8))))
  (let ((value (logand (brotli-bit-reader-buffer reader)
                       (if (zerop count) 0 (1- (ash 1 count))))))
    (setf (brotli-bit-reader-buffer reader)
          (ash (brotli-bit-reader-buffer reader) (- count))
          (brotli-bit-reader-bits reader)
          (- (brotli-bit-reader-bits reader) count))
    value))

(defun %brotli-align-byte (reader)
  (let ((bits (brotli-bit-reader-bits reader)))
    (unless (zerop (logand (brotli-bit-reader-buffer reader)
                           (if (zerop bits) 0 (1- (ash 1 bits)))))
      (%brotli-error "Non-zero padding occurs before a byte-aligned field."
                     :non-zero-padding))
    (setf (brotli-bit-reader-buffer reader) 0
          (brotli-bit-reader-bits reader) 0))
  reader)

(defun %brotli-check-final-padding (reader)
  (%brotli-align-byte reader)
  (unless (= (brotli-bit-reader-position reader)
             (length (brotli-bit-reader-octets reader)))
    (%brotli-error "Data follows the final Brotli meta-block."
                   :trailing-input (brotli-bit-reader-position reader))))

(defun %brotli-reverse-bits (value width)
  (loop with result = 0
        for index below width
        do (setf result (logior (ash result 1)
                                (ldb (byte 1 index) value)))
        finally (return result)))

(defstruct (brotli-huffman-table
             (:constructor %make-brotli-huffman-table))
  codes
  (max-length 0)
  single-symbol)

(defun %brotli-build-huffman-table (lengths &key (require-complete t))
  (let* ((max-length (loop for length across lengths maximize length))
         (counts (make-array (1+ max-length) :initial-element 0))
         (next-code (make-array (1+ max-length) :initial-element 0))
         (codes (make-hash-table :test #'equal))
         (code 0)
         (symbol-count 0)
         (single-symbol nil)
         (space 1))
    (loop for length across lengths
          for symbol from 0
          do (unless (and (integerp length) (<= 0 length 15))
               (%brotli-error "A Brotli Huffman code length is invalid."
                              :invalid-code-length))
             (unless (zerop length)
               (incf symbol-count)
               (setf single-symbol symbol)
               (incf (aref counts length))))
    (when (zerop symbol-count)
      (%brotli-error "A Brotli Huffman tree has no symbols." :empty-tree))
    (loop for bits from 1 to max-length
          do (setf space (- (* 2 space) (aref counts bits)))
             (when (minusp space)
               (%brotli-error "A Brotli Huffman tree is oversubscribed."
                              :oversubscribed-tree)))
    (when (and require-complete (> symbol-count 1) (not (zerop space)))
      (%brotli-error "A Brotli Huffman tree is incomplete." :incomplete-tree))
    (loop for bits from 1 to max-length
          do (setf code (ash (+ code (aref counts (1- bits))) 1)
                   (aref next-code bits) code))
    (loop for symbol below (length lengths)
          for length = (aref lengths symbol)
          unless (zerop length)
            do (let ((canonical (aref next-code length)))
                 (incf (aref next-code length))
                 (setf (gethash (cons (%brotli-reverse-bits canonical length)
                                      length)
                                codes)
                       symbol)))
    (%make-brotli-huffman-table :codes codes :max-length max-length
                                :single-symbol (and (= symbol-count 1)
                                                    single-symbol))))

(defun %brotli-decode-symbol (reader table)
  (unless (null (brotli-huffman-table-single-symbol table))
    (return-from %brotli-decode-symbol
      (brotli-huffman-table-single-symbol table)))
  (let ((code 0))
    (loop for length from 1 to (brotli-huffman-table-max-length table)
          do (setf code (logior code
                                (ash (%brotli-read-bits reader 1)
                                     (1- length))))
             (multiple-value-bind (symbol found-p)
                 (gethash (cons code length)
                          (brotli-huffman-table-codes table))
               (when found-p (return-from %brotli-decode-symbol symbol))))
    (%brotli-error "The Brotli stream contains an invalid Huffman code."
                   :invalid-huffman-code)))

(defun %brotli-read-code-length-prefix (reader)
  (case (%brotli-read-bits reader 2)
    (0 0)
    (1 4)
    (2 3)
    (3 (if (zerop (%brotli-read-bits reader 1))
           2
           (if (zerop (%brotli-read-bits reader 1)) 1 5)))))

(defun %brotli-read-complex-huffman-lengths (reader alphabet-size skip)
  (let* ((order #(1 2 3 4 0 5 17 6 16 7 8 9 10 11 12 13 14 15))
         (code-lengths (make-array 18 :initial-element 0)))
    (loop with space = 32
          for index from skip below 18
          while (plusp space)
          do (let* ((symbol (aref order index))
                    (length (%brotli-read-code-length-prefix reader)))
               (setf (aref code-lengths symbol) length)
               (when (plusp length)
                 (decf space (ash 32 (- length))))))
    (let* ((code-tree (%brotli-build-huffman-table code-lengths))
           (lengths (make-array alphabet-size :initial-element 0))
           (position 0)
           (previous-length 8)
           (repeat-kind nil)
           (repeat-count 0)
           (space 32768))
      (loop while (and (< position alphabet-size) (plusp space))
            for code = (%brotli-decode-symbol reader code-tree)
            do (cond
                 ((< code 16)
                  (setf (aref lengths position) code)
                  (incf position)
                  (when (plusp code)
                    (setf previous-length code)
                    (decf space (ash 32768 (- code)))
                    (when (minusp space)
                      (%brotli-error "A Brotli Huffman tree is oversubscribed."
                                     :oversubscribed-tree)))
                  (setf repeat-kind nil repeat-count 0))
                 ((or (= code 16) (= code 17))
                  (let* ((extra-count (if (= code 16) 2 3))
                         (extra (%brotli-read-bits reader extra-count))
                         (total (if (eql repeat-kind code)
                                    (+ (ash (- repeat-count 2) extra-count)
                                       extra 3)
                                    (+ extra 3)))
                         (run (- total (if (eql repeat-kind code)
                                           repeat-count 0)))
                         (length (if (= code 16) previous-length 0)))
                    (when (> (+ position run) alphabet-size)
                      (%brotli-error "A Brotli code-length repeat exceeds its alphabet."
                                     :code-length-repeat))
                    (loop repeat run
                          do (setf (aref lengths position) length)
                             (incf position))
                    (progn
                      (when (plusp length)
                        (decf space (* run (ash 32768 (- length))))
                        (when (minusp space)
                          (%brotli-error "A Brotli Huffman tree is oversubscribed."
                                         :oversubscribed-tree)))
                      (setf repeat-kind code repeat-count total))))
                 (t (%brotli-error "A Brotli code-length symbol is invalid."
                                   :invalid-code-length-symbol))))
      (let ((space 32768))
        (loop for length across lengths
              when (plusp length)
                do (decf space (ash 32768 (- length))))
        (unless (zerop space)
          (%brotli-error "A Brotli Huffman code has incomplete code space."
                         :incomplete-tree)))
      lengths)))

(defun %brotli-read-huffman-table (reader alphabet-size)
  (let ((kind (%brotli-read-bits reader 2)))
    (if (= kind 1)
        (let* ((count (1+ (%brotli-read-bits reader 2)))
               (width (integer-length (1- alphabet-size)))
               (symbols (make-array count))
               (lengths (make-array alphabet-size :initial-element 0)))
          (dotimes (index count)
            (let ((symbol (%brotli-read-bits reader width)))
              (when (or (>= symbol alphabet-size)
                        (find symbol symbols :end index))
                (%brotli-error "A Brotli simple Huffman alphabet is invalid."
                               :invalid-simple-alphabet))
              (setf (aref symbols index) symbol)))
          (let ((tree-select (if (= count 4)
                                 (%brotli-read-bits reader 1)
                                 0)))
            (loop for index below count
                  for length = (case count
                                 (1 0)
                                 (2 1)
                                 (3 (if (zerop index) 1 2))
                                 (4 (if (zerop tree-select)
                                        2
                                        (aref #(1 2 3 3) index))))
                  do (setf (aref lengths (aref symbols index)) length)))
          (if (= count 1)
              (%make-brotli-huffman-table
               :codes nil :max-length 0 :single-symbol (aref symbols 0))
              (%brotli-build-huffman-table lengths)))
        (let ((lengths (%brotli-read-complex-huffman-lengths
                        reader alphabet-size kind)))
          (%brotli-build-huffman-table lengths)))))

(defun %brotli-read-varlen-uint8 (reader)
  (if (zerop (%brotli-read-bits reader 1))
      0
      (let ((bits (%brotli-read-bits reader 3)))
        (if (zerop bits)
            1
            (+ (ash 1 bits) (%brotli-read-bits reader bits))))))

(defparameter +brotli-block-length-extra-bits+
  #(2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 7 8 9 10 11 12 13 24))

(defparameter +brotli-block-length-offsets+
  #(1 5 9 13 17 25 33 41 49 65 81 97 113 145 177 209 241 305 369 497
    753 1265 2289 4337 8433 16625))

(defun %brotli-read-block-length (reader table)
  (let* ((code (%brotli-decode-symbol reader table))
         (extra-count (aref +brotli-block-length-extra-bits+ code)))
    (+ (aref +brotli-block-length-offsets+ code)
       (%brotli-read-bits reader extra-count))))

(defstruct (brotli-block-state (:constructor %make-brotli-block-state))
  (type-count 1)
  type-tree
  count-tree
  (current-type 0)
  (previous-type 1)
  (remaining nil))

(defun %brotli-read-block-state (reader type-count)
  (if (= type-count 1)
      (%make-brotli-block-state :type-count 1)
      (let* ((state (%make-brotli-block-state
                     :type-count type-count
                     :type-tree (%brotli-read-huffman-table reader
                                                             (+ type-count 2))
                     :count-tree (%brotli-read-huffman-table reader 26))))
        (setf (brotli-block-state-remaining state)
              (%brotli-read-block-length
               reader (brotli-block-state-count-tree state)))
        state)))

(defun %brotli-switch-block-if-needed (reader state)
  (when (and (> (brotli-block-state-type-count state)
                1)
             (zerop (brotli-block-state-remaining state)))
    (let* ((type-code (%brotli-decode-symbol
                       reader (brotli-block-state-type-tree state)))
           (type-count (brotli-block-state-type-count state))
           (current (brotli-block-state-current-type state))
           (previous (brotli-block-state-previous-type state))
           (next-type
             (case type-code
               (0 previous)
               (1 (mod (1+ current) type-count))
               (otherwise (- type-code 2)))))
      (when (>= next-type type-count)
        (%brotli-error "A Brotli block type is outside its declared range."
                       :invalid-block-type))
      (setf (brotli-block-state-previous-type state) current
            (brotli-block-state-current-type state) next-type
            (brotli-block-state-remaining state)
            (%brotli-read-block-length
             reader (brotli-block-state-count-tree state)))))
  (brotli-block-state-current-type state))

(defun %brotli-decrement-block-count (state)
  (when (> (brotli-block-state-type-count state) 1)
    (decf (brotli-block-state-remaining state))))

(defun %brotli-read-context-map (reader context-count tree-count)
  (let ((map (make-array context-count :initial-element 0)))
    (when (> tree-count 1)
      (let* ((first (%brotli-read-bits reader 1))
             (rle-max (if (zerop first) 0
                        (1+ (%brotli-read-bits reader 4))))
             (tree (%brotli-read-huffman-table reader (+ tree-count rle-max)))
             (position 0))
        (loop while (< position context-count)
              for code = (%brotli-decode-symbol reader tree)
              do (cond
                  ((zerop code)
                   (setf (aref map position) 0)
                   (incf position))
                  ((> code rle-max)
                   (let ((value (- code rle-max)))
                     (when (>= value tree-count)
                       (%brotli-error "A Brotli context map value is invalid."
                                      :invalid-context-map))
                     (setf (aref map position) value)
                     (incf position)))
                  (t
                   (let ((run (+ (ash 1 code)
                                 (%brotli-read-bits reader code))))
                     (when (> (+ position run) context-count)
                       (%brotli-error "A Brotli context-map run exceeds its map."
                                      :context-map-repeat))
                     (loop repeat run
                           do (setf (aref map position) 0)
                           (incf position))))))
        (when (= 1 (%brotli-read-bits reader 1))
          (let ((mtf (make-array 256)))
            (dotimes (index 256) (setf (aref mtf index) index))
            (dotimes (index context-count)
              (let* ((rank (aref map index))
                     (value (aref mtf rank)))
                (setf (aref map index) value)
                (loop for cursor downfrom rank above 0
                      do (setf (aref mtf cursor) (aref mtf (1- cursor))))
                (setf (aref mtf 0) value)))))
        (unless (and (every (lambda (value) (< value tree-count)) map)
                     (loop with seen = (make-array tree-count
                                                   :initial-element nil)
                           for value across map
                           do (setf (aref seen value) t)
                           finally (return (every #'identity seen))))
          (%brotli-error "A Brotli context map omits or exceeds a tree index."
                         :invalid-context-map)))) map))

(defun %brotli-utf8-lut0 (byte)
  (cond
    ((< byte 32) (if (member byte '(9 10 13)) 4 0))
    ((= byte 32) 8)
    ((<= byte 47)
     (case byte
       ((34 39) 16) (37 20) (40 24) (41 28) (44 32) (46 36)
       (otherwise 12)))
    ((<= byte 57) 44)
    ((<= byte 64)
     (case byte
       ((58 59) 32) (60 24) (61 40) (62 28) (otherwise 12)))
    ((<= byte 90)
     (if (member byte '(65 69 73 79 85)) 48 52))
    ((<= byte 96)
     (case byte (91 24) (93 28) (otherwise 12)))
    ((<= byte 122)
     (if (member byte '(97 101 105 111 117)) 56 60))
    ((<= byte 126)
     (case byte (123 24) (125 28) (otherwise 12)))
    ((< byte 192) (logand byte 1))
    (t (+ 2 (logand byte 1)))))

(defun %brotli-utf8-lut1 (byte)
  (cond
    ((<= byte 32) 0)
    ((<= byte 47) 1)
    ((<= byte 57) 2)
    ((<= byte 64) 1)
    ((<= byte 90) 2)
    ((<= byte 96) 1)
    ((<= byte 122) 3)
    ((<= byte 126) 1)
    ((< byte 208) 0)
    (t 2)))

(defun %brotli-signed-lut (byte)
  (cond ((zerop byte) 0)
        ((< byte 16) 1)
        ((< byte 64) 2)
        ((< byte 128) 3)
        ((< byte 192) 4)
        ((< byte 240) 5)
        ((< byte 255) 6)
        (t 7)))

(defun %brotli-literal-context (mode previous previous-previous)
  (case mode
    (0 (logand previous #x3f))
    (1 (ash previous -2))
    (2 (logior (%brotli-utf8-lut0 previous)
               (%brotli-utf8-lut1 previous-previous)))
    (3 (logior (ash (%brotli-signed-lut previous) 3)
               (%brotli-signed-lut previous-previous)))
    (otherwise (%brotli-error "A Brotli literal context mode is invalid."
                              :invalid-context-mode))))

(defun %brotli-previous-bytes (output)
  (let ((length (fill-pointer output)))
    (values (if (zerop length) 0 (aref output (1- length)))
            (if (< length 2) 0 (aref output (- length 2))))))

(defun %brotli-emit-byte (output value limit)
  (when (>= (fill-pointer output) limit)
    (error 'brotli-size-limit-exceeded
           :message "The Brotli output exceeded its size limit."
           :reason :output-limit :limit limit
           :observed (1+ (fill-pointer output))))
  (vector-push-extend value output))

(defparameter +brotli-insert-length-extra-bits+
  #(0 0 0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 7 8 9 10 12 14 24))

(defparameter +brotli-insert-length-offsets+
  #(0 1 2 3 4 5 6 8 10 14 18 26 34 50 66 98 130 194 322 578 1090 2114
    6210 22594))

(defparameter +brotli-copy-length-extra-bits+
  #(0 0 0 0 0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 7 8 9 10 24))

(defparameter +brotli-copy-length-offsets+
  #(2 3 4 5 6 7 8 9 10 12 14 18 22 30 38 54 70 102 134 198 326 582 1094
    2118))

(defun %brotli-command-length-codes (command)
  (cond
    ((< command 128)
     (let ((offset (mod command 64)))
       (values (ash offset -3)
               (+ (* 8 (floor command 64)) (mod offset 8))
               t)))
    ((< command 256)
     (let* ((offset (mod (- command 128) 64))
            (copy-group (floor (- command 128) 64)))
       (values (ash offset -3) (+ (* 8 copy-group) (mod offset 8)) nil)))
    ((< command 384)
     (let* ((offset (mod (- command 256) 64))
            (copy-group (floor (- command 256) 64)))
       (values (+ 8 (ash offset -3))
               (+ (* 8 copy-group) (mod offset 8)) nil)))
    ((< command 448)
     (let ((offset (- command 384)))
       (values (ash offset -3) (+ 16 (mod offset 8)) nil)))
    ((< command 512)
     (let ((offset (- command 448)))
       (values (+ 16 (ash offset -3)) (mod offset 8) nil)))
    ((< command 576)
     (let ((offset (- command 512)))
       (values (+ 8 (ash offset -3)) (+ 16 (mod offset 8)) nil)))
    ((< command 640)
     (let ((offset (- command 576)))
       (values (+ 16 (ash offset -3)) (+ 8 (mod offset 8)) nil)))
    ((< command 704)
     (let ((offset (- command 640)))
       (values (+ 16 (ash offset -3)) (+ 16 (mod offset 8)) nil)))
    (t (%brotli-error "A Brotli insert-and-copy code is invalid."
                      :invalid-command-code))))

(defun %brotli-read-insert-copy-lengths (reader command)
  (multiple-value-bind (insert-code copy-code implicit-distance-p)
      (%brotli-command-length-codes command)
    (let* ((insert-extra-count
             (aref +brotli-insert-length-extra-bits+ insert-code))
           (insert-length
             (+ (aref +brotli-insert-length-offsets+ insert-code)
                (%brotli-read-bits reader insert-extra-count)))
           (copy-extra-count
             (aref +brotli-copy-length-extra-bits+ copy-code))
           (copy-length
             (+ (aref +brotli-copy-length-offsets+ copy-code)
                (%brotli-read-bits reader copy-extra-count))))
      (values insert-length copy-length copy-code implicit-distance-p))))

(defun %brotli-read-distance (reader code postfix-bits direct-codes)
  (cond
    ((< code 16)
     (values :short code))
    ((< code (+ 16 direct-codes))
     (values :distance (- code 15)))
    (t
     (let* ((index (- code direct-codes 16))
            (distance-bits (1+ (ash index (- (1+ postfix-bits)))))
            (high-code (ash index (- postfix-bits)))
            (postfix-mask (1- (ash 1 postfix-bits)))
            (low-code (logand index postfix-mask))
            (extra (%brotli-read-bits reader distance-bits))
            (offset (- (ash (+ 2 (logand high-code 1)) distance-bits) 4)))
       (values :distance
               (+ (ash (+ offset extra) postfix-bits)
                  low-code direct-codes 1))))))

(defparameter +brotli-short-distance-deltas+
  #(0 0 0 0 -1 1 -2 2 -3 3 -1 1 -2 2 -3 3))

(defun %brotli-resolve-short-distance (cache code)
  (let* ((cache-index (cond ((<= code 3) code)
                            ((<= code 9) 0)
                            (t 1)))
         (distance (+ (aref cache cache-index)
                      (aref +brotli-short-distance-deltas+ code))))
    (when (<= distance 0)
      (%brotli-error "A Brotli short distance resolves to zero or less."
                     :invalid-short-distance))
    distance))

(defun %brotli-update-distance-cache (cache distance)
  (loop for index downfrom 3 above 0
        do (setf (aref cache index) (aref cache (1- index))))
  (setf (aref cache 0) distance))

(defun %brotli-copy-reference (output distance length limit)
  (let ((start (- (fill-pointer output) distance)))
    (when (or (<= distance 0) (minusp start))
      (%brotli-error "A Brotli backward reference points before output."
                     :distance-before-output))
    (loop repeat length
          do (%brotli-emit-byte output
                                (aref output (- (fill-pointer output) distance))
                                limit))
    length))

(defun %brotli-copy-dictionary-word
    (output word-length distance max-distance remaining limit)
  (unless (<= 4 word-length 24)
    (%brotli-error "A Brotli dictionary reference has an invalid length."
                   :invalid-dictionary-length))
  (let* ((word-id (- distance max-distance 1))
         (bits (aref +brotli-dictionary-length-bits+ word-length))
         (word-index (logand word-id (1- (ash 1 bits))))
         (transform-index (ash word-id (- bits)))
         (word (%brotli-dictionary-word word-length word-index
                                        transform-index)))
    (unless word
      (%brotli-error "A Brotli static dictionary reference is invalid."
                     :invalid-dictionary-reference))
    (when (> (length word) remaining)
      (%brotli-error "A Brotli dictionary word exceeds its meta-block."
                     :dictionary-word-overrun))
    (loop for octet across word
          do (%brotli-emit-byte output octet limit))
    (length word)))

(defun %brotli-decode-compressed-metablock
    (reader output limit metablock-length window-size distance-cache)
  (let* ((literal-block-count (1+ (%brotli-read-varlen-uint8 reader)))
         (literal-blocks (%brotli-read-block-state reader literal-block-count))
         (command-block-count (1+ (%brotli-read-varlen-uint8 reader)))
         (command-blocks (%brotli-read-block-state reader command-block-count))
         (distance-block-count (1+ (%brotli-read-varlen-uint8 reader)))
         (distance-blocks (%brotli-read-block-state reader distance-block-count))
         (postfix-bits (%brotli-read-bits reader 2))
         (direct-codes (ash (%brotli-read-bits reader 4) postfix-bits))
         (literal-modes
           (let ((modes (make-array literal-block-count)))
             (dotimes (index literal-block-count)
               (setf (aref modes index) (%brotli-read-bits reader 2)))
             modes))
         (literal-tree-count (1+ (%brotli-read-varlen-uint8 reader)))
         (literal-context-map
           (%brotli-read-context-map reader (* 64 literal-block-count)
                                     literal-tree-count))
         (distance-tree-count (1+ (%brotli-read-varlen-uint8 reader)))
         (distance-context-map
           (%brotli-read-context-map reader (* 4 distance-block-count)
                                     distance-tree-count))
         (distance-alphabet (+ 16 direct-codes (ash 48 postfix-bits)))
         (literal-trees (make-array literal-tree-count))
         (command-trees (make-array command-block-count))
         (distance-trees (make-array distance-tree-count)))
    (dotimes (index literal-tree-count)
      (setf (aref literal-trees index)
            (%brotli-read-huffman-table reader 256)))
    (dotimes (index command-block-count)
      (setf (aref command-trees index)
            (%brotli-read-huffman-table reader 704)))
    (dotimes (index distance-tree-count)
      (setf (aref distance-trees index)
            (%brotli-read-huffman-table reader distance-alphabet)))
    (let ((remaining metablock-length))
      (loop while (plusp remaining)
            do (let* ((command-type (%brotli-switch-block-if-needed
                                     reader command-blocks))
                      (command (%brotli-decode-symbol
                                reader (aref command-trees command-type))))
                 (%brotli-decrement-block-count command-blocks)
                 (multiple-value-bind
                       (insert-length copy-length copy-code implicit-distance-p)
                     (%brotli-read-insert-copy-lengths reader command)
                   (when (> insert-length remaining)
                     (%brotli-error "Brotli literals exceed the meta-block length."
                                    :literal-overrun))
                   (loop repeat insert-length
                         do
                     (let* ((literal-type (%brotli-switch-block-if-needed
                                           reader literal-blocks))
                            (tree-index
                              (aref literal-context-map
                                    (+ (* 64 literal-type)
                                       (multiple-value-bind (previous previous-previous)
                                           (%brotli-previous-bytes output)
                                         (%brotli-literal-context
                                          (aref literal-modes literal-type)
                                          previous previous-previous)))))
                            (literal (%brotli-decode-symbol
                                      reader (aref literal-trees tree-index))))
                       (when (>= literal 256)
                         (%brotli-error "A Brotli literal symbol is invalid."
                                        :invalid-literal-symbol))
                       (%brotli-emit-byte output literal limit)
                       (%brotli-decrement-block-count literal-blocks)))
                   (decf remaining insert-length)
                   (when (plusp remaining)
                     (let ((distance-code 0)
                           (distance 0)
                           (distance-kind :short))
                       (unless implicit-distance-p
                         (let* ((distance-type
                                  (%brotli-switch-block-if-needed
                                   reader distance-blocks))
                                (context (min copy-code 3))
                                (tree-index
                                  (aref distance-context-map
                                        (+ (* 4 distance-type) context))))
                           (setf distance-code
                                 (%brotli-decode-symbol
                                  reader (aref distance-trees tree-index)))
                           (%brotli-decrement-block-count distance-blocks)))
                       (multiple-value-setq (distance-kind distance)
                         (%brotli-read-distance reader distance-code
                                                postfix-bits direct-codes))
                       (when (eq distance-kind :short)
                         (setf distance
                               (%brotli-resolve-short-distance
                                distance-cache distance-code)))
                       (let* ((maximum-distance
                                (min window-size (fill-pointer output)))
                              (produced-length
                                (if (> distance maximum-distance)
                                    (%brotli-copy-dictionary-word
                                     output copy-length distance maximum-distance
                                     remaining limit)
                                    (progn
                                      (when (> copy-length remaining)
                                        (%brotli-error "A Brotli copy exceeds the meta-block length."
                                                       :copy-overrun))
                                      (unless (zerop distance-code)
                                        (%brotli-update-distance-cache
                                         distance-cache distance))
                                      (%brotli-copy-reference
                                       output distance copy-length limit)))))
                         (decf remaining produced-length))))))))))

(defun %brotli-read-window-bits (reader)
  (if (zerop (%brotli-read-bits reader 1))
      16
      (let ((high (%brotli-read-bits reader 3)))
        (cond
          ((plusp high) (+ 17 high))
          (t (let ((low (%brotli-read-bits reader 3)))
               (case low
                 (0 17)
                 (1 (%brotli-error "The Brotli window-size code is reserved."
                                   :reserved-window-size))
                 (otherwise (+ 8 low)))))))))

(defun %brotli-read-meta-block (reader)
  (let ((last-p (= 1 (%brotli-read-bits reader 1))))
    (when (and last-p (= 1 (%brotli-read-bits reader 1)))
      (return-from %brotli-read-meta-block (values :last-empty t 0)))
    (let ((nibbles
           (case (%brotli-read-bits reader 2)
             (0 4) (1 5) (2 6) (3 0))))
      (if (zerop nibbles)
          (progn
            (when (= 1 (%brotli-read-bits reader 1))
              (%brotli-error "A Brotli metadata reserved bit is non-zero."
                             :reserved-metadata-bit))
            (let* ((skip-bytes (%brotli-read-bits reader 2))
                   (encoded-length (if (zerop skip-bytes)
                                       0
                                     (%brotli-read-bits reader
                                                        (* 8 skip-bytes))))
                   (metadata-length (if (zerop skip-bytes)
                                        0
                                      (1+ encoded-length))))
              (when (and (> skip-bytes 1)
                         (zerop (ldb (byte 8 (* 8 (1- skip-bytes)))
                                     encoded-length)))
                (%brotli-error "A Brotli metadata length is exuberantly encoded."
                               :exuberant-metadata-length))
              (%brotli-align-byte reader)
              (loop repeat metadata-length
                    do (%brotli-read-bits reader 8))
              (values :metadata last-p 0)))
        (let ((encoded-length 0))
          (dotimes (index nibbles)
            (let ((nibble (%brotli-read-bits reader 4)))
              (when (and (> nibbles 4)
                         (= index (1- nibbles))
                         (zerop nibble))
                (%brotli-error "A Brotli meta-block length is exuberantly encoded."
                               :exuberant-meta-block-length))
              (setf encoded-length
                    (logior encoded-length (ash nibble (* 4 index))))))
          (let ((length (1+ encoded-length)))
            (if last-p
                (values :compressed t length)
              (if (= 1 (%brotli-read-bits reader 1))
                  (values :uncompressed nil length)
                (values :compressed nil length)))))))))

(defun %brotli-decode-octets (octets max-output-bytes)
  (let* ((reader (%make-brotli-bit-reader octets))
         (window-bits (%brotli-read-window-bits reader))
         (window-size (- (ash 1 window-bits) 16))
         (output (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer 0))
         (distance-cache (make-array 4 :initial-contents #(4 11 15 16))))
    (loop do
          (multiple-value-bind (kind last-p length)
              (%brotli-read-meta-block reader)
            (case kind
              (:last-empty (loop-finish))
              (:metadata nil)
              (:uncompressed
               (%brotli-align-byte reader)
               (loop repeat length
                     do (%brotli-emit-byte output (%brotli-read-bits reader 8)
                                           max-output-bytes)))
              (:compressed
               (%brotli-decode-compressed-metablock
                reader output max-output-bytes length window-size distance-cache)))
            (when last-p (loop-finish))))
    (%brotli-check-final-padding reader)
    (values (subseq output 0 (fill-pointer output))
            (brotli-bit-reader-position reader))))

(defun brotli-decode
    (octets &key (max-output-bytes +brotli-default-max-output-bytes+)
                   max-output)
  "Decode one RFC 7932 Brotli stream into a fresh octet vector."
  (setf max-output-bytes (or max-output max-output-bytes))
  (unless (%brotli-octet-vector-p octets)
    (%brotli-error "BROTLI-DECODE requires a vector of octets."
                   :invalid-input-type (type-of octets)))
  (unless (and (integerp max-output-bytes) (>= max-output-bytes 0))
    (%brotli-error "MAX-OUTPUT-BYTES must be a non-negative integer."
                   :invalid-output-limit))
  (%brotli-decode-octets octets max-output-bytes))

(defun brotli-decompress (octets &rest options)
  (apply #'brotli-decode octets options))

(defstruct (brotli-stream (:constructor %make-brotli-stream))
  (chunks nil)
  (max-output-bytes +brotli-default-max-output-bytes+)
  (finished-p nil))

(defun make-brotli-stream
    (&key (max-output-bytes +brotli-default-max-output-bytes+) max-output)
  (setf max-output-bytes (or max-output max-output-bytes))
  (unless (and (integerp max-output-bytes) (>= max-output-bytes 0))
    (%brotli-error "MAX-OUTPUT-BYTES must be a non-negative integer."
                   :invalid-output-limit))
  (%make-brotli-stream :max-output-bytes max-output-bytes))

(defun brotli-stream-push (stream octets)
  (when (brotli-stream-finished-p stream)
    (%brotli-error "The Brotli stream has already been finished."
                   :stream-finished))
  (unless (%brotli-octet-vector-p octets)
    (%brotli-error "BROTLI-STREAM-PUSH requires a vector of octets."
                   :invalid-input-type (type-of octets)))
  (push octets (brotli-stream-chunks stream))
  nil)

(defun brotli-stream-finish (stream)
  (when (brotli-stream-finished-p stream)
    (%brotli-error "The Brotli stream has already been finished."
                   :stream-finished))
  (setf (brotli-stream-finished-p stream) t)
  (let ((chunks (nreverse (brotli-stream-chunks stream))))
    (setf (brotli-stream-chunks stream) chunks)
    (let* ((length (loop for chunk in chunks sum (length chunk)))
           (input (make-array length :element-type '(unsigned-byte 8)))
           (position 0))
      (dolist (chunk chunks)
        (replace input chunk :start1 position)
        (incf position (length chunk)))
      (setf (brotli-stream-chunks stream) nil
            chunks nil)
      (multiple-value-bind (output consumed)
          (brotli-decode input :max-output-bytes
                         (brotli-stream-max-output-bytes stream))
        (declare (ignore consumed))
        output))))
