(in-package #:deflate-kit)

(defconstant +deflate-default-max-output-bytes+ (* 16 1024 1024))

(defstruct (inflate-bit-reader (:constructor %make-inflate-bit-reader (octets start end)))
  octets
  (position start)
  (end end)
  (buffer 0)
  (bits 0))

(defun %inflate-error (message reason &optional detail)
  (error 'inflate-invalid-data
         :message message :reason reason :detail detail))

(defun %inflate-read-bits (reader count)
  (when (or (< count 0) (> count 16))
    (%inflate-error "Invalid DEFLATE bit width." :invalid-bit-width count))
  (loop while (< (inflate-bit-reader-bits reader) count)
        do (let ((position (inflate-bit-reader-position reader))
                 (octets (inflate-bit-reader-octets reader)))
             (when (>= position (inflate-bit-reader-end reader))
               (%inflate-error "The DEFLATE stream ended prematurely."
                               :truncated-input position))
             (setf (inflate-bit-reader-buffer reader)
                   (logior (inflate-bit-reader-buffer reader)
                           (ash (aref octets position)
                                (inflate-bit-reader-bits reader)))
                   (inflate-bit-reader-position reader) (1+ position)
                   (inflate-bit-reader-bits reader)
                   (+ (inflate-bit-reader-bits reader) 8))))
  (let ((mask (if (zerop count) 0 (1- (ash 1 count))))
        (value (logand (inflate-bit-reader-buffer reader)
                       (if (zerop count) 0 (1- (ash 1 count))))))
    (declare (ignore mask))
    (setf (inflate-bit-reader-buffer reader)
          (ash (inflate-bit-reader-buffer reader) (- count))
          (inflate-bit-reader-bits reader)
          (- (inflate-bit-reader-bits reader) count))
    value))

(defun %inflate-align-byte (reader)
  (setf (inflate-bit-reader-buffer reader) 0
        (inflate-bit-reader-bits reader) 0)
  reader)

(defun %inflate-reverse-bits (value width)
  (loop with result = 0
        for index below width
        do (setf result (logior (ash result 1)
                                (ldb (byte 1 index) value)))
        finally (return result)))

(defun %inflate-huffman-table (lengths)
  (let* ((max-length (loop for length across lengths maximize length))
         (counts (make-array (1+ max-length) :initial-element 0))
         (next-code (make-array (1+ max-length) :initial-element 0))
         (table (make-hash-table :test #'equal))
         (code 0))
    (loop for length across lengths
          do (when (or (< length 0) (> length 15))
               (%inflate-error "A DEFLATE Huffman length is invalid."
                               :invalid-code-length length))
             (unless (zerop length) (incf (aref counts length))))
    (loop for bits from 1 to max-length
          do (setf code (ash (+ code (aref counts (1- bits))) 1)
                   (aref next-code bits) code)
             (when (> (+ code (aref counts bits)) (ash 1 bits))
               (%inflate-error "A DEFLATE Huffman tree is oversubscribed."
                               :oversubscribed-tree bits)))
    (loop for symbol below (length lengths)
          for length = (aref lengths symbol)
          unless (zerop length)
            do (let ((code (aref next-code length)))
                 (incf (aref next-code length))
                 (setf (gethash (cons (%inflate-reverse-bits code length)
                                      length)
                                table)
                       symbol)))
    (values table max-length)))

(defun %inflate-decode-symbol (reader table max-length)
  (let ((code 0))
    (loop for length from 1 to max-length
          do (setf code (logior code
                                (ash (%inflate-read-bits reader 1)
                                     (1- length))))
             (multiple-value-bind (symbol found-p)
                 (gethash (cons code length) table)
               (when found-p (return-from %inflate-decode-symbol symbol))))
    (%inflate-error "The DEFLATE stream contains an invalid Huffman code."
                    :invalid-huffman-code)))

(defun %inflate-fixed-tables ()
  (let ((literal-lengths (make-array 288 :initial-element 0))
        (distance-lengths (make-array 32 :initial-element 5)))
    (loop for symbol from 0 to 143 do (setf (aref literal-lengths symbol) 8))
    (loop for symbol from 144 to 255 do (setf (aref literal-lengths symbol) 9))
    (loop for symbol from 256 to 279 do (setf (aref literal-lengths symbol) 7))
    (loop for symbol from 280 to 287 do (setf (aref literal-lengths symbol) 8))
    (multiple-value-bind (lt lm) (%inflate-huffman-table literal-lengths)
      (multiple-value-bind (dt dm) (%inflate-huffman-table distance-lengths)
        (values lt lm dt dm)))))

(defparameter +inflate-code-length-order+
  #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))
(defparameter +inflate-length-bases+
  #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115
    131 163 195 227 258))
(defparameter +inflate-length-extra+
  #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))
(defparameter +inflate-distance-bases+
  #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025
    1537 2049 3073 4097 6145 8193 12289 16385 24577))
(defparameter +inflate-distance-extra+
  #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))

(defun %inflate-dynamic-tables (reader)
  (let* ((literal-count (+ 257 (%inflate-read-bits reader 5)))
         (distance-count (+ 1 (%inflate-read-bits reader 5)))
         (code-count (+ 4 (%inflate-read-bits reader 4)))
         (code-lengths (make-array 19 :initial-element 0)))
    (loop for index below code-count
          do (setf (aref code-lengths (aref +inflate-code-length-order+ index))
                   (%inflate-read-bits reader 3)))
    (multiple-value-bind (code-table code-max)
        (%inflate-huffman-table code-lengths)
      (let ((lengths (make-array (+ literal-count distance-count)
                                 :initial-element 0))
            (position 0) (previous 0))
        (loop while (< position (length lengths))
              do (let ((symbol (%inflate-decode-symbol reader code-table code-max)))
                   (cond
                     ((<= symbol 15)
                      (setf (aref lengths position) symbol previous symbol)
                      (incf position))
                     ((= symbol 16)
                      (when (zerop position)
                        (%inflate-error "A repeat code has no previous length."
                                        :repeat-without-previous))
                      (let ((repeat (+ 3 (%inflate-read-bits reader 2))))
                        (when (> (+ position repeat) (length lengths))
                          (%inflate-error "A repeat code exceeds the Huffman table."
                                          :repeat-overflow repeat))
                        (loop repeat repeat
                              do (setf (aref lengths position) previous)
                                 (incf position))))
                     ((member symbol '(17 18))
                      (let ((repeat (if (= symbol 17)
                                        (+ 3 (%inflate-read-bits reader 3))
                                        (+ 11 (%inflate-read-bits reader 7)))))
                        (when (> (+ position repeat) (length lengths))
                          (%inflate-error "A zero repeat exceeds the Huffman table."
                                          :repeat-overflow repeat))
                        (loop repeat repeat
                              do (setf (aref lengths position) 0 previous 0)
                                 (incf position))))
                     (t
                      (%inflate-error "An invalid code-length symbol was decoded."
                                      :invalid-code-length-symbol symbol)))))
        (let ((literal-lengths (subseq lengths 0 literal-count))
              (distance-lengths (subseq lengths literal-count)))
          (when (zerop (aref literal-lengths 256))
            (%inflate-error "The literal/length table has no end-of-block symbol."
                            :missing-end-of-block))
          (when (every #'zerop distance-lengths)
            (%inflate-error "The distance table has no codes."
                            :missing-distance-code))
          (multiple-value-bind (lt lm) (%inflate-huffman-table literal-lengths)
            (multiple-value-bind (dt dm) (%inflate-huffman-table distance-lengths)
              (values lt lm dt dm))))))))

(defun %inflate-push (output value max-output-bytes truncate-at)
  (when (>= (fill-pointer output) max-output-bytes)
    (error 'inflate-size-limit-exceeded
           :message "The DEFLATE output exceeded its size limit."
           :reason :output-limit :limit max-output-bytes
           :observed (1+ (fill-pointer output))))
  (vector-push-extend value output)
  (when (and truncate-at (>= (fill-pointer output) truncate-at))
    (throw 'inflate-truncated (copy-seq output))))

(defun %inflate-huffman-block
    (reader output literal-table literal-max distance-table distance-max limit
     truncate-at window-size)
  (loop for symbol = (%inflate-decode-symbol reader literal-table literal-max)
        do (cond
             ((< symbol 256) (%inflate-push output symbol limit truncate-at))
             ((= symbol 256) (return output))
             ((<= 257 symbol 285)
              (let* ((index (- symbol 257))
                     (length (+ (aref +inflate-length-bases+ index)
                                (%inflate-read-bits
                                 reader (aref +inflate-length-extra+ index))))
                     (distance-symbol
                       (%inflate-decode-symbol reader distance-table distance-max)))
                (when (>= distance-symbol 30)
                  (%inflate-error "The DEFLATE distance symbol is reserved."
                                  :reserved-distance-symbol distance-symbol))
                (let* ((distance (+ (aref +inflate-distance-bases+ distance-symbol)
                                    (%inflate-read-bits
                                     reader (aref +inflate-distance-extra+
                                                  distance-symbol))))
                       (start (- (fill-pointer output) distance)))
                  (when (or (zerop distance) (> distance window-size) (minusp start))
                    (%inflate-error "The DEFLATE distance points before output."
                                    :distance-before-output distance))
                  (when (> (+ (fill-pointer output) length) limit)
                    (error 'inflate-size-limit-exceeded
                           :message "The DEFLATE output exceeded its size limit."
                           :reason :output-limit :limit limit
                           :observed (+ (fill-pointer output) length)))
                  ;; Read from the current distance behind the write cursor so
                  ;; overlapping back-references repeat the copied pattern.
                  (loop repeat length
                        do (%inflate-push output
                                          (aref output (- (fill-pointer output) distance))
                                          limit truncate-at)))))
             (t (%inflate-error "The DEFLATE literal/length symbol is invalid."
                               :invalid-literal-length-symbol symbol)))))

(defun %inflate-octet-vector-p (value)
  (and (vectorp value)
       (every (lambda (octet)
                (and (integerp octet) (<= 0 octet 255)))
              value)))

(defun inflate (octets &key (start 0) end max-output allow-trailing
                        (max-output-bytes +deflate-default-max-output-bytes+)
                        truncate-at size-hint (window-size 32768))
  "Decode OCTETS as a raw RFC 1951 DEFLATE stream.

Returns a fresh octet vector. MAX-OUTPUT-BYTES is enforced while output is
produced, including bytes produced by overlapping back-references. When
TRUNCATE-AT is reached, decoding stops and the second return value is NIL.
SIZE-HINT reserves output capacity up front. The second return value otherwise
is the number of input octets consumed from START."
  (setf max-output-bytes (or max-output max-output-bytes
                              +deflate-default-max-output-bytes+))
  (unless (%inflate-octet-vector-p octets)
    (%inflate-error "INFLATE requires a vector of octets." :invalid-input-type
                    octets))
  (setf end (or end (length octets)))
  (unless (and (integerp start) (integerp end)
               (<= 0 start end (length octets)))
    (%inflate-error "The INFLATE input range is invalid." :invalid-input-range
                    (list start end (length octets))))
  (unless (and (integerp max-output-bytes) (>= max-output-bytes 0))
    (%inflate-error "MAX-OUTPUT-BYTES must be a non-negative integer."
                    :invalid-output-limit max-output-bytes))
  (unless (or (null truncate-at)
              (and (integerp truncate-at) (>= truncate-at 0)))
    (%inflate-error "TRUNCATE-AT must be a non-negative integer."
                    :invalid-truncate-at truncate-at))
  (unless (or (null size-hint)
              (and (integerp size-hint) (>= size-hint 0)))
    (%inflate-error "SIZE-HINT must be a non-negative integer."
                    :invalid-size-hint size-hint))
  (unless (and (integerp window-size) (<= 256 window-size 32768))
    (%inflate-error "WINDOW-SIZE must be between 256 and 32768."
                    :invalid-window-size window-size))
  (when (zerop (or truncate-at 1))
    (return-from inflate (values (make-array 0 :element-type '(unsigned-byte 8)) nil)))
  (when (= start end)
    (%inflate-error "The DEFLATE stream is empty." :truncated-input start))
  (let ((reader (%make-inflate-bit-reader octets start end))
        (output (make-array (min (or size-hint 0) max-output-bytes)
                            :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0))
        (final-p nil))
    (let ((truncated-output
            (catch 'inflate-truncated
              (loop until final-p
                    do (setf final-p (= 1 (%inflate-read-bits reader 1)))
                       (case (%inflate-read-bits reader 2)
                         (0
                          (%inflate-align-byte reader)
                          (let ((length (%inflate-read-bits reader 16))
                                (inverse (%inflate-read-bits reader 16)))
                            (unless (= (logxor length inverse) #xffff)
                              (%inflate-error "The stored block length check failed."
                                              :stored-length-mismatch length))
                            (dotimes (index length)
                              (%inflate-push output (%inflate-read-bits reader 8)
                                             max-output-bytes truncate-at))))
                         (1
                          (multiple-value-bind (lt lm dt dm) (%inflate-fixed-tables)
                            (%inflate-huffman-block reader output lt lm dt dm
                                                    max-output-bytes truncate-at
                                                    window-size)))
                         (2
                          (multiple-value-bind (lt lm dt dm)
                              (%inflate-dynamic-tables reader)
                            (%inflate-huffman-block reader output lt lm dt dm
                                                    max-output-bytes truncate-at
                                                    window-size)))
                         (t (%inflate-error "The DEFLATE block type is reserved."
                                            :reserved-block-type))))
              nil)))
      (when truncated-output
        (return-from inflate (values truncated-output nil))))
    (when (plusp (inflate-bit-reader-bits reader))
      (unless (zerop (logand (inflate-bit-reader-buffer reader)
                             (1- (ash 1 (inflate-bit-reader-bits reader)))))
        (%inflate-error "Non-zero padding follows the final DEFLATE block."
                        :non-zero-padding)))
    ;; The reader keeps fewer than one byte after every read.  POSITION is
    ;; therefore the first byte not needed by the final block, including its
    ;; padding; any later byte is trailing input.
    (let ((consumed-position (inflate-bit-reader-position reader)))
      (when (and (not allow-trailing) (< consumed-position end))
      (%inflate-error "Data follows the final DEFLATE block."
                      :trailing-input consumed-position))
      (values (subseq output 0 (fill-pointer output))
              (- consumed-position start)))))

(defstruct (inflate-stream (:constructor %make-inflate-stream))
  (chunks nil)
  (max-output-bytes +deflate-default-max-output-bytes+)
  (finished-p nil))

(defun make-inflate-stream
    (&key (max-output-bytes +deflate-default-max-output-bytes+) max-output)
  (when max-output (setf max-output-bytes max-output))
  (unless (and (integerp max-output-bytes) (>= max-output-bytes 0))
    (%inflate-error "MAX-OUTPUT-BYTES must be a non-negative integer."
                    :invalid-output-limit max-output-bytes))
  (%make-inflate-stream :max-output-bytes max-output-bytes))

(defun inflate-stream-push (stream octets)
  "Append one arbitrary input chunk to STREAM.

Chunks are retained until FINISH, so boundaries may occur in the middle of a
byte-aligned field, Huffman code, or back-reference."
  (when (inflate-stream-finished-p stream)
    (%inflate-error "The INFLATE stream has already been finished."
                    :stream-finished))
  (unless (%inflate-octet-vector-p octets)
    (%inflate-error "INFLATE requires a vector of octets." :invalid-input-type
                    octets))
  (push octets (inflate-stream-chunks stream))
  nil)

(defun inflate-stream-finish (stream)
  (when (inflate-stream-finished-p stream)
    (%inflate-error "The INFLATE stream has already been finished."
                    :stream-finished))
  (setf (inflate-stream-finished-p stream) t)
  (let* ((chunks (nreverse (inflate-stream-chunks stream)))
         (length (loop for chunk in chunks sum (length chunk)))
         (input (make-array length :element-type '(unsigned-byte 8)))
         (position 0))
    (dolist (chunk chunks)
      (replace input chunk :start1 position)
      (incf position (length chunk)))
    (inflate input :max-output-bytes
             (inflate-stream-max-output-bytes stream))))
