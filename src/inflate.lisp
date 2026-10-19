(in-package #:deflate-kit)
(declaim (inline %inflate-read-bits %inflate-decode-symbol %inflate-push))

(defconstant +deflate-default-max-output-bytes+ (* 16 1024 1024))

(defstruct (inflate-bit-reader (:constructor %make-inflate-bit-reader (octets start end)))
  (octets (make-array 0 :element-type (quote (unsigned-byte 8))) :type (simple-array (unsigned-byte 8) (*)))
  (position start :type fixnum)
  (end end :type fixnum)
  (buffer 0 :type fixnum)
  (bits 0 :type fixnum))

(defun %inflate-error (message reason &optional detail)
  (error 'inflate-invalid-data
         :message message :reason reason :detail detail))

(defun %inflate-read-bits (reader count)
  (declare (type inflate-bit-reader reader)
           (type fixnum count)
           (optimize (speed 3) (safety 1)))
  (when (or (< count 0) (> count 16))
    (%inflate-error "Invalid DEFLATE bit width." :invalid-bit-width count))
  (loop while (< (inflate-bit-reader-bits reader) count)
        do (let ((position (inflate-bit-reader-position reader))
                 (octets (inflate-bit-reader-octets reader)))
             (declare (type fixnum position)
                      (type (simple-array (unsigned-byte 8) (*)) octets))
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
  (let ((value (logand (inflate-bit-reader-buffer reader)
                       (if (zerop count) 0 (1- (ash 1 count))))))
    (declare (type fixnum value))
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

(defstruct (inflate-huffman-table
            (:constructor %make-inflate-huffman-table (counts symbols)))
  (counts #() :type (simple-array fixnum (*)))
  (symbols #() :type (simple-array fixnum (*))))

(defun %inflate-huffman-table (lengths)
  (declare (type (simple-array fixnum (*)) lengths)
           (optimize (speed 3) (safety 1)))
  (let* ((max-length (loop for length fixnum across lengths
                           maximize length))
         (counts (make-array (1+ max-length) :element-type 'fixnum
                             :initial-element 0)))
    (declare (type fixnum max-length)
             (type (simple-array fixnum (*)) counts))
    (loop for length fixnum across lengths
          do (when (or (< length 0) (> length 15))
               (%inflate-error "A DEFLATE Huffman length is invalid."
                               :invalid-code-length length))
             (unless (zerop length)
               (incf (aref counts length))))
    (let ((left 1))
      (declare (type fixnum left))
      (loop for bits fixnum from 1 to max-length
            do (setf left (- (* left 2) (aref counts bits)))
               (when (minusp left)
                 (%inflate-error "A DEFLATE Huffman tree is oversubscribed."
                                 :oversubscribed-tree bits))))
    (let ((offsets (make-array (1+ max-length) :element-type 'fixnum
                               :initial-element 0))
          (symbols (make-array (count-if (lambda (length) (plusp length)) lengths)
                               :element-type 'fixnum)))
      (declare (type (simple-array fixnum (*)) offsets symbols))
      (loop for bits fixnum from 1 below max-length
            do (setf (aref offsets (1+ bits))
                     (+ (aref offsets bits) (aref counts bits))))
      (loop for symbol fixnum below (length lengths)
            for bits fixnum = (aref lengths symbol)
            unless (zerop bits)
              do (setf (aref symbols (aref offsets bits)) symbol)
                 (incf (aref offsets bits)))
      (values (%make-inflate-huffman-table counts symbols) max-length))))

(defun %inflate-decode-symbol (reader table max-length)
  (declare (type inflate-bit-reader reader)
           (type inflate-huffman-table table)
           (type fixnum max-length)
           (ftype (function (inflate-bit-reader fixnum) fixnum)
                  %inflate-read-bits)
           (inline %inflate-read-bits)
           (optimize (speed 3) (safety 1)))
  (let ((counts (inflate-huffman-table-counts table))
        (symbols (inflate-huffman-table-symbols table))
        (code 0)
        (first 0)
        (index 0))
    (declare (type fixnum code first index))
    (loop for length fixnum from 1 to max-length
          do (setf code (logior code (%inflate-read-bits reader 1)))
             (let ((count (aref counts length)))
               (when (< (- code count) first)
                 (return-from %inflate-decode-symbol
                   (aref symbols (+ index (- code first)))))
               (incf index count)
               (setf first (ash (+ first count) 1)
                     code (ash code 1))))
    (%inflate-error "The DEFLATE stream contains an invalid Huffman code."
                    :invalid-huffman-code)))

(defun %inflate-fixed-tables ()
  (let ((literal-lengths (make-array 288 :element-type 'fixnum
                                     :initial-element 0))
        (distance-lengths (make-array 32 :element-type 'fixnum
                                      :initial-element 5)))
    (loop for symbol fixnum from 0 to 143
          do (setf (aref literal-lengths symbol) 8))
    (loop for symbol fixnum from 144 to 255
          do (setf (aref literal-lengths symbol) 9))
    (loop for symbol fixnum from 256 to 279
          do (setf (aref literal-lengths symbol) 7))
    (loop for symbol fixnum from 280 to 287
          do (setf (aref literal-lengths symbol) 8))
    (multiple-value-bind (lt lm) (%inflate-huffman-table literal-lengths)
      (multiple-value-bind (dt dm) (%inflate-huffman-table distance-lengths)
        (values lt lm dt dm)))))

(defparameter +inflate-length-bases+
  (make-array 29 :element-type 'fixnum
              :initial-contents '(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115
                                    131 163 195 227 258)))
(defparameter +inflate-code-length-order+
  (make-array 19 :element-type 'fixnum
              :initial-contents '(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15)))
(defparameter +inflate-length-extra+
  (make-array 29 :element-type 'fixnum
              :initial-contents '(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0)))
(defparameter +inflate-distance-bases+
  (make-array 30 :element-type 'fixnum
              :initial-contents '(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025
                                    1537 2049 3073 4097 6145 8193 12289 16385 24577)))
(defparameter +inflate-distance-extra+
  (make-array 30 :element-type 'fixnum
              :initial-contents '(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13)))
(defun %inflate-dynamic-tables (reader)
  (declare (type inflate-bit-reader reader)
           (ftype (function (inflate-bit-reader fixnum) fixnum)
                  %inflate-read-bits)
           (ftype (function ((vector fixnum))
                            (values inflate-huffman-table fixnum))
                  %inflate-huffman-table)
           (ftype (function (inflate-bit-reader inflate-huffman-table fixnum)
                            fixnum)
                  %inflate-decode-symbol)
           (inline %inflate-read-bits %inflate-decode-symbol)
           (optimize (speed 3) (safety 1)))
  (let* ((literal-count (+ 257 (%inflate-read-bits reader 5)))
         (distance-count (+ 1 (%inflate-read-bits reader 5)))
         (code-count (+ 4 (%inflate-read-bits reader 4)))
         (code-lengths (make-array 19 :element-type 'fixnum
                                   :initial-element 0)))
    (declare (type fixnum literal-count distance-count code-count))
    (when (> literal-count 286)
      (%inflate-error "The DEFLATE literal/length count is reserved."
                      :reserved-literal-length-count literal-count))
    (when (> distance-count 30)
      (%inflate-error "The DEFLATE distance count is reserved."
                      :reserved-distance-count distance-count))
    (loop for index fixnum below code-count
          do (setf (aref code-lengths (aref +inflate-code-length-order+ index))
                   (%inflate-read-bits reader 3)))
    (multiple-value-bind (code-table code-max)
        (%inflate-huffman-table code-lengths)
      (declare (type inflate-huffman-table code-table)
               (type fixnum code-max))
      (let ((lengths (make-array (+ literal-count distance-count)
                                 :element-type 'fixnum
                                 :initial-element 0))
            (position 0)
            (previous 0))
        (declare (type (simple-array fixnum (*)) lengths)
                 (type fixnum position previous))
        (loop while (< position (length lengths))
              do (let ((symbol (%inflate-decode-symbol reader code-table code-max)))
                   (declare (type fixnum symbol))
                   (cond
                     ((<= symbol 15)
                      (setf (aref lengths position) symbol previous symbol)
                      (incf position))
                     ((= symbol 16)
                      (when (zerop position)
                        (%inflate-error "A repeat code has no previous length."
                                        :repeat-without-previous))
                      (let ((repeat (+ 3 (%inflate-read-bits reader 2))))
                        (declare (type fixnum repeat))
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
                        (declare (type fixnum repeat))
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
          (declare (type (simple-array fixnum (*)) literal-lengths distance-lengths))
          (when (zerop (aref literal-lengths 256))
            (%inflate-error "The literal/length table has no end-of-block symbol."
                            :missing-end-of-block))
          (when (every 'zerop distance-lengths)
            (%inflate-error "The distance table has no codes."
                            :missing-distance-code))
          (multiple-value-bind (lt lm) (%inflate-huffman-table literal-lengths)
            (multiple-value-bind (dt dm) (%inflate-huffman-table distance-lengths)
              (values lt lm dt dm))))))))

(defun %inflate-push (output value max-output-bytes truncate-at)
  (declare (type (vector (unsigned-byte 8)) output)
           (type (unsigned-byte 8) value)
           (type (or null integer) max-output-bytes truncate-at)
           (optimize (speed 3) (safety 1)))
  (when (and max-output-bytes
             (>= (fill-pointer output) max-output-bytes))
    (error 'inflate-size-limit-exceeded
           :message "The DEFLATE output exceeded its size limit."
           :reason :output-limit :limit max-output-bytes
           :observed (1+ (fill-pointer output))))
  (let ((position (fill-pointer output)))
    (declare (type fixnum position))
    (if (= position (length output))
        (vector-push-extend value output)
        (setf (aref output position) value
              (fill-pointer output) (1+ position)))
    (when (and truncate-at (>= (fill-pointer output) truncate-at))
      (throw 'inflate-truncated (copy-seq output)))))

(defun %inflate-huffman-block
    (reader output literal-table literal-max distance-table distance-max limit
     truncate-at window-size)
  (declare (type inflate-bit-reader reader)
           (type (vector (unsigned-byte 8)) output)
           (type inflate-huffman-table literal-table distance-table)
           (type fixnum literal-max distance-max window-size)
           (type (or null integer) limit truncate-at)
           (type (simple-array fixnum (29)) +inflate-length-bases+
                 +inflate-length-extra+)
           (type (simple-array fixnum (30)) +inflate-distance-bases+
                 +inflate-distance-extra+)
           (ftype (function (inflate-bit-reader fixnum) fixnum)
                  %inflate-read-bits)
           (ftype (function (inflate-bit-reader inflate-huffman-table fixnum)
                            fixnum)
                  %inflate-decode-symbol)
           (inline %inflate-read-bits %inflate-decode-symbol)
           (optimize (speed 3) (safety 1)))
  (let ((produced (fill-pointer output)))
    (declare (type fixnum produced))
    (labels ((emit (value)
               (declare (type (unsigned-byte 8) value))
               (when (and limit (>= produced limit))
                 (error 'inflate-size-limit-exceeded
                        :message "The DEFLATE output exceeded its size limit."
                        :reason :output-limit :limit limit
                        :observed (1+ produced)))
               (when (= produced (array-total-size output))
                 (let ((capacity (array-total-size output)))
                   (setf output
                         (adjust-array output
                                       (min (* 2 capacity)
                                            (or limit most-positive-fixnum))
                                       :fill-pointer produced))))
               (setf (aref output produced) value)
               (incf produced)
               (when (and truncate-at (>= produced truncate-at))
                 (setf (fill-pointer output) produced)
                 (throw 'inflate-truncated (copy-seq output)))))
      (declare (inline emit))
      (loop for symbol fixnum =
              (%inflate-decode-symbol reader literal-table literal-max)
            do (cond
                 ((< symbol 256)
                  (emit symbol))
                 ((= symbol 256)
                  (return))
                 ((<= 257 symbol 285)
                  (let* ((index (- symbol 257))
                         (length (+ (aref +inflate-length-bases+ index)
                                    (%inflate-read-bits
                                     reader (aref +inflate-length-extra+ index))))
                         (distance-symbol
                           (%inflate-decode-symbol reader distance-table distance-max)))
                    (declare (type fixnum index length distance-symbol))
                    (when (>= distance-symbol 30)
                      (%inflate-error "The DEFLATE distance symbol is reserved."
                                      :reserved-distance-symbol distance-symbol))
                    (let* ((distance (+ (aref +inflate-distance-bases+ distance-symbol)
                                        (%inflate-read-bits
                                         reader (aref +inflate-distance-extra+
                                                      distance-symbol))))
                           (start (- produced distance)))
                      (declare (type fixnum distance start))
                      (when (or (zerop distance) (> distance window-size)
                                (minusp start))
                        (%inflate-error "The DEFLATE distance points before output."
                                        :distance-before-output distance))
                      (when (and limit (> (+ produced length) limit)
                                 (or (null truncate-at) (> truncate-at limit)))
                        (error 'inflate-size-limit-exceeded
                               :message "The DEFLATE output exceeded its size limit."
                               :reason :output-limit :limit limit
                               :observed (+ produced length)))
                      (loop repeat length
                            do (emit (aref output (- produced distance)))))))
                 (t
                  (%inflate-error "The DEFLATE literal/length symbol is invalid."
                                  :invalid-literal-length-symbol symbol))))
    (setf (fill-pointer output) produced)
    output)))

(defun %inflate-octet-vector-p (value)
  (or (typep value (quote (vector (unsigned-byte 8))))
      (and (vectorp value)
           (every (lambda (octet)
                    (and (integerp octet) (<= 0 octet 255)))
                  value))))

(defun inflate (octets &key (start 0) end max-output allow-trailing
                        max-output-bytes unbounded truncate-at size-hint
                        (window-size 32768))
  "Decode OCTETS as a raw RFC 1951 DEFLATE stream.

Returns a fresh octet vector. MAX-OUTPUT-BYTES is enforced while output is
produced, including bytes produced by overlapping back-references. UNBOUNDED
explicitly disables that limit. When TRUNCATE-AT is reached, decoding stops
and the second return value is NIL. SIZE-HINT reserves output capacity up
front. The second return value otherwise is the number of input octets
consumed from START."
  (when (and unbounded (or max-output max-output-bytes))
    (error 'deflate-error
           :message "UNBOUNDED cannot be combined with an output limit."))
  (setf max-output-bytes
        (if unbounded nil
            (or max-output max-output-bytes
                +deflate-default-max-output-bytes+)))
  (progn
    (unless (%inflate-octet-vector-p octets)
      (%inflate-error "INFLATE requires a vector of octets."
                      :invalid-input-type octets))
    (unless (typep octets
                   (quote (simple-array (unsigned-byte 8) (*))))
      (setf octets
            (make-array (length octets)
                        :element-type (quote (unsigned-byte 8))
                        :initial-contents octets))))
  (setf end (or end (length octets)))
  (unless (and (integerp start) (integerp end)
               (<= 0 start end (length octets)))
    (%inflate-error "The INFLATE input range is invalid." :invalid-input-range
                    (list start end (length octets))))
  (unless (or (null max-output-bytes)
              (and (integerp max-output-bytes) (>= max-output-bytes 0)))
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
    (return-from inflate
      (values (make-array 0 :element-type '(unsigned-byte 8)) nil)))
  (when (= start end)
    (%inflate-error "The DEFLATE stream is empty." :truncated-input start))
  (let* ((initial-capacity
           (min (or max-output-bytes 1048576)
                (max 64 (or size-hint
                            (min (* 4 (- end start)) 1048576)))))
         (reader (%make-inflate-bit-reader octets start end))
         (output (make-array initial-capacity
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
    ;; POSITION is the first input byte not needed by the final block.
    (let ((consumed-position (inflate-bit-reader-position reader)))
      (when (and (not allow-trailing) (< consumed-position end))
        (%inflate-error "Data follows the final DEFLATE block."
                        :trailing-input consumed-position))
      (values (if (= (fill-pointer output) (length output))
                  output
                  (subseq output 0 (fill-pointer output)))
              (- consumed-position start)))))

(defstruct (inflate-stream (:constructor %make-inflate-stream))
  (chunks nil)
  (max-output-bytes +deflate-default-max-output-bytes+)
  (finished-p nil))

(defun make-inflate-stream
    (&key (max-output-bytes +deflate-default-max-output-bytes+
                            max-output-bytes-p)
          (max-output nil max-output-p)
          unbounded)
  (when (and unbounded (or max-output-bytes-p max-output-p))
    (error 'deflate-error
           :message "UNBOUNDED cannot be combined with an output limit."))
  (when max-output-p (setf max-output-bytes max-output))
  (when unbounded (setf max-output-bytes nil))
  (unless (or (and unbounded (null max-output-bytes))
              (and (integerp max-output-bytes) (>= max-output-bytes 0)))
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
    (if (null (inflate-stream-max-output-bytes stream))
        (inflate input :unbounded t)
        (inflate input :max-output-bytes
                 (inflate-stream-max-output-bytes stream)))))
