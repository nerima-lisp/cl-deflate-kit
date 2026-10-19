(in-package #:deflate-kit)

(defconstant +zstd-magic+ #xfd2fb528)
(defconstant +zstd-max-block-size+ (* 128 1024))
(defconstant +zstd-default-max-window-size-bytes+ (* 8 1024 1024))
(defconstant +zstd-default-max-output-bytes+ (* 8 1024 1024))
(defconstant +zstd-u64-mask+ #xffffffffffffffff)

(defparameter +zstd-default-ll-distribution+
  #(4 3 2 2 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 2 2 2 2 2 2 3 2 1 1 1 1 1
    -1 -1 -1 -1))
(defparameter +zstd-default-ml-distribution+
  #(1 4 3 2 2 2 2 2
    2 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 -1 -1
    -1 -1 -1 -1 -1))
(defparameter +zstd-default-of-distribution+
  #(1 1 1 1 1 1 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 -1 -1 -1 -1 -1))

(defstruct (zstd-output (:constructor %make-zstd-output (limit)))
  (data (make-array 0 :element-type '(unsigned-byte 8)
                    :adjustable t :fill-pointer 0))
  limit)

(defstruct (zstd-forward-bits
            (:constructor %make-zstd-forward-bits
                (&key octets start position end)))
  octets start position end)
(defstruct zstd-reverse-bits octets start position)
(defstruct zstd-fse-entry symbol bits baseline)
(defstruct zstd-fse-table log entries)
(defstruct zstd-huffman-table codes max-bits)
(defstruct zstd-frame-state
  window-size
  block-max
  huffman-table
  ll-table
  of-table
  ml-table
  (offsets #(1 4 8)))
(defstruct (zstd-stream
            (:constructor %make-zstd-stream
                (&key chunks max-output-bytes max-window-size-bytes finished-p)))
  chunks max-output-bytes max-window-size-bytes finished-p)

(defun %zstd-invalid (reason &optional detail position)
  (error 'zstd-invalid-data :message "Invalid Zstandard data."
         :reason reason :detail detail :position position))

(defun %zstd-limit-exceeded (reason limit observed)
  (error 'zstd-size-limit-exceeded :message "Zstandard size limit exceeded."
         :reason reason :limit limit :observed observed))

(defun %zstd-validate-limit (value name)
  (declare (ignore name))
  (unless (and (integerp value) (not (minusp value)))
    (error 'type-error :datum value :expected-type '(integer 0 *)))
  value)

(defun %zstd-check-output (output count)
  (let ((observed (+ (fill-pointer (zstd-output-data output)) count)))
    (when (> observed (zstd-output-limit output))
      (%zstd-limit-exceeded :output-size-limit
                            (zstd-output-limit output) observed))
    observed))

(defun %zstd-reserve-output (output count)
  (let* ((data (zstd-output-data output))
         (start (fill-pointer data))
         (end (%zstd-check-output output count))
         (capacity (array-total-size data)))
    (when (> end capacity)
      (let ((new-capacity
              (min (zstd-output-limit output)
                   (max end (* 2 (max capacity 1))))))
        (setf data (adjust-array data new-capacity :fill-pointer start)
              (zstd-output-data output) data)))
    (setf (fill-pointer (zstd-output-data output)) end)
    start))

(defun %zstd-append-output (output octets start end)
  (let* ((count (- end start))
         (output-start (%zstd-reserve-output output count)))
    (replace (zstd-output-data output) octets
             :start1 output-start :start2 start :end2 end)
    output-start))

(defun %zstd-repeat-output (output byte count)
  (let ((start (%zstd-reserve-output output count)))
    (loop for index from start below (+ start count)
          do (setf (aref (zstd-output-data output) index) byte))
    start))

(defun %zstd-backref-output (output offset count)
  (let* ((data (zstd-output-data output))
         (start (%zstd-reserve-output output count)))
    (when (or (zerop offset) (> offset start))
      (%zstd-invalid :offset-out-of-range offset))
    (loop for index from start below (+ start count)
          do (setf (aref data index) (aref data (- index offset))))
    start))

(defun %zstd-output-length (output)
  (fill-pointer (zstd-output-data output)))

(defun %zstd-output-slice (output start)
  (subseq (zstd-output-data output) start (%zstd-output-length output)))

(defun %zstd-require-bytes (octets position count end)
  (declare (ignore octets))
  (when (or (< position 0) (< count 0) (> (+ position count) end))
    (%zstd-invalid :truncated-input
                   (list :position position :needed count :end end)
                   position))
  position)

(defun %zstd-read-byte (octets position end)
  (%zstd-require-bytes octets position 1 end)
  (values (aref octets position) (1+ position)))

(defun %zstd-read-uint-le (octets position count end)
  (%zstd-require-bytes octets position count end)
  (values (loop for index below count
                sum (ash (aref octets (+ position index)) (* 8 index)))
          (+ position count)))

(defun %zstd-forward-read-bits (reader count)
  (when (or (< count 0) (> count 16))
    (%zstd-invalid :invalid-fse-bit-width count))
  (let ((position (zstd-forward-bits-position reader)))
    (when (> (+ position count)
             (* 8 (- (zstd-forward-bits-end reader)
                     (zstd-forward-bits-start reader))))
      (%zstd-invalid :truncated-fse-header position))
    (let ((value 0)
          (octets (zstd-forward-bits-octets reader))
          (start (zstd-forward-bits-start reader)))
      (dotimes (bit count)
        (let* ((absolute (+ position bit))
               (byte (aref octets (+ start (floor absolute 8))))
               (set (ldb (byte 1 (mod absolute 8)) byte)))
          (setf value (logior value (ash set bit)))))
      (setf (zstd-forward-bits-position reader) (+ position count))
      value)))

(defun %zstd-reverse-reader (octets start end)
  (%zstd-require-bytes octets start (- end start) end)
  (when (or (<= end start) (zerop (aref octets (1- end))))
    (%zstd-invalid :invalid-bitstream :missing-end-marker end))
  (let* ((last-byte (aref octets (1- end)))
         (marker-bit (1- (integer-length last-byte))))
    (make-zstd-reverse-bits
     :octets octets :start start
     :position (+ (* 8 (- end start 1)) marker-bit))))

(defun %zstd-reverse-read-bits (reader count)
  (when (or (< count 0) (> count 32))
    (%zstd-invalid :invalid-bit-width count))
  (when (> count (zstd-reverse-bits-position reader))
    (%zstd-invalid :truncated-bitstream count))
  (let* ((value 0)
         (position (zstd-reverse-bits-position reader))
         (start (- position count))
         (octets (zstd-reverse-bits-octets reader))
         (base (zstd-reverse-bits-start reader)))
    (dotimes (bit-index count)
      (let* ((absolute (+ start bit-index))
             (byte (aref octets (+ base (floor absolute 8))))
             (bit (ldb (byte 1 (mod absolute 8)) byte)))
        (setf value (logior value (ash bit bit-index)))))
    (setf (zstd-reverse-bits-position reader) start)
    value))

(defun %zstd-highbit (value)
  (1- (integer-length value)))

(defun %zstd-read-normalized-counts (octets position end max-symbol max-log)
  (let* ((reader (%make-zstd-forward-bits
                  :octets octets :start position :position 0 :end end))
         (log (+ 5 (%zstd-forward-read-bits reader 4))))
    (when (or (< log 5) (> log max-log))
      (%zstd-invalid :invalid-fse-table (list :accuracy-log log :maximum max-log)
                     position))
    (let* ((table-size (ash 1 log))
           (remaining (1+ table-size))
           (threshold table-size)
           (bits (1+ log))
           (counts (make-array (1+ max-symbol) :initial-element 0))
           (symbol 0)
           (previous-zeroes-p nil))
      (loop while (and (> remaining 1) (<= symbol max-symbol))
            do (when previous-zeroes-p
                 (let ((zero-count 0)
                       (repeat 3))
                   (loop while (= repeat 3)
                         do (setf repeat (%zstd-forward-read-bits reader 2))
                            (incf zero-count repeat))
                   (when (> (+ symbol zero-count) (1+ max-symbol))
                     (%zstd-invalid :invalid-fse-table :too-many-zero-counts
                                    position))
                   (loop repeat zero-count
                         do (setf (aref counts symbol) 0)
                            (incf symbol))
                   (when (> symbol max-symbol)
                     (%zstd-invalid :invalid-fse-table :too-many-zero-counts
                                    position))
                   (setf previous-zeroes-p nil)))
               (when (< remaining threshold)
                 (setf bits (1+ (%zstd-highbit remaining))
                       threshold (ash 1 (1- bits))))
               (when (zerop bits)
                 (%zstd-invalid :invalid-fse-table :invalid-count-width position))
               (let* ((maximum (- (1- (* 2 threshold)) remaining))
                      (value (%zstd-forward-read-bits reader (1- bits)))
                      (count (if (< value maximum)
                                 value
                                 (- (+ value
                                       (ash (%zstd-forward-read-bits reader 1)
                                            (1- bits)))
                                    maximum)))
                      (normalized (1- count)))
                 (when (> (abs normalized) remaining)
                   (%zstd-invalid :invalid-fse-table
                                  (list :count normalized :remaining remaining)
                                  position))
                 (setf (aref counts symbol) normalized
                       previous-zeroes-p (zerop normalized))
                 (decf remaining (abs normalized))
                 (incf symbol)))
      (unless (= remaining 1)
        (%zstd-invalid :invalid-fse-table
                       (list :remaining remaining :symbols symbol) position))
      (when (or (< symbol 2) (> symbol (1+ max-symbol)))
        (%zstd-invalid :invalid-fse-table (list :symbol-count symbol) position))
      (when (zerop (aref counts (1- symbol)))
        (%zstd-invalid :invalid-fse-table :zero-final-symbol position))
      (values (subseq counts 0 symbol) log
              (+ position (ceiling (zstd-forward-bits-position reader) 8))))))

(defun %zstd-build-fse-table (counts log)
  (let* ((table-size (ash 1 log))
         (mask (1- table-size))
         (step (+ (ash table-size -1) (ash table-size -3) 3))
         (spread (make-array table-size :initial-element nil))
         (next (make-array (length counts) :initial-element 0))
         (high (1- table-size))
         (position 0)
         (allocated 0))
    (unless (and (<= 5 log 9) (<= (length counts) 256))
      (%zstd-invalid :invalid-fse-table (list :log log :symbols (length counts))))
    (loop for symbol below (length counts)
          for count = (aref counts symbol)
          do (when (< count -1)
               (%zstd-invalid :invalid-fse-table (list :symbol symbol :count count)))
             (incf allocated (if (= count -1) 1 count))
             (if (= count -1)
                 (progn
                   (setf (aref spread high) symbol
                         (aref next symbol) 1)
                   (decf high))
                 (setf (aref next symbol) count)))
    (unless (= allocated table-size)
      (%zstd-invalid :invalid-fse-table
                     (list :allocated allocated :expected table-size)))
    (when (< (count-if (lambda (count) (not (zerop count))) counts) 2)
      (%zstd-invalid :invalid-fse-table :fewer-than-two-symbols))
    (loop for symbol below (length counts)
          for count = (aref counts symbol)
          when (plusp count)
            do (loop repeat count
                     do (when (aref spread position)
                          (%zstd-invalid :invalid-fse-table :duplicate-state))
                        (setf (aref spread position) symbol)
                        (setf position (logand (+ position step) mask))
                        (loop while (> position high)
                              do (setf position (logand (+ position step) mask)))))
    (unless (loop for value across spread always value)
      (%zstd-invalid :invalid-fse-table :unassigned-state))
    (let ((entries (make-array table-size)))
      (dotimes (state table-size)
        (let* ((symbol (aref spread state))
               (next-state (aref next symbol))
               (number-of-bits (- log (%zstd-highbit next-state)))
               (baseline (- (ash next-state number-of-bits) table-size)))
          (unless (and (plusp next-state)
                       (<= 0 number-of-bits log)
                       (<= 0 baseline (1- table-size)))
            (%zstd-invalid :invalid-fse-table
                           (list :state state :symbol symbol :next next-state)))
          (setf (aref entries state)
                (make-zstd-fse-entry :symbol symbol :bits number-of-bits
                                     :baseline baseline))
          (incf (aref next symbol))))
      (make-zstd-fse-table :log log :entries entries))))

(defun %zstd-rle-fse-table (symbol)
  (make-zstd-fse-table :log 0
                       :entries (vector (make-zstd-fse-entry
                                         :symbol symbol :bits 0 :baseline 0))))

(defun %zstd-fse-table-from-distribution (distribution log)
  (%zstd-build-fse-table distribution log))

(defparameter +zstd-default-ll-table+
  (%zstd-fse-table-from-distribution +zstd-default-ll-distribution+ 6))
(defparameter +zstd-default-ml-table+
  (%zstd-fse-table-from-distribution +zstd-default-ml-distribution+ 6))
(defparameter +zstd-default-of-table+
  (%zstd-fse-table-from-distribution +zstd-default-of-distribution+ 5))

(defconstant +xxh64-prime1+ #x9e3779b185ebca87)
(defconstant +xxh64-prime2+ #xc2b2ae3d27d4eb4f)
(defconstant +xxh64-prime3+ #x165667b19e3779f9)
(defconstant +xxh64-prime4+ #x85ebca77c2b2ae63)
(defconstant +xxh64-prime5+ #x27d4eb2f165667c5)

(defun %zstd-u64 (value)
  (logand value +zstd-u64-mask+))

(defun %zstd-rotl64 (value count)
  (%zstd-u64 (logior (ash value count)
                     (ash value (- count 64)))))

(defun %zstd-xxh64-round (accumulator value)
  (%zstd-u64 (* (%zstd-rotl64
                 (%zstd-u64 (+ accumulator
                                (%zstd-u64 (* value +xxh64-prime2+))))
                 31)
                +xxh64-prime1+)))

(defun %zstd-xxh64-merge (accumulator value)
  (%zstd-u64 (+ (* (logxor accumulator (%zstd-xxh64-round 0 value))
                   +xxh64-prime1+)
                +xxh64-prime4+)))

(defun %zstd-xxh64 (octets start end)
  (let* ((length (- end start))
         (position start)
         (hash nil))
    (labels ((read64 (index)
               (%zstd-read-uint-le octets index 8 end))
             (read32 (index)
               (%zstd-read-uint-le octets index 4 end)))
      (if (>= length 32)
          (let ((v1 (%zstd-u64 (+ +xxh64-prime1+ +xxh64-prime2+)))
                (v2 +xxh64-prime2+)
                (v3 0)
                (v4 (%zstd-u64 (- +xxh64-prime1+))))
            (loop while (<= (+ position 32) end)
                  do (multiple-value-bind (word next) (read64 position)
                       (setf v1 (%zstd-xxh64-round v1 word)
                             position next))
                     (multiple-value-bind (word next) (read64 position)
                       (setf v2 (%zstd-xxh64-round v2 word)
                             position next))
                     (multiple-value-bind (word next) (read64 position)
                       (setf v3 (%zstd-xxh64-round v3 word)
                             position next))
                     (multiple-value-bind (word next) (read64 position)
                       (setf v4 (%zstd-xxh64-round v4 word)
                             position next)))
            (setf hash (%zstd-u64
                        (+ (%zstd-rotl64 v1 1) (%zstd-rotl64 v2 7)
                           (%zstd-rotl64 v3 12) (%zstd-rotl64 v4 18))))
            (setf hash (%zstd-xxh64-merge hash v1)
                  hash (%zstd-xxh64-merge hash v2)
                  hash (%zstd-xxh64-merge hash v3)
                  hash (%zstd-xxh64-merge hash v4)))
          (setf hash +xxh64-prime5+))
      (setf hash (%zstd-u64 (+ hash length)))
      (loop while (<= (+ position 8) end)
            do (multiple-value-bind (word next) (read64 position)
                 (setf hash (%zstd-u64
                             (+ (* (%zstd-rotl64
                                    (logxor hash
                                            (%zstd-xxh64-round 0 word))
                                    27)
                                   +xxh64-prime1+)
                                +xxh64-prime4+))
                       position next)))
      (when (<= (+ position 4) end)
        (multiple-value-bind (word next) (read32 position)
          (setf hash (%zstd-u64
                      (+ (* (%zstd-rotl64
                             (logxor hash
                                     (%zstd-u64 (* word +xxh64-prime1+)))
                             23)
                            +xxh64-prime2+)
                         +xxh64-prime3+))
                position next)))
      (loop while (< position end)
            for byte = (aref octets position)
            do (setf hash (%zstd-u64
                           (* (%zstd-rotl64
                               (logxor hash
                                       (%zstd-u64 (* byte +xxh64-prime5+)))
                               11)
                              +xxh64-prime1+)))
               (incf position))
      (setf hash (logxor hash (ash hash -33))
            hash (%zstd-u64 (* hash +xxh64-prime2+))
            hash (logxor hash (ash hash -29))
            hash (%zstd-u64 (* hash +xxh64-prime3+))
            hash (logxor hash (ash hash -32)))
      (%zstd-u64 hash))))

(defun %zstd-parse-frame-header (octets position end output max-window)
  (multiple-value-bind (descriptor next) (%zstd-read-byte octets position end)
    (when (logbitp 3 descriptor)
      (%zstd-invalid :reserved-frame-header-bit descriptor position))
    (let* ((content-size-flag (ldb (byte 2 6) descriptor))
           (single-segment-p (logbitp 5 descriptor))
           (checksum-p (logbitp 2 descriptor))
           (dictionary-size (aref #(0 1 2 4) (ldb (byte 2 0) descriptor)))
           (window-size nil)
           (content-size nil))
      (unless single-segment-p
        (multiple-value-bind (window-descriptor after-window)
            (%zstd-read-byte octets next end)
          (let* ((exponent (ash window-descriptor -3))
                 (mantissa (ldb (byte 3 0) window-descriptor))
                 (window-base (ash 1 (+ exponent 10))))
            (setf window-size (+ window-base
                                 (* (floor window-base 8) mantissa))
                  next after-window))))
      (multiple-value-bind (dictionary-id after-dictionary)
          (%zstd-read-uint-le octets next dictionary-size end)
        (setf next after-dictionary)
        (when (plusp dictionary-id)
          (error 'unsupported-container-error :format :zstd
                 :reason :dictionary-id)))
      (let ((content-size-size
              (cond ((zerop content-size-flag)
                     (if single-segment-p 1 0))
                    ((= content-size-flag 1) 2)
                    ((= content-size-flag 2) 4)
                    (t 8))))
        (when (plusp content-size-size)
          (multiple-value-bind (size after-size)
              (%zstd-read-uint-le octets next content-size-size end)
            (setf content-size (if (= content-size-size 2)
                                   (+ size 256)
                                   size)
                  next after-size)))
        (when single-segment-p
          (setf window-size content-size)))
      (when (> window-size max-window)
        (%zstd-limit-exceeded :window-size-limit max-window window-size))
      (when (and content-size
                 (> content-size
                    (- (zstd-output-limit output) (%zstd-output-length output))))
        (%zstd-limit-exceeded :output-size-limit
                              (zstd-output-limit output)
                              (+ (%zstd-output-length output) content-size)))
      (values next
              (make-zstd-frame-state
               :window-size window-size
               :block-max (min window-size +zstd-max-block-size+)
               :offsets #(1 4 8))
              content-size checksum-p))))

(defun %zstd-parse-fse-table (octets position end max-symbol max-log)
  (multiple-value-bind (counts log next)
      (%zstd-read-normalized-counts octets position end max-symbol max-log)
    (values (%zstd-build-fse-table counts log) next)))

(defun %zstd-fse-entry-at (table state)
  (when (or (< state 0) (>= state (length (zstd-fse-table-entries table))))
    (%zstd-invalid :invalid-fse-state state))
  (aref (zstd-fse-table-entries table) state))

(defun %zstd-fse-update-state (table state reader)
  (let* ((entry (%zstd-fse-entry-at table state))
         (next (+ (zstd-fse-entry-baseline entry)
                  (%zstd-reverse-read-bits reader (zstd-fse-entry-bits entry)))))
    (when (or (< next 0) (>= next (length (zstd-fse-table-entries table))))
      (%zstd-invalid :invalid-fse-state next))
    next))

(defun %zstd-huffman-table-from-weights (weights)
  (when (or (zerop (length weights)) (> (length weights) 255))
    (%zstd-invalid :invalid-huffman-table (length weights)))
  (let ((weight-sum 0))
    (loop for weight across weights
          do (when (> weight 11)
               (%zstd-invalid :invalid-huffman-table weight))
             (when (plusp weight)
               (incf weight-sum (ash 1 (1- weight)))))
    (when (zerop weight-sum)
      (%zstd-invalid :invalid-huffman-table :empty-tree))
    (let* ((max-bits (integer-length weight-sum))
           (target (ash 1 max-bits))
           (last-weight (- target weight-sum)))
      (when (or (> max-bits 11)
                (not (and (plusp last-weight)
                          (= (logand last-weight (1- last-weight)) 0))))
        (%zstd-invalid :invalid-huffman-table
                       (list :weight-sum weight-sum :last-weight last-weight)))
      (let* ((all-weights (make-array (1+ (length weights))))
             (lengths (make-array (1+ (length weights)) :initial-element 0))
             (counts (make-array (1+ max-bits) :initial-element 0))
             (codes (make-hash-table :test #'equal))
             (last-bits (integer-length last-weight))
             (code 0))
        (replace all-weights weights)
        (setf (aref all-weights (length weights)) last-bits)
        (loop for weight across all-weights
              for symbol from 0
              do (when (> weight max-bits)
                   (%zstd-invalid :invalid-huffman-table
                                  (list :symbol symbol :weight weight)))
                 (when (plusp weight)
                   (let ((number-of-bits (+ max-bits 1 (- weight))))
                     (when (or (<= number-of-bits 0) (> number-of-bits 11))
                       (%zstd-invalid :invalid-huffman-table number-of-bits))
                     (setf (aref lengths symbol) number-of-bits)
                     (incf (aref counts number-of-bits)))))
        (loop for number-of-bits from max-bits downto 1
              do (let ((next-code code))
                   (setf code (ash (+ code (aref counts number-of-bits)) -1))
                   (loop for symbol below (length lengths)
                         when (= (aref lengths symbol) number-of-bits)
                           do (setf (gethash (cons next-code number-of-bits) codes)
                                    symbol)
                              (incf next-code))))
        (unless (= code 1)
          (%zstd-invalid :invalid-huffman-table :incomplete-tree))
        (make-zstd-huffman-table :codes codes :max-bits max-bits)))))

(defun %zstd-huffman-symbol (reader table)
  (let ((code 0)
        (codes (zstd-huffman-table-codes table)))
    (loop for number-of-bits from 1 to (zstd-huffman-table-max-bits table)
          do (setf code (+ (ash code 1)
                           (%zstd-reverse-read-bits reader 1)))
             (multiple-value-bind (symbol found-p)
                 (gethash (cons code number-of-bits) codes)
               (when found-p (return-from %zstd-huffman-symbol symbol))))
    (%zstd-invalid :invalid-huffman-code)))

(defun %zstd-huffman-decode-stream (octets start end table output output-start count)
  (if (zerop count)
      (unless (= start end)
        (let ((reader (%zstd-reverse-reader octets start end)))
          (unless (zerop (zstd-reverse-bits-position reader))
            (%zstd-invalid :invalid-huffman-bitstream :unexpected-empty-stream))))
      (let ((reader (%zstd-reverse-reader octets start end)))
        (loop for index below count
              do (setf (aref output (+ output-start index))
                       (%zstd-huffman-symbol reader table)))
        (unless (zerop (zstd-reverse-bits-position reader))
          (%zstd-invalid :invalid-huffman-bitstream :unused-bits))))
  (+ output-start count))

(defun %zstd-decode-huffman-weights (octets start end table)
  (let* ((reader (%zstd-reverse-reader octets start end))
         (log (zstd-fse-table-log table))
         (state1 (%zstd-reverse-read-bits reader log))
         (state2 (%zstd-reverse-read-bits reader log))
         (weights (make-array 0 :adjustable t :fill-pointer 0)))
    (labels ((emit (state)
               (when (= (length weights) 255)
                 (%zstd-invalid :invalid-huffman-tree-description
                                :too-many-weights))
               (vector-push-extend
                (zstd-fse-entry-symbol (%zstd-fse-entry-at table state))
                weights))
             (advance (state)
               (let* ((entry (%zstd-fse-entry-at table state))
                      (bit-count (zstd-fse-entry-bits entry))
                      (available (zstd-reverse-bits-position reader))
                      (underflow (> bit-count available))
                      (bits (if underflow
                                (ash (%zstd-reverse-read-bits reader available)
                                     (- bit-count available))
                                (%zstd-reverse-read-bits reader bit-count)))
                      (next (+ (zstd-fse-entry-baseline entry) bits)))
                 (when (or (< next 0)
                           (>= next (length (zstd-fse-table-entries table))))
                   (%zstd-invalid :invalid-fse-state next))
                 (values next underflow)))
             (finish ()
               (values (%zstd-huffman-table-from-weights weights)
                       (length weights))))
      (loop
        do (emit state1)
           (multiple-value-bind (next underflow) (advance state1)
             (setf state1 next)
             (when underflow
               (emit state2)
               (return-from %zstd-decode-huffman-weights (finish))))
           (emit state2)
           (multiple-value-bind (next underflow) (advance state2)
             (setf state2 next)
             (when underflow
               (emit state1)
               (return-from %zstd-decode-huffman-weights (finish))))))))

(defun %zstd-parse-huffman-tree (octets position end)
  (multiple-value-bind (header next) (%zstd-read-byte octets position end)
    (if (>= header 128)
        (let* ((count (- header 127))
               (byte-count (ceiling count 2)))
          (%zstd-require-bytes octets next byte-count end)
          (let ((weights (make-array count)))
            (dotimes (index count)
              (let ((byte (aref octets (+ next (floor index 2)))))
                (setf (aref weights index)
                      (if (evenp index)
                          (ash byte -4)
                          (ldb (byte 4 0) byte)))))
            (values (%zstd-huffman-table-from-weights weights)
                    (+ next byte-count))))
        (let ((weight-size header))
          (when (zerop weight-size)
            (%zstd-invalid :invalid-huffman-tree-description :empty-weights))
          (%zstd-require-bytes octets next weight-size end)
          (multiple-value-bind (fse-table weight-start)
              (%zstd-parse-fse-table octets next (+ next weight-size) 11 6)
            (when (>= weight-start (+ next weight-size))
              (%zstd-invalid :invalid-huffman-tree-description :empty-fse-stream))
            (multiple-value-bind (huffman-table count)
                (%zstd-decode-huffman-weights
                 octets weight-start (+ next weight-size) fse-table)
              (declare (ignore count))
              (values huffman-table (+ next weight-size))))))))

(defun %zstd-check-block-growth (output block-start block-max count)
  (%zstd-check-output output count)
  (when (> (+ (- (%zstd-output-length output) block-start) count)
           block-max)
    (%zstd-invalid :block-output-size-limit
                   (list :maximum block-max :additional count))))

(defun %zstd-read-literals-header (octets position end block-type size-format)
  (if (<= block-type 1)
      (let* ((header-size (if (member size-format '(0 2)) 1
                              (if (= size-format 1) 2 3)))
             (header (nth-value 0 (%zstd-read-uint-le
                                   octets position header-size end)))
             (size-bits (if (member size-format '(0 2)) 5
                            (if (= size-format 1) 12 20)))
             (regenerated (ldb (byte size-bits (if (or (= size-format 0) (= size-format 2)) 3 4)) header)))
        (values regenerated nil 1 (+ position header-size)))
      (let* ((header-size (case size-format (0 3) (1 3) (2 4) (3 5)))
             (field-bits (case size-format (0 10) (1 10) (2 14) (3 18)))
             (header (nth-value 0 (%zstd-read-uint-le
                                   octets position header-size end)))
             (mask (1- (ash 1 field-bits)))
             (regenerated (logand (ash header -4) mask))
             (compressed (logand (ash header (+ field-bits 4)) mask))
             (streams (if (zerop size-format) 1 4)))
        (values regenerated compressed streams (+ position header-size)))))

(defun %zstd-decode-literals (octets position end output block-start frame-state)
  (multiple-value-bind (header next) (%zstd-read-byte octets position end)
    (let ((block-type (ldb (byte 2 0) header))
          (size-format (ldb (byte 2 2) header)))
      (multiple-value-bind (regenerated compressed-size stream-count content-start)
          (%zstd-read-literals-header octets (1- next) end block-type size-format)
        (when (> regenerated (zstd-frame-state-block-max frame-state))
          (%zstd-invalid :literal-size-limit
                         (list :size regenerated
                               :maximum (zstd-frame-state-block-max frame-state))))
        (%zstd-check-block-growth output block-start
                                  (zstd-frame-state-block-max frame-state)
                                  regenerated)
        (let ((literals (make-array regenerated :element-type '(unsigned-byte 8))))
          (if (<= block-type 1)
              (if (zerop block-type)
                  (progn
                    (%zstd-require-bytes octets content-start regenerated end)
                    (replace literals octets :start2 content-start
                             :end2 (+ content-start regenerated))
                    (values literals (+ content-start regenerated)))
                  (progn
                    (%zstd-require-bytes octets content-start 1 end)
                    (dotimes (index regenerated)
                      (setf (aref literals index) (aref octets content-start)))
                    (values literals (1+ content-start))))
              (let ((content-end (+ content-start compressed-size))
                    (huffman-table nil))
                (%zstd-require-bytes octets content-start compressed-size end)
                (if (= block-type 2)
                    (multiple-value-bind (table after-tree)
                        (%zstd-parse-huffman-tree octets content-start content-end)
                      (setf huffman-table table
                            content-start after-tree
                            (zstd-frame-state-huffman-table frame-state) table))
                    (setf huffman-table
                          (zstd-frame-state-huffman-table frame-state)))
                (unless huffman-table
                  (%zstd-invalid :treeless-literals-without-table))
                (if (= stream-count 1)
                    (progn
                      (%zstd-huffman-decode-stream
                       octets content-start content-end huffman-table literals 0
                       regenerated)
                      (values literals content-end))
                    (progn
                      (%zstd-require-bytes octets content-start 6 content-end)
                      (multiple-value-bind (first-size p1)
                          (%zstd-read-uint-le octets content-start 2 content-end)
                        (multiple-value-bind (second-size p2)
                            (%zstd-read-uint-le octets p1 2 content-end)
                          (multiple-value-bind (third-size p3)
                              (%zstd-read-uint-le octets p2 2 content-end)
                            (let* ((stream-start p3)
                                   (stream-bytes (- content-end stream-start))
                                   (first-three (+ first-size second-size third-size))
                                   (per-stream (ceiling regenerated 4))
                                   (last-stream (- regenerated (* 3 per-stream))))
                              (when (or (> first-three stream-bytes)
                                        (minusp last-stream))
                                (%zstd-invalid :invalid-huffman-jump-table
                                               (list :sizes first-size second-size
                                                     third-size stream-bytes)))
                              (let ((offset stream-start))
                                (%zstd-huffman-decode-stream
                                 octets offset (+ offset first-size) huffman-table
                                 literals 0 per-stream)
                                (incf offset first-size)
                                (%zstd-huffman-decode-stream
                                 octets offset (+ offset second-size) huffman-table
                                 literals per-stream per-stream)
                                (incf offset second-size)
                                (%zstd-huffman-decode-stream
                                 octets offset (+ offset third-size) huffman-table
                                 literals (* 2 per-stream) per-stream)
                                (incf offset third-size)
                                (%zstd-huffman-decode-stream
                                 octets offset content-end huffman-table literals
                                 (* 3 per-stream) last-stream))
                              (values literals content-end))))))))))))))

(defparameter +zstd-ll-baselines+
  #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
    16 18 20 22 24 28 32 40 48 64 128 256 512 1024 2048 4096
    8192 16384 32768 65536))
(defparameter +zstd-ll-extra-bits+
  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    1 1 1 1 2 2 3 3 4 6 7 8 9 10 11 12 13 14 15 16))
(defparameter +zstd-ml-baselines+
  #(3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
    21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 37 39 41
    43 47 51 59 67 83 99 131 259 515 1027 2051 4099 8195
    16387 32771 65539))
(defparameter +zstd-ml-extra-bits+
  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 2 2 3 3 4 4 5 7
    8 9 10 11 12 13 14 15 16))

(defun %zstd-sequence-table (octets position end mode default previous
                             maximum-symbol maximum-log)
  (case mode
    (0 (values default position))
    (1 (multiple-value-bind (symbol next)
           (%zstd-read-byte octets position end)
         (when (> symbol maximum-symbol)
           (%zstd-invalid :invalid-sequence-table
                          (list :symbol symbol :maximum maximum-symbol)
                          position))
         (values (%zstd-rle-fse-table symbol) next)))
    (2 (%zstd-parse-fse-table octets position end maximum-symbol maximum-log))
    (3 (unless previous
         (%zstd-invalid :repeat-mode-without-table position))
       (values previous position))
    (otherwise (%zstd-invalid :invalid-sequence-mode mode position))))

(defun %zstd-repeat-offset (frame-state offset-value literal-length)
  (let* ((offsets (zstd-frame-state-offsets frame-state))
         (offset nil)
         (history-index nil))
    (cond
      ((> offset-value 3)
       (setf offset (- offset-value 3)))
      ((zerop literal-length)
       (case offset-value
         (1 (setf history-index 1))
         (2 (setf history-index 2))
         (3 (setf offset (1- (aref offsets 0))))
         (otherwise (%zstd-invalid :invalid-repeat-offset offset-value))))
      (t (setf history-index (1- offset-value))))
    (when history-index
      (setf offset (aref offsets history-index))
      (loop for index downfrom history-index above 0
            do (setf (aref offsets index) (aref offsets (1- index))))
      (setf (aref offsets 0) offset))
    (when (and (> offset-value 3)
               (or (zerop literal-length) (plusp literal-length)))
      (setf (aref offsets 2) (aref offsets 1)
            (aref offsets 1) (aref offsets 0)
            (aref offsets 0) offset))
    (when (and (zerop literal-length) (= offset-value 3))
      (setf (aref offsets 2) (aref offsets 1)
            (aref offsets 1) (aref offsets 0)
            (aref offsets 0) offset))
    (when (or (null offset) (zerop offset))
      (%zstd-invalid :offset-out-of-range offset))
    offset))

(defun %zstd-emit-sequence (output frame-start block-start frame-state
                            literals literal-position literal-length offset-value
                            match-length)
  (when (> (+ literal-position literal-length) (length literals))
    (%zstd-invalid :sequence-literals-out-of-range
                   (list :position literal-position :length literal-length
                         :available (length literals))))
  (%zstd-check-block-growth output block-start
                            (zstd-frame-state-block-max frame-state)
                            literal-length)
  (%zstd-append-output output literals literal-position
                       (+ literal-position literal-length))
  (let* ((offset (%zstd-repeat-offset frame-state offset-value literal-length))
         (available (- (%zstd-output-length output) frame-start)))
    (when (or (>= offset (zstd-frame-state-window-size frame-state))
              (> offset available))
      (%zstd-invalid :offset-out-of-range
                     (list :offset offset :available available
                           :window (zstd-frame-state-window-size frame-state))))
    (%zstd-check-block-growth output block-start
                              (zstd-frame-state-block-max frame-state)
                              match-length)
    (%zstd-backref-output output offset match-length))
  (+ literal-position literal-length))

(defun %zstd-decode-sequences (octets position end output frame-start block-start
                               literals frame-state)
  (multiple-value-bind (first-byte next) (%zstd-read-byte octets position end)
    (let ((sequence-count
            (cond ((zerop first-byte) 0)
                  ((< first-byte 128) first-byte)
                  ((< first-byte 255)
                   (multiple-value-bind (second after-second)
                       (%zstd-read-byte octets next end)
                     (setf next after-second)
                     (+ (ash (- first-byte 128) 8) second)))
                  (t (multiple-value-bind (second after-second)
                         (%zstd-read-byte octets next end)
                       (multiple-value-bind (third after-third)
                           (%zstd-read-byte octets after-second end)
                         (setf next after-third)
                         (+ #x7f00 second (ash third 8)))))))
          (literal-position 0))
      (when (zerop sequence-count)
        (unless (= next end)
          (%zstd-invalid :unexpected-sequence-data next))
        (%zstd-check-block-growth output block-start
                                  (zstd-frame-state-block-max frame-state)
                                  (length literals))
        (%zstd-append-output output literals 0 (length literals))
        (return-from %zstd-decode-sequences end))
      (multiple-value-bind (modes after-modes)
          (%zstd-read-byte octets next end)
        (when (logtest modes 3)
          (%zstd-invalid :reserved-sequence-mode-bits modes next))
        (let* ((ll-mode (ldb (byte 2 6) modes))
               (of-mode (ldb (byte 2 4) modes))
               (ml-mode (ldb (byte 2 2) modes))
               (table-position after-modes)
               (ll-table nil)
               (of-table nil)
               (ml-table nil))
          (multiple-value-setq (ll-table table-position)
            (%zstd-sequence-table octets table-position end ll-mode
                                  +zstd-default-ll-table+
                                  (zstd-frame-state-ll-table frame-state) 35 9))
          (multiple-value-setq (of-table table-position)
            (%zstd-sequence-table octets table-position end of-mode
                                  +zstd-default-of-table+
                                  (zstd-frame-state-of-table frame-state) 31 8))
          (multiple-value-setq (ml-table table-position)
            (%zstd-sequence-table octets table-position end ml-mode
                                  +zstd-default-ml-table+
                                  (zstd-frame-state-ml-table frame-state) 52 9))
          (let ((reader (%zstd-reverse-reader octets table-position end))
                (ll-state nil)
                (of-state nil)
                (ml-state nil))
            (setf ll-state (%zstd-reverse-read-bits
                            reader (zstd-fse-table-log ll-table))
                  of-state (%zstd-reverse-read-bits
                            reader (zstd-fse-table-log of-table))
                  ml-state (%zstd-reverse-read-bits
                            reader (zstd-fse-table-log ml-table)))
            (setf (zstd-frame-state-ll-table frame-state) ll-table
                  (zstd-frame-state-of-table frame-state) of-table
                  (zstd-frame-state-ml-table frame-state) ml-table)
            (loop for index below sequence-count
                  do (let* ((ll-entry (%zstd-fse-entry-at ll-table ll-state))
                            (of-entry (%zstd-fse-entry-at of-table of-state))
                            (ml-entry (%zstd-fse-entry-at ml-table ml-state))
                            (ll-code (zstd-fse-entry-symbol ll-entry))
                            (of-code (zstd-fse-entry-symbol of-entry))
                            (ml-code (zstd-fse-entry-symbol ml-entry)))
                       (unless (and (<= 0 ll-code 35)
                                    (<= 0 of-code 31)
                                    (<= 0 ml-code 52))
                         (%zstd-invalid :invalid-sequence-code
                                        (list ll-code of-code ml-code)))
                       (let* ((offset-value
                                (+ (ash 1 of-code)
                                   (%zstd-reverse-read-bits reader of-code)))
                              (match-length
                                (+ (aref +zstd-ml-baselines+ ml-code)
                                   (%zstd-reverse-read-bits
                                    reader (aref +zstd-ml-extra-bits+ ml-code))))
                              (literal-length
                                (+ (aref +zstd-ll-baselines+ ll-code)
                                   (%zstd-reverse-read-bits
                                    reader (aref +zstd-ll-extra-bits+ ll-code)))))
                         (setf literal-position
                               (%zstd-emit-sequence
                                output frame-start block-start frame-state literals
                                literal-position literal-length offset-value
                                match-length)))
                       (unless (= index (1- sequence-count))
                         (setf ll-state (%zstd-fse-update-state
                                         ll-table ll-state reader)
                               ml-state (%zstd-fse-update-state
                                         ml-table ml-state reader)
                               of-state (%zstd-fse-update-state
                                         of-table of-state reader)))))
            (unless (zerop (zstd-reverse-bits-position reader))
              (%zstd-invalid :trailing-sequence-bits
                             (zstd-reverse-bits-position reader)))
            (when (< literal-position (length literals))
              (let ((remaining (- (length literals) literal-position)))
                (%zstd-check-block-growth output block-start
                                          (zstd-frame-state-block-max frame-state)
                                          remaining)
                (%zstd-append-output output literals literal-position
                                     (length literals))))
            end))))))
(defun %zstd-decode-compressed-block
    (octets start end output frame-start frame-state)
  (let ((block-start (%zstd-output-length output)))
    (multiple-value-bind (literals next)
        (%zstd-decode-literals octets start end output block-start frame-state)
      (%zstd-decode-sequences octets next end output frame-start block-start
                              literals frame-state))))
(defun %zstd-decode-block
    (octets position end output frame-start frame-state)
  (multiple-value-bind (header content-start)
      (%zstd-read-uint-le octets position 3 end)
    (let* ((last-p (logbitp 0 header))
           (type (ldb (byte 2 1) header))
           (size (ldb (byte 21 3) header))
           (block-max (zstd-frame-state-block-max frame-state))
           (content-end (+ content-start size)))
      (when (> size block-max)
        (%zstd-invalid :block-size-limit
                       (list :size size :maximum block-max) position))
      (let ((next
              (case type
                (0
                 (%zstd-require-bytes octets content-start size end)
                 (%zstd-check-block-growth output (%zstd-output-length output)
                                           block-max size)
                 (%zstd-append-output output octets content-start content-end)
                 content-end)
                (1
                 (%zstd-require-bytes octets content-start 1 end)
                 (%zstd-check-block-growth output (%zstd-output-length output)
                                           block-max size)
                 (%zstd-repeat-output output (aref octets content-start) size)
                 (1+ content-start))
                (2
                 (%zstd-require-bytes octets content-start size end)
                 (%zstd-decode-compressed-block
                  octets content-start content-end output frame-start frame-state))
                (otherwise (%zstd-invalid :reserved-block-type type position)))))
        (values next last-p)))))
(defun %zstd-decode-frame (octets position end output max-window)
  (let ((frame-start (%zstd-output-length output)))
    (multiple-value-bind (position frame-state content-size checksum-p)
        (%zstd-parse-frame-header octets position end output max-window)
      (let ((finished-p nil))
        (loop until finished-p
              do (multiple-value-bind (next last-p)
                     (%zstd-decode-block octets position end output frame-start
                                         frame-state)
                   (unless (> next position)
                     (%zstd-invalid :no-frame-progress position))
                   (setf position next
                         finished-p last-p)))
        (let ((actual-size (- (%zstd-output-length output) frame-start)))
          (when (and content-size (/= content-size actual-size))
            (%zstd-invalid :content-size-mismatch
                           (list :expected content-size :actual actual-size)
                           position))
          (when checksum-p
            (multiple-value-bind (expected after-checksum)
                (%zstd-read-uint-le octets position 4 end)
              (let ((actual
                      (logand (%zstd-xxh64 (zstd-output-data output)
                                           frame-start (%zstd-output-length output))
                              #xffffffff)))
                (unless (= expected actual)
                  (error 'checksum-error
                         :format :zstd :reason "content checksum mismatch"
                         :expected expected :actual actual)))
              (setf position after-checksum))))
        position))))
(defun %zstd-octet-vector-p (value)
  (and (vectorp value)
       (loop for octet across value
             always (typep octet '(unsigned-byte 8)))))
(defun zstd-decode
    (octets &key (max-output-bytes +zstd-default-max-output-bytes+)
                 (max-window-size-bytes +zstd-default-max-window-size-bytes+))
  (%zstd-validate-limit max-output-bytes :max-output-bytes)
  (%zstd-validate-limit max-window-size-bytes :max-window-size-bytes)
  (unless (%zstd-octet-vector-p octets)
    (%zstd-invalid :invalid-input-type (type-of octets)))
  (let* ((end (length octets))
         (position 0)
         (output (%make-zstd-output max-output-bytes)))
    (when (zerop end)
      (%zstd-invalid :empty-input))
    (loop while (< position end)
          do (multiple-value-bind (magic after-magic)
                 (%zstd-read-uint-le octets position 4 end)
               (let ((next
                       (cond
                         ((= magic +zstd-magic+)
                          (%zstd-decode-frame octets after-magic end output
                                              max-window-size-bytes))
                         ((<= #x184d2a50 magic #x184d2a5f)
                          (multiple-value-bind (size payload-start)
                              (%zstd-read-uint-le octets after-magic 4 end)
                            (%zstd-require-bytes octets payload-start size end)
                            (+ payload-start size)))
                         (t (%zstd-invalid :invalid-frame-magic magic position)))))
                 (unless (> next position)
                   (%zstd-invalid :no-frame-progress position))
                 (setf position next))))
    (subseq (zstd-output-data output) 0 (%zstd-output-length output))))
(defun make-zstd-stream
    (&key (max-output-bytes +zstd-default-max-output-bytes+)
          (max-window-size-bytes +zstd-default-max-window-size-bytes+))
  (%zstd-validate-limit max-output-bytes :max-output-bytes)
  (%zstd-validate-limit max-window-size-bytes :max-window-size-bytes)
  (%make-zstd-stream :chunks nil
                     :max-output-bytes max-output-bytes
                     :max-window-size-bytes max-window-size-bytes
                     :finished-p nil))
(defun zstd-stream-push (stream chunk)
  (unless (zstd-stream-p stream)
    (error 'type-error :datum stream :expected-type 'zstd-stream))
  (when (zstd-stream-finished-p stream)
    (%zstd-invalid :stream-finished))
  (unless (%zstd-octet-vector-p chunk)
    (%zstd-invalid :invalid-input-type (type-of chunk)))
  (push chunk (zstd-stream-chunks stream))
  nil)
(defun zstd-stream-finish (stream)
  (unless (zstd-stream-p stream)
    (error 'type-error :datum stream :expected-type 'zstd-stream))
  (when (zstd-stream-finished-p stream)
    (%zstd-invalid :stream-finished))
  (setf (zstd-stream-finished-p stream) t)
  (let* ((chunks (nreverse (zstd-stream-chunks stream)))
         (length (loop for chunk in chunks sum (length chunk)))
         (input (make-array length :element-type '(unsigned-byte 8)))
         (position 0))
    (dolist (chunk chunks)
      (replace input chunk :start1 position)
      (incf position (length chunk)))
    (zstd-decode input
                 :max-output-bytes (zstd-stream-max-output-bytes stream)
                 :max-window-size-bytes
                 (zstd-stream-max-window-size-bytes stream))))
(setf (fdefinition 'zstd-decompress) #'zstd-decode)
