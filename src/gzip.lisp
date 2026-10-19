(in-package #:deflate-kit)

(defstruct (gzip-member-header
            (:constructor %make-gzip-member-header))
  flags mtime xfl os fextra fname fcomment fhcrc)
(defun gzip-member-header-extra-flags (header) (gzip-member-header-xfl header))
(defun gzip-member-header-operating-system (header) (gzip-member-header-os header))

(defun %put-u16-le (octets index value)
  (setf (aref octets index) (ldb (byte 8 0) value)
        (aref octets (1+ index)) (ldb (byte 8 8) value))
  octets)

(defun %put-u32-le (octets index value)
  (setf (aref octets index) (ldb (byte 8 0) value)
        (aref octets (+ index 1)) (ldb (byte 8 8) value)
        (aref octets (+ index 2)) (ldb (byte 8 16) value)
        (aref octets (+ index 3)) (ldb (byte 8 24) value))
  octets)

(defun %u16-le (octets index)
  (logior (aref octets index) (ash (aref octets (1+ index)) 8)))

(defun %u32-le (octets index)
  (logior (aref octets index)
          (ash (aref octets (+ index 1)) 8)
          (ash (aref octets (+ index 2)) 16)
          (ash (aref octets (+ index 3)) 24)))

(defun %gzip-error (reason)
  (error 'invalid-container-error :format :gzip :reason reason))

(defun gzip-encode (data &key (level 6) (mtime 0) (filename nil) (comment nil))
  "Wrap raw DEFLATE DATA in one RFC 1952 gzip member."
  (let* ((input (%octets data))
         (compressed (%deflate-raw input :level level))
         (name (and filename (%octets filename)))
         (text (and comment (%octets comment)))
         (flags (logior (if name 8 0) (if text 16 0)))
         (header-length (+ 10 (if name (1+ (length name)) 0)
                            (if text (1+ (length text)) 0)))
         (result (make-array (+ header-length (length compressed) 8)
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (dolist (field (list name text))
      (when (and field (position 0 field))
        (%gzip-error "header fields must not contain NUL")))
    (setf (aref result 0) #x1f
          (aref result 1) #x8b
          (aref result 2) 8
          (aref result 3) flags)
    (%put-u32-le result 4 mtime)
    (setf (aref result 8) 0
          (aref result 9) 255)
    (let ((index 10))
      (when name
        (replace result name :start1 index)
        (incf index (length name))
        (incf index))
      (when text
        (replace result text :start1 index)
        (incf index (length text))
        (incf index))
      (replace result compressed :start1 index)
      (incf index (length compressed))
      (%put-u32-le result index (crc32 input))
      (%put-u32-le result (+ index 4) (length input)))
    result))

(defun %skip-zero-terminated (input index end)
  (loop while (< index end)
        do (if (zerop (aref input index))
               (return (1+ index))
               (incf index))
        finally (%gzip-error "unterminated gzip header field")))
(defun %gzip-header-at (input start end)
  (when (or (not (integerp start)) (not (integerp end))
            (< start 0) (< end start) (> end (length input)))
    (%gzip-error "invalid gzip header range"))
  (when (> (+ start 10) end)
    (%gzip-error "truncated member header"))
  (unless (and (= (aref input start) #x1f)
               (= (aref input (1+ start)) #x8b)
               (= (aref input (+ start 2)) 8))
    (%gzip-error "invalid member signature or compression method"))
  (let* ((flags (aref input (+ start 3)))
         (index (+ start 10))
         (fextra nil)
         (fname nil)
         (fcomment nil)
         (fhcrc nil))
    (when (logtest flags #xe0)
      (%gzip-error "reserved header flags are set"))
    (when (logtest flags 4)
      (when (> (+ index 2) end)
        (%gzip-error "truncated extra field length"))
      (let* ((extra-length (%u16-le input index))
             (extra-start (+ index 2))
             (extra-end (+ extra-start extra-length)))
        (when (> extra-end end)
          (%gzip-error "truncated extra field"))
        (setf fextra (subseq input extra-start extra-end)
              index extra-end)))
    (when (logtest flags 8)
      (let ((field-end (%skip-zero-terminated input index end)))
        (setf fname (subseq input index (1- field-end))
              index field-end)))
    (when (logtest flags 16)
      (let ((field-end (%skip-zero-terminated input index end)))
        (setf fcomment (subseq input index (1- field-end))
              index field-end)))
    (when (logtest flags 2)
      (when (> (+ index 2) end)
        (%gzip-error "truncated header CRC"))
      (let ((expected (%u16-le input index))
            (actual (logand (crc32 input :start start :end index) #xffff)))
        (unless (= expected actual)
          (error 'checksum-error :format :gzip :reason "header CRC mismatch"
                 :expected expected :actual actual))
        (setf fhcrc expected))
      (incf index 2))
    (values (%make-gzip-member-header
             :flags flags
             :mtime (%u32-le input (+ start 4))
             :xfl (aref input (+ start 8))
             :os (aref input (+ start 9))
             :fextra fextra
             :fname fname
             :fcomment fcomment
             :fhcrc fhcrc)
            index)))
(defun gzip-member-header-at (data &key (start 0) end)
  "Return the gzip member header and the absolute start of its DEFLATE data."
  (let* ((input (%octets data))
         (end (or end (length input))))
    (%gzip-header-at input start end)))
(defun gzip-decode-prefix (data &key (start 0) end max-output)
  "Decode one gzip member up to MAX-OUTPUT bytes.
Return the output, its header, and its absolute end position. The position is
NIL when decoding reaches the output bound."
  (unless (and (integerp max-output) (>= max-output 0))
    (error 'type-error :datum max-output
           :expected-type '(integer 0 *)))
  (let* ((input (%octets data))
         (end (or end (length input))))
    (multiple-value-bind (header data-start)
        (%gzip-header-at input start end)
      (multiple-value-bind (output consumed)
          (%inflate-raw input :start data-start :end end
                        :max-output-bytes max-output
                        :truncate-at max-output)
        (unless consumed
          (return-from gzip-decode-prefix (values output header nil)))
        (let ((trailer (+ data-start consumed)))
          (when (> (+ trailer 8) end)
            (%gzip-error "truncated member trailer"))
          (let ((expected-crc (%u32-le input trailer))
                (actual-crc (crc32 output))
                (expected-size (%u32-le input (+ trailer 4))))
            (unless (= expected-crc actual-crc)
              (error 'checksum-error :format :gzip :reason "member CRC mismatch"
                     :expected expected-crc :actual actual-crc))
            (unless (= expected-size (logand (length output) #xffffffff))
              (%gzip-error "member input size mismatch")))
          (values output header (+ trailer 8)))))))

(defun %gzip-member (input start max-output)
  (let ((length (length input)))
    (multiple-value-bind (header index)
        (%gzip-header-at input start length)
      (when (> (+ index 8) length)
        (%gzip-error "truncated member trailer"))
      (multiple-value-bind (output consumed)
          (%inflate-raw input :start index :end (- length 8)
                        :max-output-bytes max-output
                        :unbounded (null max-output))
        (let ((trailer (+ index consumed)))
          (when (> (+ trailer 8) length)
            (%gzip-error "raw stream exceeds member"))
          (let ((expected-crc (%u32-le input trailer))
                (actual-crc (crc32 output))
                (expected-size (%u32-le input (+ trailer 4))))
            (unless (= expected-crc actual-crc)
              (error 'checksum-error :format :gzip :reason "member CRC mismatch"
                     :expected expected-crc :actual actual-crc))
            (unless (= expected-size (logand (length output) #xffffffff))
              (%gzip-error "member input size mismatch")))
          (values output (+ trailer 8) header))))))

(defun gzip-decode (data &key max-output max-output-bytes unbounded
                              (allow-trailing-padding nil))
  "Decode all concatenated RFC 1952 members in DATA.
Return the decoded octets and a list of their headers in input order.
UNBOUNDED explicitly disables the default output limit. When
ALLOW-TRAILING-PADDING is true, accept an all-zero suffix only after a member."
  (when (and unbounded (or max-output max-output-bytes))
    (error 'deflate-error
           :message "UNBOUNDED cannot be combined with an output limit."))
  (when (and max-output max-output-bytes)
    (error 'deflate-error :message "specify only one output limit"))
  (setf max-output (if unbounded nil (or max-output max-output-bytes)))
  (%validate-max-output max-output)
  (let* ((input (%octets data))
         (output-limit (if unbounded nil
                           (or max-output +deflate-default-max-output-bytes+)))
         (result (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t))
         (headers nil)
         (index 0))
    (when (zerop (length input)) (%gzip-error "empty stream"))
    (loop while (< index (length input))
          do (when (and headers
                        allow-trailing-padding
                        (loop for padding-index from index below (length input)
                              always (zerop (aref input padding-index))))
               (setf index (length input))
               (return))
             (multiple-value-bind (member next header)
                 (%gzip-member input index
                               (and output-limit
                                    (- output-limit (length result))))
               (let ((old-length (length result)))
                 (setf result (adjust-array result (+ old-length (length member))))
                 (replace result member :start1 old-length))
               (push header headers)
               (setf index next)))
    (values result (nreverse headers))))

(setf (fdefinition 'encode-gzip) #'gzip-encode
      (fdefinition 'decode-gzip) #'gzip-decode
      (fdefinition 'gzip-compress) #'gzip-encode
      (fdefinition 'gzip-decompress) #'gzip-decode)
