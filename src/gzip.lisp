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

(defun %gzip-member (input start max-output)
  (let ((length (length input)))
    (when (> (+ start 10) length)
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
        (when (> (+ index 2) length) (%gzip-error "truncated extra field length"))
        (let* ((extra-length (%u16-le input index))
               (extra-start (+ index 2))
               (extra-end (+ extra-start extra-length)))
          (when (> extra-end length) (%gzip-error "truncated extra field"))
          (setf fextra (subseq input extra-start extra-end)
                index extra-end)))
      (when (logtest flags 8)
        (let ((field-end (%skip-zero-terminated input index length)))
          (setf fname (subseq input index (1- field-end))
                index field-end)))
      (when (logtest flags 16)
        (let ((field-end (%skip-zero-terminated input index length)))
          (setf fcomment (subseq input index (1- field-end))
                index field-end)))
      (when (logtest flags 2)
        (when (> (+ index 2) length) (%gzip-error "truncated header CRC"))
        (let ((expected (%u16-le input index))
              (actual (logand (crc32 (subseq input start index)) #xffff)))
          (unless (= expected actual)
            (error 'checksum-error :format :gzip :reason "header CRC mismatch"
                   :expected expected :actual actual))
          (setf fhcrc expected))
        (incf index 2))
      (when (> (+ index 8) length) (%gzip-error "truncated member trailer"))
      (multiple-value-bind (output consumed)
          (%inflate-raw input :start index :end (- length 8)
                        :max-output-bytes max-output)
        (let ((trailer (+ index consumed)))
          (when (> (+ trailer 8) length) (%gzip-error "raw stream exceeds member"))
          (let ((expected-crc (%u32-le input trailer))
                (actual-crc (crc32 output))
                (expected-size (%u32-le input (+ trailer 4))))
            (unless (= expected-crc actual-crc)
              (error 'checksum-error :format :gzip :reason "member CRC mismatch"
                     :expected expected-crc :actual actual-crc))
            (unless (= expected-size (logand (length output) #xffffffff))
              (%gzip-error "member input size mismatch")))
          (values output
                  (+ trailer 8)
                  (%make-gzip-member-header
                   :flags flags
                   :mtime (%u32-le input (+ start 4))
                   :xfl (aref input (+ start 8))
                   :os (aref input (+ start 9))
                   :fextra fextra
                   :fname fname
                   :fcomment fcomment
                   :fhcrc fhcrc)))))))

(defun gzip-decode (data &key max-output max-output-bytes
                              (allow-trailing-padding nil))
  "Decode all concatenated RFC 1952 members in DATA.
Return the decoded octets and a list of their headers in input order.
When ALLOW-TRAILING-PADDING is true, accept an all-zero suffix only after at least one member."
  (when (and max-output max-output-bytes)
    (error 'deflate-error :message "specify only one output limit"))
  (setf max-output (or max-output max-output-bytes))
  (%validate-max-output max-output)
  (let* ((input (%octets data))
         (output-limit (or max-output +deflate-default-max-output-bytes+))
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
                               (- output-limit (length result)))
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
