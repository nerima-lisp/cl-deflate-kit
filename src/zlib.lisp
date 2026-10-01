(in-package #:deflate-kit)

(defun %u16-be (octets index)
  (logior (ash (aref octets index) 8)
          (aref octets (1+ index))))

(defun %u32-be (octets index)
  (logior (ash (aref octets index) 24)
          (ash (aref octets (+ index 1)) 16)
          (ash (aref octets (+ index 2)) 8)
          (aref octets (+ index 3))))

(defun %put-u32-be (octets index value)
  (setf (aref octets index) (ldb (byte 8 24) value)
        (aref octets (+ index 1)) (ldb (byte 8 16) value)
        (aref octets (+ index 2)) (ldb (byte 8 8) value)
        (aref octets (+ index 3)) (ldb (byte 8 0) value))
  octets)

(defun %container-error (format reason)
  (error 'invalid-container-error :format format :reason reason))

(defun %deflate-raw (data &key level)
  (or *raw-deflate-function*
      (%container-error :deflate "no raw deflate codec has been registered"))
  (multiple-value-bind (result consumed)
      (funcall *raw-deflate-function* (%octets data) :level level)
    (declare (ignore consumed))
    (%octets result)))

(defun %inflate-raw (data &key (start 0) end max-output-bytes)
  (or *raw-inflate-function*
      (%container-error :inflate "no raw inflate codec has been registered"))
  (let ((input (%octets data)))
    (multiple-value-bind (result consumed)
        (funcall *raw-inflate-function* input :start start :end end
                 :max-output-bytes max-output-bytes)
      (values (%octets result) (or consumed (- (or end (length input)) start))))))

(defun zlib-encode (data &key (level 6))
  "Wrap raw DEFLATE DATA in an RFC 1950 zlib stream."
  (let* ((input (%octets data))
         (compressed (%deflate-raw input :level level))
         ;; CM=8, CINFO=7 (32 KiB window), FCHECK is selected below.
         (cmf #x78)
         (flg (logand #xe0 (case level
                             ((0 1) #x00)
                             ((2 3 4 5) #x40)
                             ((6) #x80)
                             ((7 8 9) #xc0)
                             (otherwise #x80))))
         (header (logior flg (mod (- 31 (mod (+ (ash cmf 8) flg) 31)) 31)))
         (result (make-array (+ 2 (length compressed) 4)
                             :element-type '(unsigned-byte 8))))
    (setf (aref result 0) cmf
          (aref result 1) header)
    (replace result compressed :start1 2)
    (%put-u32-be result (+ 2 (length compressed)) (adler32 input))))

(defun zlib-decode (data &key max-output)
  "Decode and validate one RFC 1950 zlib stream."
  (%validate-max-output max-output)
  (let* ((input (%octets data))
         (length (length input)))
    (when (< length 6)
      (%container-error :zlib "stream is shorter than its header and trailer"))
    (let ((cmf (aref input 0))
          (flg (aref input 1)))
      (unless (and (= (logand cmf #x0f) 8)
                   (<= (ash cmf -4) 7)
                   (zerop (mod (+ (ash cmf 8) flg) 31)))
        (%container-error :zlib "invalid CMF/FLG header")))
    (when (logtest (aref input 1) #x20)
      (error 'unsupported-container-error :format :zlib
             :reason "preset dictionaries are not supported"))
    (multiple-value-bind (output consumed)
        (%inflate-raw input :start 2 :end (- length 4)
                      :max-output-bytes max-output)
      (unless (= consumed (- length 6))
        (%container-error :zlib "raw stream does not end before the Adler-32 trailer"))
      (let ((expected (%u32-be input (- length 4)))
            (actual (adler32 output)))
        (unless (= expected actual)
          (error 'checksum-error :format :zlib :reason "Adler-32 mismatch"
                 :expected expected :actual actual)))
      output)))

(setf (fdefinition 'encode-zlib) #'zlib-encode
      (fdefinition 'decode-zlib) #'zlib-decode
      (fdefinition 'zlib-compress) #'zlib-encode
      (fdefinition 'zlib-decompress) #'zlib-decode)
