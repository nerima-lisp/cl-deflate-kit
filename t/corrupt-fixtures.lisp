(in-package #:deflate-kit/test)
  (defparameter *malformed-raw-deflate-fixtures* (list #() #(7)))
  (defparameter *malformed-gzip-fixtures*
    (list #() #(31 139) #(31 139 8 0 0 0 0 0 0 255)))
  (defun %codec-contract-octets (string)
    (map (quote (vector (unsigned-byte 8)))
         (function char-code)
         string))
  (defun %codec-contract-signals (type thunk)
    (let ((condition nil))
      (handler-case (funcall thunk)
        (error (caught) (setf condition caught)))
      (unless (typep condition type)
        (error "Expected ~S, got ~S."
               type
               (if condition (type-of condition) :no-condition)))
      condition))
  (defun %gzip-with-corrupt-crc (input)
    (let ((encoded (gzip-compress input)))
      (setf (aref encoded (- (length encoded) 8))
            (logxor (aref encoded (- (length encoded) 8)) #xff))
      encoded))
  (defun %gzip-with-corrupt-isize (input)
    (let ((encoded (gzip-compress input)))
      (setf (aref encoded (1- (length encoded)))
            (logxor (aref encoded (1- (length encoded))) #xff))
      encoded))
  (cl-weave:describe "cl-deflate-kit malformed input and bounds"
    (cl-weave:it "rejects empty and malformed raw DEFLATE streams"
      (dolist (fixture *malformed-raw-deflate-fixtures*)
        (%codec-contract-signals
         (quote inflate-invalid-data)
         (lambda () (deflate-kit:inflate fixture :unbounded t))))
      t)
    (cl-weave:it "rejects empty, truncated, and malformed gzip members"
      (dolist (fixture *malformed-gzip-fixtures*)
        (%codec-contract-signals
         (quote invalid-container-error)
         (lambda () (deflate-kit:gzip-decode fixture :unbounded t))))
      t)
    (cl-weave:it "checks gzip CRC and ISIZE trailers"
      (let ((input (%codec-contract-octets "checksum")))
        (%codec-contract-signals
         (quote checksum-error)
         (lambda ()
           (deflate-kit:gzip-decode
            (%gzip-with-corrupt-crc input)
            :unbounded t)))
        (%codec-contract-signals
         (quote invalid-container-error)
         (lambda ()
           (deflate-kit:gzip-decode
            (%gzip-with-corrupt-isize input)
            :unbounded t))))
      t)
    (cl-weave:it "keeps explicit output limits while allowing unbounded decoding"
      (let* ((input (%codec-contract-octets "12345"))
             (raw (deflate-kit:deflate input))
             (gzip (gzip-compress input)))
        (%codec-contract-signals
         (quote inflate-size-limit-exceeded)
         (lambda () (deflate-kit:inflate raw :max-output-bytes 4)))
        (%codec-contract-signals
         (quote inflate-size-limit-exceeded)
         (lambda () (deflate-kit:gzip-decode gzip :max-output 4)))
        (assert (equalp input (deflate-kit:inflate raw :unbounded t)))
        (assert (equalp input (deflate-kit:gzip-decode gzip :unbounded t))))
      t)
    (cl-weave:it "decodes every member in a concatenated gzip stream"
      (let* ((first (%codec-contract-octets "first member"))
             (second (%codec-contract-octets "second member"))
             (joined (concatenate
                      (quote (vector (unsigned-byte 8)))
                      (gzip-compress first)
                      (gzip-compress second))))
        (multiple-value-bind (decoded headers)
            (deflate-kit:gzip-decode joined :unbounded t)
          (assert (equalp decoded
                          (concatenate
                           (quote (vector (unsigned-byte 8)))
                           first second)))
          (assert (= (length headers) 2)))
        (progn
          (%codec-contract-signals
           (quote inflate-size-limit-exceeded)
           (lambda ()
             (deflate-kit:gzip-decode
              joined :max-output (length first))))
          t))))

