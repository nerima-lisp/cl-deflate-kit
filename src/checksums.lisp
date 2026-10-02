(in-package #:deflate-kit)

(define-condition invalid-container-error (deflate-error)
  ((format :initarg :format :reader deflate-error-format)
   (reason :initarg :reason :reader deflate-error-reason))
  (:report (lambda (condition stream)
            (format stream "Invalid ~A stream: ~A"
                    (deflate-error-format condition)
                    (deflate-error-reason condition)))))

(define-condition checksum-error (deflate-error)
  ((expected :initarg :expected :reader checksum-error-expected)
   (actual :initarg :actual :reader checksum-error-actual))
  (:report (lambda (condition stream)
            (format stream "Invalid ~A checksum (expected ~8,'0X, got ~8,'0X)"
                    (deflate-error-format condition)
                    (checksum-error-expected condition)
                    (checksum-error-actual condition)))))

(define-condition unsupported-container-error (invalid-container-error) ())

(export '(invalid-container-error checksum-error unsupported-container-error
          deflate-error-format deflate-error-reason register-raw-codecs))

(defparameter +crc32-table+
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (index 256 table)
      (let ((value index))
        (loop repeat 8
              do
          (setf value (if (logbitp 0 value)
                          (logxor #xedb88320 (ash value -1))
                          (ash value -1))))
        (setf (aref table index) value)))))

(defun %octets (value)
  (typecase value
    ((vector (unsigned-byte 8)) value)
    (string (let ((result (make-array (length value)
                                      :element-type '(unsigned-byte 8))))
              (loop for character across value
                    for index from 0
                    do (let ((code (char-code character)))
                         (unless (<= code 255)
                           (error 'type-error :datum character
                                  :expected-type '(unsigned-byte 8)))
                         (setf (aref result index) code)))
              result))
    ((vector * *) (let ((result (make-array (length value)
                                             :element-type '(unsigned-byte 8))))
                    (loop for byte across value
                          for index from 0
                          do (unless (typep byte '(unsigned-byte 8))
                               (error 'type-error :datum byte
                                      :expected-type '(unsigned-byte 8)))
                             (setf (aref result index) byte))
                    result))
    (list (let ((result (make-array (length value)
                                    :element-type '(unsigned-byte 8))))
            (loop for byte in value
                  for index from 0
                  do (unless (typep byte '(unsigned-byte 8))
                       (error 'type-error :datum byte
                              :expected-type '(unsigned-byte 8)))
                     (setf (aref result index) byte))
            result))
    (t (error 'type-error :datum value
              :expected-type '(or (vector (unsigned-byte 8)) list string)))))

(defun %u32 (value)
  (logand value #xffffffff))

(defun %validate-max-output (max-output)
  (when (and max-output
             (or (not (integerp max-output)) (minusp max-output)))
    (error 'type-error :datum max-output :expected-type '(or null (integer 0 *))))
  max-output)

(defun crc32 (data &key (initial #xffffffff) (start 0) end)
  "Return the IEEE CRC-32 of DATA as an unsigned 32-bit integer."
  (let ((octets (%octets data))
        (crc (%u32 initial)))
    (loop for index fixnum from start below (or end (length octets))
          do (setf crc
                   (logxor (aref +crc32-table+
                                 (logand (logxor crc (aref octets index)) #xff))
                           (ash crc -8))))
    (%u32 (logxor crc #xffffffff))))

(defun adler32 (data &key (initial 1) (start 0) end)
  "Return the Adler-32 of DATA as an unsigned 32-bit integer."
  (let ((a (logand initial #xffff))
        (b (logand (ash initial -16) #xffff)))
    (loop for byte across (subseq (%octets data) start end)
          do (setf a (mod (+ a byte) 65521)
                   b (mod (+ b a) 65521)))
    (logior (ash b 16) a)))

(defvar *raw-deflate-function* nil)
(defvar *raw-inflate-function* nil)

(defun register-raw-codecs (deflate-function inflate-function)
  "Register the raw DEFLATE/INFLATE functions used by container codecs.

Each function receives an octet vector and may accept the keyword arguments
used by the corresponding internal wrapper.  The inflate function must
return either an octet vector, or two values of octet vector and consumed
input length when decoding a member from a larger stream."
  (check-type deflate-function function)
  (check-type inflate-function function)
  (setf *raw-deflate-function* deflate-function
        *raw-inflate-function* inflate-function)
  (values deflate-function inflate-function))
