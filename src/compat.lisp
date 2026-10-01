(in-package #:deflate-kit)

(register-raw-codecs
 (lambda (data &key level)
   (deflate data :level (or level 6)))
 (lambda (data &key (start 0) end max-output-bytes)
   (inflate data :start start :end end :allow-trailing t
            :max-output-bytes (or max-output-bytes
                                  +deflate-default-max-output-bytes+))))

(defstruct (deflate-stream (:constructor %make-deflate-stream))
  deflater)

(defun make-deflate-stream (&key (level 6))
  (%make-deflate-stream :deflater (make-deflater :level level)))

(defun deflate-stream-push (stream octets)
  (deflater-write (deflate-stream-deflater stream) octets))

(defun deflate-stream-flush (stream)
  (deflater-flush (deflate-stream-deflater stream)))

(defun deflate-stream-finish (stream)
  (deflater-finish (deflate-stream-deflater stream)))
