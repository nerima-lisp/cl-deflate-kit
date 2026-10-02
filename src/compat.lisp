(in-package #:deflate-kit)

(register-raw-codecs
 (lambda (data &key level)
   (deflate data :level (or level 6)))
 (lambda (data &key (start 0) end max-output-bytes truncate-at size-hint
                   window-size)
   (inflate data :start start :end end :allow-trailing t
            :truncate-at truncate-at :size-hint size-hint
            :window-size (or window-size 32768)
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
