(defpackage #:deflate-kit
  (:use #:cl)
  (:export
   #:deflate-error #:deflate-error-message #:deflate-error-position
   #:deflate-output-limit #:deflate-output-limit-limit
   #:inflate #:deflate #:zlib-decompress #:zlib-compress
   #:gzip-decompress #:gzip-compress
   #:crc32 #:adler32
   #:make-inflate-stream #:inflate-stream-push #:inflate-stream-finish
   #:make-deflate-stream #:deflate-stream-push #:deflate-stream-finish))
