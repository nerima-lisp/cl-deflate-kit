(defpackage #:deflate-kit
  (:use #:cl)
  (:export
   #:deflate-error #:deflate-error-message #:deflate-error-position
   #:deflate-output-limit #:deflate-output-limit-limit
   #:inflate-invalid-data #:inflate-error-reason #:inflate-error-detail
   #:inflate-size-limit-exceeded #:inflate-error-observed
   #:invalid-container-error #:unsupported-container-error #:checksum-error
   #:deflate-error-format #:deflate-error-reason
   #:invalid-compression-level #:invalid-compression-level-level
   #:deflater-finished-error
   #:inflate #:deflate
   #:gzip-encode #:gzip-decode #:gzip-compress #:gzip-decompress
   #:zlib-encode #:zlib-decode #:zlib-compress #:zlib-decompress
   #:crc32 #:adler32 #:register-raw-codecs
   #:make-deflater #:deflater-write #:deflater-flush #:deflater-finish
   #:deflater-output #:deflater-level
   #:make-inflate-stream #:inflate-stream-push #:inflate-stream-finish
   #:make-deflate-stream #:deflate-stream-push #:deflate-stream-flush
   #:deflate-stream-finish))
