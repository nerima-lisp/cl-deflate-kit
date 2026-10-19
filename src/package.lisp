(defpackage #:deflate-kit
  (:use #:cl)
  (:export
   #:deflate-error #:deflate-error-message #:deflate-error-position
   #:deflate-output-limit #:deflate-output-limit-limit
   #:deflate-input-limit #:deflate-input-limit-limit
   #:inflate-invalid-data #:inflate-error-reason #:inflate-error-detail
   #:inflate-size-limit-exceeded #:inflate-error-observed
   #:zstd-invalid-data #:zstd-size-limit-exceeded
   #:invalid-container-error #:unsupported-container-error #:checksum-error
   #:deflate-error-format #:deflate-error-reason
   #:invalid-compression-level #:invalid-compression-level-level
   #:deflater-finished-error
   #:inflate #:deflate
   #:gzip-encode #:gzip-decode #:gzip-member-header-at #:gzip-decode-prefix
   #:gzip-compress #:gzip-decompress
   #:gzip-member-header #:gzip-member-header-p
   #:gzip-member-header-flags #:gzip-member-header-mtime
   #:gzip-member-header-xfl #:gzip-member-header-extra-flags
   #:gzip-member-header-os #:gzip-member-header-operating-system
   #:gzip-member-header-fextra #:gzip-member-header-extra
   #:gzip-member-header-fname #:gzip-member-header-fcomment
   #:gzip-member-header-fhcrc
   #:zlib-encode #:zlib-decode #:zlib-compress #:zlib-decompress
   #:zstd-decode #:crc32 #:adler32 #:register-raw-codecs
   #:make-deflater #:deflater-write #:deflater-flush #:deflater-finish
   #:deflater-output #:deflater-level
   #:make-inflate-stream #:inflate-stream-push #:inflate-stream-finish
   #:make-deflate-stream #:deflate-stream-push #:deflate-stream-flush
   #:deflate-stream-finish
   #:make-zstd-stream #:zstd-stream-push #:zstd-stream-finish
   #:brotli-decode #:make-brotli-stream
   #:brotli-stream-push #:brotli-stream-finish #:brotli-invalid-data
   #:brotli-size-limit-exceeded #:brotli-input-limit-exceeded))
