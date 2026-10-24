(in-package #:asdf-user)
(asdf:defsystem "cl-deflate-kit"
  :description "Pure Common Lisp DEFLATE, zlib, and gzip codecs with Brotli and Zstandard decompression."
  :version "0.4.0" :license "MIT" :author "nerima-lisp"
  :pathname "src" :serial t
  :components ((:file "package") (:file "conditions")
               (:file "checksums") (:file "inflate") (:file "deflate")
               (:file "zlib") (:file "gzip") (:file "zstd") (:file "compat")
               (:file "brotli-dictionary") (:file "brotli"))
  :in-order-to ((test-op (test-op "cl-deflate-kit/test"))))
(asdf:defsystem "cl-deflate-kit/test"
  :depends-on ("cl-deflate-kit" "cl-weave") :pathname "t" :serial t
  :components ((:file "package") (:file "corrupt-fixtures")
               (:file "codec-property-tests") (:file "tests")
               (:file "brotli-tests"))
  :perform (asdf:test-op (op c) (declare (ignore op c))
             (progn
               (uiop:symbol-call "DEFLATE-KIT/TEST" "RUN-TESTS")
               (uiop:symbol-call "DEFLATE-KIT/TEST" "RUN-BROTLI-TESTS"))))
