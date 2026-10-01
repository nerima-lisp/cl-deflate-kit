(in-package #:asdf-user)
(asdf:defsystem "cl-deflate-kit"
  :description "Pure Common Lisp DEFLATE, zlib and gzip codecs."
  :version "0.1.0" :license "MIT" :author "nerima-lisp"
  :pathname "" :serial t
  :components ((:file "package") (:file "conditions") (:file "deflate")))
(asdf:defsystem "cl-deflate-kit/test"
  :depends-on ("cl-deflate-kit") :pathname "t" :serial t
  :components ((:file "package") (:file "tests"))
  :perform (asdf:test-op (op c) (declare (ignore op c))
             (uiop:symbol-call "DEFLATE-KIT/TEST" "RUN-TESTS")))
