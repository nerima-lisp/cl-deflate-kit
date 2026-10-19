(in-package #:deflate-kit/test)

(defvar *brotli-assertion-count* 0)

(defun %brotli-octets (&rest octets)
  (make-array (length octets) :element-type '(unsigned-byte 8)
                            :initial-contents octets))

(defun %brotli-check (test description)
  (incf *brotli-assertion-count*)
  (unless test
    (error "Brotli test failed: ~A" description)))

(defun %brotli-check-condition (type thunk description)
  (let ((caught nil))
    (handler-case (funcall thunk)
      (condition (condition)
        (setf caught condition)))
    (%brotli-check (typep caught type) description)
    caught))

(defun %brotli-output-p (actual expected)
  (and (typep actual '(vector (unsigned-byte 8)))
       (equalp expected actual)))

(defun %brotli-check-output (actual expected description)
  (%brotli-check (%brotli-output-p actual expected) description)
  actual)

(defun %brotli-fixture (name)
  (%read-octets
   (asdf:system-relative-pathname
    "cl-deflate-kit/test"
    (format nil "t/fixtures/brotli/~A" name))))

(defun %brotli-stream-decode-chunked (compressed expected width description)
  (let ((stream (make-brotli-stream)))
    (loop for start from 0 below (length compressed) by width
          for end = (min (+ start width) (length compressed))
          for chunk = (subseq compressed start end)
          for index from 0
          do (%brotli-check (null (brotli-stream-push stream chunk))
                            (format nil "~A push ~D returns NIL" description index)))
    (%brotli-check-output (brotli-stream-finish stream) expected description)))

(defun run-brotli-tests ()
  (let ((*brotli-assertion-count* 0)
        (empty (%brotli-fixture "empty"))
        (empty-br (%brotli-fixture "empty.br"))
        (simple (%brotli-fixture "simple.txt"))
        (simple-br (%brotli-fixture "simple.txt.br"))
        (dictionary (%brotli-fixture "dictionary.txt"))
        (dictionary-br (%brotli-fixture "dictionary.txt.br"))
        (large (%brotli-fixture "large.txt"))
        (large-br (%brotli-fixture "large.txt.br")))
    (%brotli-check (zerop (length empty)) "empty fixture has zero output bytes")
    (%brotli-check (= #x100000 (length large))
                   "large fixture is exactly 1 MiB")
    (%brotli-check-output (brotli-decode empty-br) empty
                          "empty CLI fixture decodes")
    (%brotli-check-output (brotli-decode simple-br) simple
                          "simple CLI fixture decodes")
    (%brotli-check-output (brotli-decode dictionary-br) dictionary
                          "dictionary CLI fixture decodes")
    (%brotli-check-output (brotli-decode large-br) large
                          "large CLI fixture decodes")

    (dolist (width '(1 2 7 31 43))
      (%brotli-stream-decode-chunked simple-br simple width
                                     (format nil "simple stream width ~D" width)))
    (dolist (width '(1 3 17 53))
      (%brotli-stream-decode-chunked dictionary-br dictionary width
                                     (format nil "dictionary stream width ~D" width)))
    (dolist (width '(4093 65536))
      (%brotli-stream-decode-chunked large-br large width
                                     (format nil "large stream width ~D" width)))

    (let ((uncompressed
            (%brotli-octets #x20 #x00 #x10 #x41 #x42 #x43 #x03))
          (metadata-and-uncompressed
            (%brotli-octets #x2c #x00 #x4d #x10 #x00 #x08 #x41 #x42 #x43 #x03))
          (distance-cache-seed
            (%brotli-octets #x82 #x00 #x00 #x00 #x44 #x50 #x28 #x12 #x10))
          (static-dictionary
            (%brotli-octets #x62 #x00 #x00 #x00 #x04 #x40 #x08 #x10 #x00)))
      ;; RFC 7932 Sections 9.1-9.2, encoded with WBITS 16.
      (%brotli-check-output (brotli-decode uncompressed) (%octets "ABC")
                            "RFC uncompressed meta-block followed by final empty block")
      (%brotli-check-output (brotli-decode metadata-and-uncompressed)
                            (%octets "ABC")
                            "RFC metadata meta-block followed by uncompressed and final empty blocks")
      ;; RFC 7932 Sections 3.4, 4, and 5 use one-symbol trees for this explicit distance copy.
      (%brotli-check-output (brotli-decode distance-cache-seed)
                            (%octets "AAAAA")
                            "RFC one-symbol simple prefix trees decode an explicit distance")
      ;; RFC 7932 Sections 4, 5, and 8: IAC 2 reuses distance 4, selecting "left".
      (%brotli-check-output (brotli-decode static-dictionary)
                            (%octets "left")
                            "one-shot decode resets distance history before static dictionary references"))

    (%brotli-check-condition
     'brotli-invalid-data
     (lambda ()
       (brotli-decode (subseq simple-br 0 (1- (length simple-br)))))
     "truncated CLI stream is rejected")
    (%brotli-check-condition
     'brotli-invalid-data
     (lambda ()
       (brotli-decode
        (concatenate '(vector (unsigned-byte 8)) simple-br (%brotli-octets 0))))
     "trailing bytes are rejected")
    (%brotli-check-condition
     'brotli-invalid-data
     (lambda ()
       (let ((stream (make-brotli-stream)))
         (brotli-stream-push stream
                             (subseq simple-br 0 (1- (length simple-br))))
         (brotli-stream-finish stream)))
     "truncated stream input is rejected at finish")
    (let ((bad-padding (copy-seq empty-br)))
      (%brotli-check (zerop (logand #x80 (aref bad-padding (1- (length bad-padding)))))
                     "empty fixture has zero final padding bits")
      (setf (aref bad-padding (1- (length bad-padding)))
            (logior #x80 (aref bad-padding (1- (length bad-padding)))))
      (%brotli-check-condition
       'brotli-invalid-data
       (lambda () (brotli-decode bad-padding))
       "nonzero final padding is rejected"))

    (let ((condition
            (%brotli-check-condition
             'brotli-size-limit-exceeded
             (lambda () (brotli-decode simple-br :max-output 8))
             "one-shot output limit is enforced")))
      (%brotli-check (= 8 (deflate-output-limit-limit condition))
                     "one-shot output limit is reported")
      (%brotli-check (> (brotli-size-limit-exceeded-observed condition) 8)
                     "one-shot observed output exceeds the limit"))
    (%brotli-check-condition
     'brotli-size-limit-exceeded
     (lambda ()
       (let ((stream (make-brotli-stream :max-output-bytes 8)))
         (brotli-stream-push stream simple-br)
         (brotli-stream-finish stream)))
     "stream output limit is enforced")

    (unless (plusp *brotli-assertion-count*)
      (error "Brotli test suite ran no checks"))
    (format t "Brotli focused tests passed (~D checks)~%"
            *brotli-assertion-count*)
    t))
