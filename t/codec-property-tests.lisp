(in-package #:deflate-kit/test)
  (cl-weave:describe "cl-deflate-kit codec round-trip properties"
    (cl-weave:it-property "raw DEFLATE round-trips bounded byte vectors"
        ((octet-values
          (cl-weave:gen-vector
           (cl-weave:gen-integer :min 0 :max 255)
           :min-length 0 :max-length 256)))
      (let ((input (map (quote (vector (unsigned-byte 8)))
                        (function identity)
                        octet-values)))
        (assert (equalp input
                        (deflate-kit:inflate (deflate-kit:deflate input)
                                             :unbounded t))))
      t)
    (cl-weave:it-property "gzip round-trips bounded byte vectors"
        ((octet-values
          (cl-weave:gen-vector
           (cl-weave:gen-integer :min 0 :max 255)
           :min-length 0 :max-length 256)))
      (let ((input (map (quote (vector (unsigned-byte 8)))
                        (function identity)
                        octet-values)))
        (assert (equalp input
                        (deflate-kit:gzip-decode
                         (gzip-compress input)
                         :unbounded t))))
      t))
  (cl-weave:describe "cl-deflate-kit gzip position contracts"
    (cl-weave:it "returns the absolute end of a gzip header at a nonzero offset"
      (let* ((start 3)
             (name "first.bin")
             (input (%codec-contract-octets "first member"))
             (member (gzip-compress input :filename name))
             (data (concatenate
                    (quote (vector (unsigned-byte 8)))
                    #(91 92 93)
                    member)))
        (multiple-value-bind (header next)
            (deflate-kit:gzip-member-header-at data :start start)
          (assert (gzip-member-header-p header))
          (assert (equalp (gzip-member-header-fname header)
                          (%codec-contract-octets name)))
          (assert (= next (+ start 10 (length name) 1)))))
      t)
    (cl-weave:it "decodes only the first member and reports bounded prefix positions"
      (let* ((start 3)
             (first (%codec-contract-octets "first member"))
             (second (%codec-contract-octets "second member"))
             (first-member (gzip-compress first))
             (second-member (gzip-compress second))
             (data (concatenate
                    (quote (vector (unsigned-byte 8)))
                    #(91 92 93)
                    first-member
                    second-member))
             (after-first nil))
        (progn
          (multiple-value-bind (prefix header next)
              (deflate-kit:gzip-decode-prefix data :start start :max-output 0)
            (assert (zerop (length prefix)))
            (assert (gzip-member-header-p header))
            (assert (null next)))
          (multiple-value-bind (prefix header next)
              (deflate-kit:gzip-decode-prefix data :start start :max-output 4)
            (assert (equalp prefix (subseq first 0 4)))
            (assert (gzip-member-header-p header))
            (assert (null next))))
        (multiple-value-bind (decoded header next)
            (deflate-kit:gzip-decode-prefix
             data :start start :max-output (1+ (length first)))
          (assert (equalp decoded first))
          (assert (gzip-member-header-p header))
          (assert (= next (+ start (length first-member))))
          (setf after-first next))
        (multiple-value-bind (decoded header next)
            (deflate-kit:gzip-decode-prefix
             data :start after-first :max-output 256)
          (assert (equalp decoded second))
          (assert (gzip-member-header-p header))
          (assert (= next (length data)))))
      (progn
        (let* ((repetitive (make-array 4096 :element-type
                                       '(unsigned-byte 8)
                                       :initial-element 65))
               (raw (deflate-kit:deflate repetitive))
               (member (gzip-compress repetitive)))
          (assert (< (length raw) 64))
          (assert (equalp (subseq repetitive 0 10)
                          (deflate-kit:inflate raw :truncate-at 10)))
          (multiple-value-bind (prefix header next)
              (deflate-kit:gzip-decode-prefix member :max-output 10)
            (assert (equalp prefix (subseq repetitive 0 10)))
            (assert (gzip-member-header-p header))
            (assert (null next))))
        t)))
  (defun run-codec-contract-tests ()
    (unless (cl-weave:run-all :pass-with-no-tests nil)
      (error "cl-weave reported a codec contract failure."))
    t)

