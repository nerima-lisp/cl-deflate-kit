(in-package #:deflate-kit/test)

(defun %octets (string)
  (map '(vector (unsigned-byte 8)) #'char-code string))

(defun %assert-condition (type thunk)
  (handler-case
      (progn (funcall thunk) (error "Expected ~S" type))
    (condition (condition)
      (unless (typep condition type)
        (error "Expected ~S, got ~S" type (type-of condition))))))

(defun %write-octets (pathname octets)
  (with-open-file (stream pathname :direction :output :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-sequence octets stream)))

(defun %read-octets (pathname)
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let ((output (make-array (file-length stream)
                              :element-type '(unsigned-byte 8))))
      (read-sequence output stream)
      output)))

(defun %pattern-octets (length)
  (let ((output (make-array length :element-type '(unsigned-byte 8))))
    (loop for i below length
          do (setf (aref output i) (char-code
                                    (char "the quick brown fox jumps over the lazy dog "
                                          (mod i 44)))))
    output))

(defun %random-octets (length)
  (let ((output (make-array length :element-type '(unsigned-byte 8)))
        (state 2463534242))
    (loop for i below length
          do (setf state (logand #xffffffff (+ (* state 1664525) 1013904223))
                   (aref output i) (ldb (byte 8 24) state)))
    output))

(defun %test-gzip-header-padding (input second)
  (let ((checks 0))
    (flet ((check (passed)
             (incf checks)
             (assert passed)))
      (let* ((member (gzip-compress input :mtime #x89abcdef
                                    :filename "hello.txt" :comment "kit test"))
             (plain-member (gzip-compress second))
             (empty-name-member
               (gzip-compress input :filename "" :comment ""))
             (empty-extra-member (%gzip-with-extra plain-member #()))
             (joined nil)
             (padding nil)
             (garbage nil)
             (mixed nil)
             (inter-member-padding nil))
        (setf (aref member 3) (logior (aref member 3) 1)
              (aref member 8) 2
              (aref member 9) 3
              member (%gzip-with-extra member)
              member (%gzip-with-header-crc member)
              joined (concatenate '(vector (unsigned-byte 8))
                                  member plain-member)
              padding (concatenate '(vector (unsigned-byte 8)) joined #(0 0))
              garbage (concatenate '(vector (unsigned-byte 8)) joined #(1))
              mixed (concatenate '(vector (unsigned-byte 8)) joined #(0 1))
              inter-member-padding
                (concatenate '(vector (unsigned-byte 8)) member #(0) plain-member))
        (multiple-value-bind (decoded headers) (gzip-decode member)
          (check (equalp decoded input))
          (check (= (length headers) 1))
          (let ((header (first headers)))
            (check (gzip-member-header-p header))
            (check (= (gzip-member-header-flags header) #x1f))
            (check (= (gzip-member-header-mtime header) #x89abcdef))
            (check (= (gzip-member-header-xfl header) 2))
            (check (= (gzip-member-header-os header) 3))
            (check (equalp (gzip-member-header-fextra header) #(1 2 3)))
            (check (equalp (gzip-member-header-fname header)
                           (%octets "hello.txt")))
            (check (equalp (gzip-member-header-fcomment header)
                           (%octets "kit test")))
            (check (= (gzip-member-header-fhcrc header)
                      (logand (crc32 (subseq member 0 (%gzip-header-end member)))
                              #xffff)))))
        (multiple-value-bind (decoded headers) (gzip-decode plain-member)
          (check (equalp decoded second))
          (check (= (length headers) 1))
          (let ((header (first headers)))
            (check (zerop (gzip-member-header-flags header)))
            (check (zerop (gzip-member-header-mtime header)))
            (check (zerop (gzip-member-header-xfl header)))
            (check (= (gzip-member-header-os header) 255))
            (check (null (gzip-member-header-fextra header)))
            (check (null (gzip-member-header-fname header)))
            (check (null (gzip-member-header-fcomment header)))
            (check (null (gzip-member-header-fhcrc header)))))
        (multiple-value-bind (decoded headers)
            (gzip-decode empty-name-member)
          (declare (ignore decoded))
          (let ((header (first headers)))
            (check (equalp (gzip-member-header-fname header) #()))
            (check (equalp (gzip-member-header-fcomment header) #()))
            (check (= (gzip-member-header-flags header) 24))))
        (multiple-value-bind (decoded headers)
            (gzip-decode empty-extra-member)
          (declare (ignore decoded))
          (check (= (length headers) 1))
          (check (equalp (gzip-member-header-fextra (first headers)) #()))
          (check (= (gzip-member-header-flags (first headers)) 4)))
        (multiple-value-bind (decoded headers) (gzip-decode joined)
          (check (equalp decoded
                         (concatenate '(vector (unsigned-byte 8)) input second)))
          (check (= (length headers) 2))
          (check (= (gzip-member-header-mtime (first headers)) #x89abcdef))
          (check (equalp (gzip-member-header-fname (first headers))
                         (%octets "hello.txt")))
          (check (zerop (gzip-member-header-mtime (second headers))))
          (check (null (gzip-member-header-fname (second headers))))
          (check (zerop (gzip-member-header-flags (second headers)))))
        (check (progn
                 (%assert-condition 'invalid-container-error
                                    (lambda () (gzip-decode padding)))
                 t))
        (multiple-value-bind (decoded headers)
            (gzip-decode padding :allow-trailing-padding t)
          (check (equalp decoded
                         (concatenate '(vector (unsigned-byte 8)) input second)))
          (check (= (length headers) 2)))
        (check (equalp input
                       (gzip-decode member :allow-trailing-padding t)))
        (check (progn
                 (%assert-condition
                  'invalid-container-error
                  (lambda () (gzip-decode garbage :allow-trailing-padding t)))
                 t))
        (check (progn
                 (%assert-condition
                  'invalid-container-error
                  (lambda () (gzip-decode mixed :allow-trailing-padding t)))
                 t))
        (check (progn
                 (%assert-condition
                  'invalid-container-error
                  (lambda ()
                    (gzip-decode inter-member-padding
                                 :allow-trailing-padding t)))
                 t))
        (check (progn
                 (%assert-condition
                  'invalid-container-error
                  (lambda () (gzip-decode #(0 0) :allow-trailing-padding t)))
                 t))))
    (format t "Gzip header and padding checks passed: ~D~%" checks)
    checks))

(defun %gzip-header-end (gzip)
  (let ((flags (aref gzip 3))
        (index 10))
    (when (logtest flags 4)
      (incf index (+ 2 (logior (aref gzip index)
                               (ash (aref gzip (1+ index)) 8)))))
    (dolist (flag '(8 16))
      (when (logtest flags flag)
        (loop until (zerop (aref gzip index))
              do (incf index))
        (incf index)))
    index))

(defun %gzip-with-extra (gzip &optional (extra #(1 2 3)))
  (let ((header (copy-seq (subseq gzip 0 10))))
    (setf (aref header 3) (logior (aref header 3) 4))
    (concatenate '(vector (unsigned-byte 8))
                 header
                 (vector (ldb (byte 8 0) (length extra))
                         (ldb (byte 8 8) (length extra)))
                 extra
                 (subseq gzip 10))))

(defun %gzip-with-header-crc (gzip &key corrupt)
  (let* ((header-end (%gzip-header-end gzip))
         (header (copy-seq (subseq gzip 0 header-end)))
         (body (subseq gzip header-end))
         (header-crc nil))
    (setf (aref header 3) (logior (aref header 3) 2))
    (setf header-crc (logand (crc32 header) #xffff))
    (when corrupt (setf header-crc (logxor header-crc #xff)))
    (concatenate '(vector (unsigned-byte 8)) header
                 (vector (ldb (byte 8 0) header-crc)
                         (ldb (byte 8 8) header-crc))
                 body)))

(defun %gzip-executable ()
  "gzip")

(defun %external-gzip-round-trip (input &key (level 6) no-name)
  (let* ((directory (uiop:temporary-directory))
         (stem (format nil "cl-deflate-kit-~D" (get-internal-real-time)))
         (source (merge-pathnames (format nil "~A-input" stem) directory))
         (compressed (merge-pathnames (format nil "~A.gz" stem) directory))
         (decoded (merge-pathnames (format nil "~A-output" stem) directory))
         (gzip (%gzip-executable)))
    (unwind-protect
         (progn
           (%write-octets source input)
           (let ((ours (gzip-compress input :level level)))
             (%write-octets compressed ours)
             (uiop:run-program (list gzip "-dc" (namestring compressed))
                              :output decoded :error-output :string)
             (assert (equalp input (%read-octets decoded))))
           (uiop:run-program (append (list gzip)
                                     (if no-name '("-9" "-n") nil)
                                     (list "-c" (namestring source)))
                            :output compressed :error-output :string)
           (assert (equalp input (gzip-decompress (%read-octets compressed)))))
      (dolist (pathname (list source compressed decoded))
        (when (probe-file pathname) (delete-file pathname))))))

(defun %streaming-one-byte-round-trip (input)
  (let ((deflater (make-deflater :level 6)))
    (loop for octet across input
          do (deflater-write deflater
                             (make-array 1 :element-type '(unsigned-byte 8)
                                         :initial-contents (list octet)))
             (deflater-flush deflater))
    (deflater-finish deflater)
    (assert (equalp input (inflate (deflater-output deflater))))))

(defun %stream-api-round-trip (input)
  (let ((deflater (make-deflate-stream :level 6))
        (chunks nil)
        (inflater (make-inflate-stream)))
    (push (deflate-stream-push deflater (subseq input 0 17)) chunks)
    (push (deflate-stream-flush deflater) chunks)
    (push (deflate-stream-push deflater (subseq input 17)) chunks)
    (push (deflate-stream-finish deflater) chunks)
    (let ((encoded (apply #'concatenate '(vector (unsigned-byte 8))
                          (nreverse chunks))))
      (loop for start from 0 below (length encoded) by 3
            do (inflate-stream-push inflater
                                    (subseq encoded start (min (+ start 3)
                                                               (length encoded)))))
      (assert (equalp input (inflate-stream-finish inflater))))))

(defun %assert-output-limit (compressed)
  (let ((caught nil))
    (handler-case (inflate compressed :max-output-bytes 4)
      (inflate-size-limit-exceeded (condition)
        (setf caught condition)))
    (assert caught)
    (assert (= 4 (deflate-output-limit-limit caught)))
    (assert (= 5 (inflate-error-observed caught)))))
(defun %test-zstd-predefined-tables ()
  (let ((checks 0))
    (labels ((check (passed)
               (incf checks)
               (assert passed))
             (table-rows (table)
               (let* ((entries (deflate-kit::zstd-fse-table-entries table))
                      (rows (make-array (length entries))))
                 (dotimes (state (length entries) rows)
                   (let ((entry (aref entries state)))
                     (setf (aref rows state)
                           (vector (deflate-kit::zstd-fse-entry-symbol entry)
                                   (deflate-kit::zstd-fse-entry-bits entry)
                                   (deflate-kit::zstd-fse-entry-baseline entry)))))))
             (check-table (table log expected)
               (check (and (= log (deflate-kit::zstd-fse-table-log table))
                           (equalp expected (table-rows table))))))
      ;; RFC 8878 Appendix A.1, Table 28, states 0-63.
      (check-table
       deflate-kit::+zstd-default-ll-table+
       6
       #(#(0 4 0) #(0 4 16) #(1 5 32) #(3 5 0)
         #(4 5 0) #(6 5 0) #(7 5 0) #(9 5 0)
         #(10 5 0) #(12 5 0) #(14 6 0) #(16 5 0)
         #(18 5 0) #(19 5 0) #(21 5 0) #(22 5 0)
         #(24 5 0) #(25 5 32) #(26 5 0) #(27 6 0)
         #(29 6 0) #(31 6 0) #(0 4 32) #(1 4 0)
         #(2 5 0) #(4 5 32) #(5 5 0) #(7 5 32)
         #(8 5 0) #(10 5 32) #(11 5 0) #(13 6 0)
         #(16 5 32) #(17 5 0) #(19 5 32) #(20 5 0)
         #(22 5 32) #(23 5 0) #(25 4 0) #(25 4 16)
         #(26 5 32) #(28 6 0) #(30 6 0) #(0 4 48)
         #(1 4 16) #(2 5 32) #(3 5 32) #(5 5 32)
         #(6 5 32) #(8 5 32) #(9 5 32) #(11 5 32)
         #(12 5 32) #(15 6 0) #(17 5 32) #(18 5 32)
         #(20 5 32) #(21 5 32) #(23 5 32) #(24 5 32)
         #(35 6 0) #(34 6 0) #(33 6 0) #(32 6 0)))
      ;; RFC 8878 Appendix A.2, Table 29, states 0-63.
      (check-table
       deflate-kit::+zstd-default-ml-table+
       6
       #(#(0 6 0) #(1 4 0) #(2 5 32) #(3 5 0)
         #(5 5 0) #(6 5 0) #(8 5 0) #(10 6 0)
         #(13 6 0) #(16 6 0) #(19 6 0) #(22 6 0)
         #(25 6 0) #(28 6 0) #(31 6 0) #(33 6 0)
         #(35 6 0) #(37 6 0) #(39 6 0) #(41 6 0)
         #(43 6 0) #(45 6 0) #(1 4 16) #(2 4 0)
         #(3 5 32) #(4 5 0) #(6 5 32) #(7 5 0)
         #(9 6 0) #(12 6 0) #(15 6 0) #(18 6 0)
         #(21 6 0) #(24 6 0) #(27 6 0) #(30 6 0)
         #(32 6 0) #(34 6 0) #(36 6 0) #(38 6 0)
         #(40 6 0) #(42 6 0) #(44 6 0) #(1 4 32)
         #(1 4 48) #(2 4 16) #(4 5 32) #(5 5 32)
         #(7 5 32) #(8 5 32) #(11 6 0) #(14 6 0)
         #(17 6 0) #(20 6 0) #(23 6 0) #(26 6 0)
         #(29 6 0) #(52 6 0) #(51 6 0) #(50 6 0)
         #(49 6 0) #(48 6 0) #(47 6 0) #(46 6 0)))
      ;; RFC 8878 Appendix A.3, Table 30, states 0-31.
      (check-table
       deflate-kit::+zstd-default-of-table+
       5
       #(#(0 5 0) #(6 4 0) #(9 5 0) #(15 5 0)
         #(21 5 0) #(3 5 0) #(7 4 0) #(12 5 0)
         #(18 5 0) #(23 5 0) #(5 5 0) #(8 4 0)
         #(14 5 0) #(20 5 0) #(2 5 0) #(7 4 16)
         #(11 5 0) #(17 5 0) #(22 5 0) #(4 5 0)
         #(8 4 16) #(13 5 0) #(19 5 0) #(1 5 0)
         #(6 4 16) #(10 5 0) #(16 5 0) #(28 5 0)
         #(27 5 0) #(26 5 0) #(25 5 0) #(24 5 0)))
    (format t "RFC 8878 predefined FSE table checks passed: ~D~%" checks)
    checks)))
(defun %test-zstd-api-contract ()
  (let ((checks 0))
    (labels ((fixture (name)
               (asdf:system-relative-pathname
                "cl-deflate-kit"
                (format nil "t/zstd-fixtures/~A" name)))
             (check (passed)
               (incf checks)
               (assert passed))
             (rejects (type thunk)
               (incf checks)
               (%assert-condition type thunk))
             (expect-input-limit (thunk limit observed)
               (let ((caught nil))
                 (handler-case (funcall thunk)
                   (zstd-input-limit-exceeded (condition)
                     (setf caught condition)))
                 (check
                  (and (typep caught 'deflate-error)
                       (typep caught 'deflate-input-limit)
                       (not (typep caught 'deflate-output-limit))
                       (eq :input-limit (inflate-error-reason caught))
                       (eql limit (deflate-input-limit-limit caught))
                       (eql observed (inflate-error-observed caught))
                       (eql limit (deflate-error-position caught))
                       (search (format nil "byte ~D" limit)
                               (princ-to-string caught))
                       (search "Zstandard input size limit exceeded"
                               (princ-to-string caught)))))))
      (let* ((plain (%read-octets (fixture "plain.txt")))
             (plain-frame (%read-octets (fixture "plain.zst")))
             (frame-length (length plain-frame))
             (large-frame (%read-octets (fixture "window-9m.zst")))
             (large-output (make-array (* 9 1024 1024)
                                       :element-type '(unsigned-byte 8)
                                       :initial-element #x57)))
        (check (equalp plain
                       (zstd-decode plain-frame :max-output (length plain))))
        (let ((default-input-size (* 16 1024 1024)))
          (check (= default-input-size
                    deflate-kit::+zstd-default-max-input-bytes+))
          (check (= default-input-size
                    (deflate-kit::zstd-stream-max-input-bytes
                     (make-zstd-stream))))
          (expect-input-limit
           (lambda ()
             (zstd-decode
              (make-array (1+ default-input-size)
                          :element-type '(unsigned-byte 8)
                          :initial-element 0)))
           default-input-size (1+ default-input-size)))
        (let ((caught nil))
          (handler-case
              (zstd-decode plain-frame
                           :max-output (length plain)
                           :max-output-bytes (length plain))
            (zstd-invalid-data (condition)
              (setf caught condition)))
          (check (and (typep caught 'zstd-invalid-data)
                      (eq :conflicting-output-limits
                          (inflate-error-reason caught)))))
        (check (equalp large-output
                       (zstd-decode
                        large-frame :unbounded t
                        :max-window-size-bytes (* 9 1024 1024))))
        (check (equalp plain
                       (zstd-decode plain-frame
                                    :max-input-bytes frame-length)))
        (expect-input-limit
         (lambda ()
           (zstd-decode plain-frame :max-input-bytes (1- frame-length)))
         (1- frame-length) frame-length)
        (let ((stream (make-zstd-stream :max-input-bytes frame-length)))
          (zstd-stream-push stream plain-frame)
          (check (equalp plain (zstd-stream-finish stream))))
        (expect-input-limit
         (lambda ()
           (let ((stream (make-zstd-stream
                          :max-input-bytes (1- frame-length))))
             (zstd-stream-push stream plain-frame)))
         (1- frame-length) frame-length)
        (let* ((split (floor frame-length 2))
               (buffer (make-array (max split (- frame-length split))
                                   :element-type '(unsigned-byte 8)
                                   :adjustable t :fill-pointer 0))
               (stream (make-zstd-stream)))
          (loop for index below split
                do (vector-push-extend (aref plain-frame index) buffer))
          (zstd-stream-push stream buffer)
          (setf (fill-pointer buffer) 0)
          (loop for index from split below frame-length
                do (vector-push-extend (aref plain-frame index) buffer))
          (zstd-stream-push stream buffer)
          (check (equalp plain (zstd-stream-finish stream))))
        (let ((caught nil))
          (handler-case
              (zstd-decode (subseq plain-frame 0 (1- frame-length)))
            (zstd-invalid-data (condition)
              (setf caught condition)))
          (check (and (typep caught 'deflate-error)
                      (eq :truncated-input (inflate-error-reason caught))
                      (listp (inflate-error-detail caught))
                      (integerp (deflate-error-position caught))))))
      (format t "FR-013 Zstandard API checks passed: ~D~%" checks)
      checks)))

(defun %test-zstd ()
  (let ((checks 0))
    (labels ((fixture (name)
               (asdf:system-relative-pathname
                "cl-deflate-kit"
                (format nil "t/zstd-fixtures/~A" name)))
             (check (passed)
               (incf checks)
               (assert passed))
             (rejects (type thunk)
               (incf checks)
               (%assert-condition type thunk))
             (decode-at-split (compressed expected split)
               (let ((stream (make-zstd-stream)))
                 (zstd-stream-push stream (subseq compressed 0 split))
                 (zstd-stream-push stream (subseq compressed split))
                 (equalp expected (zstd-stream-finish stream))))
             (check-splits (compressed expected)
               (let ((length (length compressed)))
                 (dolist (split (remove-duplicates
                                 (list 1 4 (floor length 2) (1- length))))
                   (when (< 0 split length)
                     (check (decode-at-split compressed expected split))))))
             (first-block-header-offset (frame)
               (let* ((descriptor (aref frame 4))
                      (single-segment (logtest #x20 descriptor))
                      (content-size-flag (ldb (byte 2 6) descriptor))
                      (dictionary-id-size
                        (aref #(0 1 2 4) (ldb (byte 2 0) descriptor)))
                      (content-size-size
                        (case content-size-flag
                          (0 (if single-segment 1 0))
                          (1 2)
                          (2 4)
                          (3 8))))
                 (+ 5 (if single-segment 0 1)
                    dictionary-id-size content-size-size))))
      (let* ((plain (%read-octets (fixture "plain.txt")))
             (plain-frame (%read-octets (fixture "plain.zst")))
             (checksum-frame (%read-octets (fixture "checksum.zst")))
             (rle-output (%read-octets (fixture "rle-256k.bin")))
             (rle-frame (%read-octets (fixture "rle-256k.zst")))
             (raw-multiblock (%read-octets
                              (fixture "raw-multiblock.bin")))
             (raw-multiblock-frame (%read-octets
                                    (fixture "raw-multiblock.zst"))))
        (dolist (pair (list (list "plain.zst" "plain.txt")
                            (list "checksum.zst" "checksum.txt")
                            (list "empty.zst" "empty.txt")
                            (list "rle-256k.zst" "rle-256k.bin")
                            (list "raw-multiblock.zst" "raw-multiblock.bin")
                            (list "multilingual.zst" "multilingual.txt")
                            (list "multiframe.zst" "multiframe.bin")
                            (list "ncount.zst" "ncount.txt")))
          (check (equalp (%read-octets (fixture (second pair)))
                         (zstd-decode (%read-octets
                                       (fixture (first pair)))))))
        (dolist (name (list "block-128k" "empty-block"
                            "rle-first-block" "zeroSeq_2B"))
          (check (equalp (%read-octets (fixture (concatenate 'string name ".bin")))
                         (zstd-decode
                          (%read-octets
                           (fixture (concatenate 'string name ".zst")))))))
        (check (equalp plain (zstd-decode plain-frame)))
        (check-splits plain-frame plain)
        (check-splits raw-multiblock-frame raw-multiblock)
        (let* ((joined-frame (concatenate '(vector (unsigned-byte 8))
                                          plain-frame plain-frame))
               (joined-output (concatenate '(vector (unsigned-byte 8))
                                           plain plain)))
          (check (equalp joined-output (zstd-decode joined-frame)))
          (check (decode-at-split joined-frame joined-output
                                  (length plain-frame))))
        (let* ((skippable-frame #(95 42 77 24 3 0 0 0 1 2 3))
               (prefixed (concatenate '(vector (unsigned-byte 8))
                                      skippable-frame plain-frame)))
          (check (equalp plain (zstd-decode prefixed)))
          (check (equalp #() (zstd-decode skippable-frame))))
        (let ((bad-checksum (copy-seq checksum-frame)))
          (setf (aref bad-checksum (1- (length bad-checksum)))
                (logxor (aref bad-checksum (1- (length bad-checksum))) 1))
          (rejects 'checksum-error (lambda () (zstd-decode bad-checksum))))
        (rejects 'unsupported-container-error
                 (lambda ()
                   (zstd-decode
                    (%read-octets (fixture "dictionary-id-12345.zst")))))
        (check (equalp rle-output
                       (zstd-decode rle-frame
                                    :max-output-bytes (length rle-output))))
        (rejects 'zstd-size-limit-exceeded
                 (lambda ()
                   (zstd-decode rle-frame
                                :max-output-bytes (1- (length rle-output)))))
        (rejects 'zstd-size-limit-exceeded
                 (lambda ()
                   (zstd-decode
                    (%read-octets (fixture "window-9m.zst"))
                    :max-output-bytes (* 10 1024 1024))))
        (check (equalp (make-array (* 9 1024 1024)
                                   :element-type '(unsigned-byte 8)
                                   :initial-element #x57)
                       (zstd-decode
                        (%read-octets (fixture "window-9m.zst"))
                        :max-output-bytes (* 10 1024 1024)
                        :max-window-size-bytes (* 9 1024 1024))))
        (rejects 'zstd-invalid-data
                 (lambda ()
                   (zstd-decode (subseq plain-frame 0 (1- (length plain-frame))))))
        (progn
          (rejects 'zstd-invalid-data
                   (lambda () (zstd-decode #(0 0 0 0))))
          ;; This zero-count run overruns max-symbol 2 in the normalized-count table.
          (rejects 'zstd-invalid-data
                   (lambda ()
                     (deflate-kit::%zstd-parse-fse-table #(0 6) 0 2 2 9))))
        (let* ((bad-block (copy-seq plain-frame))
               (offset (first-block-header-offset plain-frame)))
          (setf (aref bad-block offset)
                (logior #x06 (logand #xf9 (aref bad-block offset))))
          (progn
            (rejects 'zstd-invalid-data
                     (lambda () (zstd-decode bad-block)))
            (check
             (let* ((output (deflate-kit::%make-zstd-output 8))
                    (data (make-array 4 :element-type '(unsigned-byte 8)
                                      :adjustable t :fill-pointer 3
                                      :initial-contents #(17 34 51 0))))
               (setf (deflate-kit::zstd-output-data output) data)
               (deflate-kit::%zstd-append-output output #(68) 0 1)
               (check (and (= 4 (fill-pointer data))
                           (= 4 (array-total-size data))))
               (deflate-kit::%zstd-backref-output output 1 3)
               (equalp #(17 34 51 68 68 68 68)
                       (subseq (deflate-kit::zstd-output-data output) 0
                               (fill-pointer (deflate-kit::zstd-output-data output))))))
            ;; Offset 3 exceeds the two bytes already in output history.
            (check
             (let ((output (deflate-kit::%make-zstd-output 8))
                   (caught nil))
               (deflate-kit::%zstd-append-output output #(1 2) 0 2)
               (handler-case
                   (deflate-kit::%zstd-backref-output output 3 1)
                 (zstd-invalid-data (condition)
                   (setf caught condition)))
               (and (typep caught 'zstd-invalid-data)
                    (eq :offset-out-of-range
                        (inflate-error-reason caught))))))))
    (format t "FR-013 focused tests passed: ~D~%" checks)
    checks)))
(defun %test-zstd-security-regressions ()
  (let ((checks 0))
    (labels ((fixture (name)
               (asdf:system-relative-pathname
                "cl-deflate-kit"
                (format nil "t/zstd-fixtures/~A" name)))
             (check (passed)
               (incf checks)
               (assert passed))
             (named-check (name passed)
               (check passed)
               (format t "Zstandard regression passed: ~A~%" name))
             (block-start (frame wanted-type)
               (let* ((descriptor (aref frame 4))
                      (single-segment (logtest #x20 descriptor))
                      (content-size-flag (ldb (byte 2 6) descriptor))
                      (dictionary-size
                        (aref #(0 1 2 4) (ldb (byte 2 0) descriptor)))
                      (content-size-size
                        (case content-size-flag
                          (0 (if single-segment 1 0))
                          (1 2)
                          (2 4)
                          (3 8)))
                      (position (+ 5 (if single-segment 0 1)
                                    dictionary-size content-size-size))
                      (end (length frame)))
                 (loop
                   (when (> (+ position 3) end)
                     (return nil))
                   (multiple-value-bind (header after-header)
                       (deflate-kit::%zstd-read-uint-le frame position 3 end)
                     (let ((type (ldb (byte 2 1) header))
                           (size (ash header -3))
                           (last-p (logbitp 0 header)))
                       (when (= type wanted-type)
                         (return position))
                       (when last-p
                         (return nil))
                       (setf position (+ after-header
                                         (if (= type 1) 1 size))))))))
             (check-block-limit (name frame type)
               (let* ((position (block-start frame type))
                      (end (length frame)))
                 (unless position
                   (error "Zstandard fixture has no block of type ~D." type))
                 (let* ((header
                          (nth-value 0
                                     (deflate-kit::%zstd-read-uint-le
                                      frame position 3 end)))
                        (size (ash header -3))
                        (output (deflate-kit::%make-zstd-output (1- size)))
                        (state
                          (deflate-kit::make-zstd-frame-state
                           :window-size (* 128 1024)
                           :block-max (* 128 1024)))
                        (caught nil))
                   (handler-case
                       (deflate-kit::%zstd-decode-block
                        frame position end output 0 state)
                     (zstd-size-limit-exceeded (condition)
                       (setf caught condition)))
                   (named-check
                    name
                    (and caught
                         (eq :output-size-limit (inflate-error-reason caught))
                         (= (1- size) (deflate-output-limit-limit caught))
                         (= size (inflate-error-observed caught))
                         (= position (deflate-error-position caught))
                         (zerop (fill-pointer
                                 (deflate-kit::zstd-output-data output)))))))))
      (let* ((window-frame (%read-octets (fixture "window-9m.zst")))
             (plain-frame (%read-octets (fixture "plain.zst")))
             (checksum-frame (%read-octets (fixture "checksum.zst")))
             (dictionary-frame
               (%read-octets (fixture "dictionary-id-12345.zst")))
             (raw-frame (%read-octets (fixture "raw-multiblock.zst")))
             (rle-frame (%read-octets (fixture "rle-first-block.zst")))
             (window-limit (* 8 1024 1024)))
        (named-check
         "zstd-window-default-is-8-mib"
         (and (= window-limit deflate-kit::+zstd-default-max-window-size-bytes+)
              (= window-limit
                 (deflate-kit::zstd-stream-max-window-size-bytes
                  (make-zstd-stream)))))
        (let ((caught nil))
          (handler-case
              (zstd-decode window-frame :unbounded t)
            (zstd-size-limit-exceeded (condition)
              (setf caught condition)))
          (named-check
           "zstd-window-default-rejects-9-mib"
           (and caught
                (eq :window-size-limit (inflate-error-reason caught))
                (= window-limit (deflate-output-limit-limit caught))
                (> (inflate-error-observed caught) window-limit))))
        (named-check
         "zstd-window-explicit-override-accepts-9-mib"
         (= (* 9 1024 1024)
            (length (zstd-decode
                     window-frame :unbounded t
                     :max-window-size-bytes (* 9 1024 1024)))))
        (let ((caught nil))
          (handler-case
              (zstd-decode #() :max-output-bytes "bad")
            (error (condition)
              (setf caught condition)))
          (named-check
           "zstd-invalid-output-limit-condition"
           (and (typep caught 'zstd-invalid-data)
                (typep caught 'deflate-error)
                (not (typep caught 'type-error))
                (eq :invalid-limit (inflate-error-reason caught)))))
        (check-block-limit "zstd-raw-block-output-limit" raw-frame 0)
        (check-block-limit "zstd-rle-block-output-limit" rle-frame 1)
        (let* ((output (deflate-kit::%make-zstd-output 32))
               (state (deflate-kit::make-zstd-frame-state
                       :window-size (* 128 1024) :block-max (* 128 1024)))
               (caught nil))
          (deflate-kit::%zstd-append-output output #(65) 0 1)
          (handler-case
              (deflate-kit::%zstd-emit-sequence
               output 0 0 state #() 0 0 4 65536 29)
            (zstd-size-limit-exceeded (condition)
              (setf caught condition)))
          (named-check
           "zstd-long-match-output-limit"
           (and caught
                (eq :output-size-limit (inflate-error-reason caught))
                (= 32 (deflate-output-limit-limit caught))
                (= 65537 (inflate-error-observed caught))
                (= 29 (deflate-error-position caught))
                (= 1 (fill-pointer
                      (deflate-kit::zstd-output-data output))))))
        (let ((caught nil))
          (handler-case
              (deflate-kit::%zstd-huffman-table-from-weights #(12))
            (zstd-invalid-data (condition)
              (setf caught condition)))
          (named-check
           "zstd-invalid-huffman-weight"
           (and caught
                (eq :invalid-huffman-table (inflate-error-reason caught))
                (eql 12 (inflate-error-detail caught)))))
        (let* ((state (deflate-kit::make-zstd-frame-state
                       :window-size 1024 :block-max 1024))
               (offsets (deflate-kit::zstd-frame-state-offsets state))
               (first (deflate-kit::%zstd-repeat-offset state 2 1))
               (first-history (copy-seq offsets))
               (second (deflate-kit::%zstd-repeat-offset state 3 0))
               (second-history (copy-seq offsets))
               (third (deflate-kit::%zstd-repeat-offset state 6 1))
               (third-history (copy-seq offsets)))
          (named-check
           "zstd-repeat-offset-transition"
           (and (= first 4) (equalp first-history #(4 1 8))
                (= second 3) (equalp second-history #(3 4 1))
                (= third 3) (equalp third-history #(3 3 4)))))
        (let ((caught nil)
              (frame-end (length plain-frame)))
          (handler-case
              (zstd-decode
               (concatenate '(vector (unsigned-byte 8)) plain-frame #(0)))
            (zstd-invalid-data (condition)
              (setf caught condition)))
          (named-check
           "zstd-trailing-byte-after-frame"
           (and caught
                (eq :truncated-input (inflate-error-reason caught))
                (= frame-end (deflate-error-position caught)))))
        (let ((bad-checksum (copy-seq checksum-frame))
              (caught nil))
          (setf (aref bad-checksum (1- (length bad-checksum)))
                (logxor (aref bad-checksum (1- (length bad-checksum))) 1))
          (handler-case
              (zstd-decode bad-checksum)
            (checksum-error (condition)
              (setf caught condition)))
          (named-check
           "zstd-checksum-error-position"
           (and caught
                (= (- (length checksum-frame) 4)
                   (deflate-error-position caught)))))
        (let* ((descriptor (aref dictionary-frame 4))
               (dictionary-position (+ 5 (if (logbitp 5 descriptor) 0 1)))
               (caught nil))
          (handler-case
              (zstd-decode dictionary-frame)
            (unsupported-container-error (condition)
              (setf caught condition)))
          (named-check
           "zstd-dictionary-id-error-position"
           (and caught
                (eq :dictionary-id (deflate-error-reason caught))
                (= dictionary-position (deflate-error-position caught)))))
        (let* ((max-input (1- (length plain-frame)))
               (split (floor (length plain-frame) 2))
               (stream (make-zstd-stream :max-input-bytes max-input))
               (caught nil))
          (handler-case
              (progn
                (zstd-stream-push stream (subseq plain-frame 0 split))
                (zstd-stream-push stream (subseq plain-frame split)))
            (zstd-input-limit-exceeded (condition)
              (setf caught condition)))
          (named-check
           "zstd-input-limit-across-pushes"
           (and caught
                (typep caught 'deflate-input-limit)
                (not (typep caught 'deflate-output-limit))
                (eq :input-limit (inflate-error-reason caught))
                (= max-input (deflate-input-limit-limit caught))
                (= (1+ max-input) (inflate-error-observed caught))
                (= max-input (deflate-error-position caught)))))))
    (format t "FR-013 Zstandard security regressions passed: ~D~%" checks)
    checks))

(defun run-tests ()
  (let* ((input (%octets "hello hello hello hello"))
         (second (%octets "second member"))
         (raw (deflate input))
         (gzip (gzip-compress input :filename "hello.txt" :comment "kit test"))
         (member-2 (gzip-compress second))
         (zlib (zlib-compress input)))
    (assert (equalp input (inflate raw)))
    ;; RFC 1951 fixed-Huffman vector for the ASCII string "hello".
    (assert (equalp (%octets "hello")
                    (inflate #(203 72 205 201 201 7 0))))
    (multiple-value-bind (prefix consumed)
        (inflate #(203 72 205 201 201 7 0) :truncate-at 4)
      (assert (equalp prefix (%octets "hell")))
      (assert (null consumed)))
    (assert (equalp input (inflate raw :size-hint 64)))
    (assert (= #xcbf43926 (crc32 (%octets "123456789"))))
    (assert (= #x11e60398 (adler32 (%octets "Wikipedia"))))
    (assert (equalp input (gzip-decompress gzip)))
    (let ((extra (gzip-decompress (%gzip-with-extra gzip))))
      (assert (equalp input extra)))
    (let ((bad-extra (copy-seq (%gzip-with-extra gzip))))
      (setf (aref bad-extra 10) #xff
            (aref bad-extra 11) #xff)
      (%assert-condition 'invalid-container-error
                         (lambda () (gzip-decompress bad-extra))))
    (assert (equalp input
                    (gzip-decompress (%gzip-with-header-crc gzip))))
    (%assert-condition 'checksum-error
                       (lambda ()
                         (gzip-decompress
                          (%gzip-with-header-crc gzip :corrupt t))))
    (assert (equalp (concatenate '(vector (unsigned-byte 8)) input second)
                    (gzip-decompress (concatenate '(vector (unsigned-byte 8))
                                                  gzip member-2))))
    ;; The default limit applies to the complete concatenated gzip stream.
    (%assert-condition 'inflate-size-limit-exceeded
                       (lambda ()
                         (let* ((large (make-array (* 16 1024 1024)
                                                   :element-type '(unsigned-byte 8)
                                                   :initial-element 65))
                                (members (concatenate '(vector (unsigned-byte 8))
                                                      (gzip-compress large)
                                                      (gzip-compress #(66)))))
                           (gzip-decompress members))))
    (assert (equalp input (zlib-decompress zlib)))
    (%assert-condition 'inflate-invalid-data
                       (lambda ()
                         (let ((large (make-array 514
                                                  :element-type '(unsigned-byte 8))))
                           (loop for index below (length large)
                                 do (setf (aref large index)
                                          (if (= (mod index 257) 256)
                                              99
                                              (mod index 257))))
                           (let* ((small-window (zlib-compress large))
                                  (cmf #x08)
                                  (flg (logand (aref small-window 1) #xe0))
                                  (fcheck (mod (- 31 (mod (+ (ash cmf 8) flg)
                                                             31))
                                                  31)))
                             (setf (aref small-window 0) cmf
                                   (aref small-window 1) (logior flg fcheck))
                             (zlib-decompress small-window)))))
    (let ((bad (copy-seq gzip)))
      (setf (aref bad (- (length bad) 8))
            (logxor (aref bad (- (length bad) 8)) #xff))
      (%assert-condition 'checksum-error (lambda () (gzip-decompress bad))))
    (let ((bad (copy-seq zlib)))
      (setf (aref bad (1- (length bad)))
            (logxor (aref bad (1- (length bad))) #xff))
      (%assert-condition 'checksum-error (lambda () (zlib-decompress bad))))
    (%assert-condition 'unsupported-container-error
                       (lambda ()
                         (let ((dictionary (copy-seq zlib)))
                           (let* ((cmf (aref dictionary 0))
                                  (flg (logior (logand (aref dictionary 1) #xe0)
                                               #x20))
                                  (fcheck (mod (- 31 (mod (+ (ash cmf 8) flg)
                                                           31))
                                                31)))
                             (setf (aref dictionary 1) (logior flg fcheck)))
                           (zlib-decompress dictionary))))
    (%assert-condition 'inflate-size-limit-exceeded
                       (lambda () (gzip-decompress gzip :max-output 4)))
    (%assert-condition 'inflate-size-limit-exceeded
                       (lambda () (zlib-decompress zlib :max-output 4)))
    (%assert-condition 'inflate-invalid-data
                       (lambda () (inflate (make-array 1 :element-type '(unsigned-byte 8)
                                                        :initial-contents '(7)))))
    (%assert-condition 'inflate-invalid-data
                       (lambda () (inflate (subseq raw 0 (1- (length raw))))))
    (let ((random (%random-octets 32)))
      (assert (equalp random (inflate (deflate random :level 6)))))
    (dolist (level '(0 1 3 6 9))
      (%external-gzip-round-trip (%pattern-octets 4097) :level level))
    (%external-gzip-round-trip (%pattern-octets 4097) :level 9 :no-name t)
    (dolist (size '(0 1 65535 65536 65537 131071))
      (let ((data (if (zerop size) (make-array 0 :element-type '(unsigned-byte 8))
                      (%pattern-octets size))))
        (assert (equalp data (inflate (deflate data))))
        (assert (equalp data (gzip-decompress (gzip-compress data))))
        (assert (equalp data (zlib-decompress (zlib-compress data))))))
    (let ((repetitive (%pattern-octets 131072))
          (random (%random-octets 131072)))
      (assert (< (length (deflate repetitive :level 6)) (length repetitive)))
      (assert (equalp random (inflate (deflate random :level 6)))))
    (%streaming-one-byte-round-trip (%pattern-octets 257))
    (%stream-api-round-trip (%pattern-octets 257))
    (%assert-output-limit (deflate (%pattern-octets 1024)))
    (%assert-condition 'inflate-size-limit-exceeded
                       (lambda ()
                         (inflate (deflate (%pattern-octets 4))
                                  :max-output-bytes 0)))
    (%assert-condition 'inflate-invalid-data
                       (lambda ()
                         (inflate (concatenate '(vector (unsigned-byte 8))
                                               raw #(0)))))
    (%assert-condition 'deflate-error
                       (lambda () (deflate input :raw nil)))
    (%assert-condition 'deflate-error
                       (lambda () (deflate input :sync-flush t)))
    (%assert-condition 'deflate-error
                       (lambda () (deflater-flush (make-deflater) :sync nil)))
    (progn
      (%test-gzip-header-padding input second)
      (run-codec-contract-tests)
      (progn
        (%test-zstd-predefined-tables)
        (%test-zstd-api-contract)
        (progn (%test-zstd) (%test-zstd-security-regressions))))
    (format t "FR-002 focused tests passed~%")
    t))
