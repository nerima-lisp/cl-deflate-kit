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
        (large-br (%brotli-fixture "large.txt.br"))
        (cyrillic (%brotli-fixture "cyrillic.txt"))
        (cyrillic-br (%brotli-fixture "cyrillic.br"))
        (cyrillic-q5-br (%brotli-fixture "cyrillic-q5.br"))
        (hebrew (%brotli-fixture "hebrew.txt"))
        (hebrew-br (%brotli-fixture "hebrew.br"))
        (arabic (%brotli-fixture "arabic.txt"))
        (arabic-br (%brotli-fixture "arabic.br"))
        (cjk (%brotli-fixture "cjk.txt"))
        (cjk-br (%brotli-fixture "cjk.br")))
    (labels ((crc32 (octets)
               (loop with crc = #xffffffff
                     for octet across octets
                     do (setf crc (logxor crc octet))
                        (dotimes (index 8)
                          (setf crc
                                (if (oddp crc)
                                    (logxor #xedb88320 (ash crc -1))
                                    (ash crc -1))))
                     finally (return (logxor crc #xffffffff))))
             (pack-bits (bits)
               (let ((octets
                       (make-array (ceiling (length bits) 8)
                                   :element-type '(unsigned-byte 8)
                                   :initial-element 0)))
                 (loop for bit in bits
                       for index from 0
                       do (setf (aref octets (floor index 8))
                                (logior (aref octets (floor index 8))
                                        (ash bit (mod index 8)))))
                 octets))
             (rewindow (compressed window-bits)
               (let ((reader (deflate-kit::%make-brotli-bit-reader compressed)))
                 (deflate-kit::%brotli-read-window-bits reader)
                 (let* ((consumed (- (* 8 (deflate-kit::brotli-bit-reader-position reader))
                                     (deflate-kit::brotli-bit-reader-bits reader)))
                        (prefix
                          ;; WBITS prefix bits in transmitted order (LSB-first): 10 -> 1000010,
                          ;; 16 -> 0, 18 -> 1100, and 24 -> 1111 (RFC 7932 §9.1).
                          (case window-bits
                            (10 '(1 0 0 0 0 1 0))
                            (16 '(0))
                            (18 '(1 1 0 0))
                            (24 '(1 1 1 1))
                            (otherwise
                             (error "Unsupported test window size: ~D"
                                    window-bits))))
                        (remaining
                          (loop repeat (- (* 8 (length compressed)) consumed)
                                collect (deflate-kit::%brotli-read-bits reader 1))))
                   (pack-bits (append prefix remaining)))))
             (check-invalid-reason (condition reason description)
               (%brotli-check
                (and (typep condition 'brotli-invalid-data)
                     (eq reason (inflate-error-reason condition)))
                description))
             (check-output-limit (condition expected-limit description)
               (%brotli-check (typep condition 'deflate-output-limit)
                              (format nil "~A inherits DEFLATE-OUTPUT-LIMIT"
                                      description))
               (%brotli-check (= expected-limit
                                 (deflate-output-limit-limit condition))
                              (format nil "~A reports its limit" description))
               (%brotli-check (eq :output-limit
                                  (inflate-error-reason condition))
                              (format nil "~A reports OUTPUT-LIMIT" description))
               (%brotli-check (> (inflate-error-observed condition)
                                 expected-limit)
                              (format nil "~A reports observed output" description))))
      (%brotli-check (zerop (length empty)) "empty fixture has zero output bytes")
      (%brotli-check (= #x100000 (length large))
                     "large fixture is exactly 1 MiB")
      (%brotli-check (= (* 16 1024 1024) deflate-kit::+brotli-default-max-output-bytes+)
                     "default Brotli output cap is 16 MiB")
      (%brotli-check
       (= (* 16 1024 1024)
          (deflate-kit::brotli-stream-max-input-bytes (make-brotli-stream)))
       "default Brotli stream input cap is 16 MiB")
      (%brotli-check-output (brotli-decode empty-br) empty
                            "empty CLI fixture decodes")
      (%brotli-check-output (brotli-decode simple-br) simple
                            "simple CLI fixture decodes")
      (%brotli-check-output (brotli-decode dictionary-br) dictionary
                            "dictionary CLI fixture decodes")
      (%brotli-check-output (brotli-decode large-br) large
                            "large CLI fixture decodes")
      (%brotli-check-output (brotli-decode cyrillic-br) cyrillic
                            "Cyrillic q11 CLI fixture decodes")
      (%brotli-check-output (brotli-decode cyrillic-q5-br) cyrillic
                            "Cyrillic q5 CLI fixture decodes")
      (%brotli-check-output (brotli-decode hebrew-br) hebrew
                            "Hebrew q11 CLI fixture decodes")
      (%brotli-check-output (brotli-decode arabic-br) arabic
                            "Arabic q11 CLI fixture decodes")
      (%brotli-check-output (brotli-decode cjk-br) cjk
                            "CJK q11 CLI fixture decodes")

      (dolist (width '(1 2 7 31 43))
        (%brotli-stream-decode-chunked simple-br simple width
                                       (format nil "simple stream width ~D" width)))
      (dolist (width '(1 3 17 53))
        (%brotli-stream-decode-chunked dictionary-br dictionary width
                                       (format nil "dictionary stream width ~D" width)))
      (dolist (width '(4093 65536))
        (%brotli-stream-decode-chunked large-br large width
                                       (format nil "large stream width ~D" width)))

      (let ((stream (make-brotli-stream))
            (buffer (%brotli-octets 0)))
        (loop for octet across simple-br
              for index from 0
              do (setf (aref buffer 0) octet)
                 (%brotli-check
                  (null (brotli-stream-push stream buffer))
                  (format nil "reused buffer push ~D returns NIL" index))
                 (setf (aref buffer 0) (logxor octet #xff)))
        (progn
          (%brotli-check
           (let ((input (deflate-kit::brotli-stream-input stream)))
             (and (= (fill-pointer input) (length simple-br))
                  (equalp (subseq input 0 (fill-pointer input)) simple-br)))
           "stream buffer snapshots each reused push buffer")
          (%brotli-check-output
           (brotli-stream-finish stream)
           simple
           "stream decodes bytes from its reusable input buffer")
          (let ((empty-stream (make-brotli-stream :max-input-bytes 0))
                (empty-buffer (%brotli-octets)))
            (loop repeat 128
                  do (brotli-stream-push empty-stream empty-buffer))
            (%brotli-check
             (let ((input (deflate-kit::brotli-stream-input empty-stream)))
               (and (zerop (fill-pointer input))
                    (zerop (length input))
                    (zerop (array-total-size input))))
             "repeated empty pushes keep input bytes and buffer length at zero"))))

      (let ((lut2 (make-array 256 :element-type '(unsigned-byte 8))))
        (dotimes (byte 256)
          (setf (aref lut2 byte) (deflate-kit::%brotli-signed-lut byte)))
        (%brotli-check
         (and (= 256 (length deflate-kit::+brotli-utf8-lut0+))
              (= #x8e91efb7 (crc32 deflate-kit::+brotli-utf8-lut0+)))
         "RFC 7932 section 7.1 UTF-8 LUT0 CRC-32")
        (%brotli-check
         (and (= 256 (length deflate-kit::+brotli-utf8-lut1+))
              (= #xd01a32f4 (crc32 deflate-kit::+brotli-utf8-lut1+)))
         "RFC 7932 section 7.1 UTF-8 LUT1 CRC-32")
        (%brotli-check
         (and (= 256 (length lut2))
              (= #x0dd7a0d6 (crc32 lut2)))
         "RFC 7932 section 7.1 UTF-8 LUT2 CRC-32"))
      (%brotli-check (= 43 (deflate-kit::%brotli-literal-context 0 #xab 0))
                     "LSB6 literal context uses the low six bits")
      (%brotli-check (= 42 (deflate-kit::%brotli-literal-context 1 #xab 0))
                     "MSB6 literal context uses the high six bits")
      (%brotli-check (= 51 (deflate-kit::%brotli-literal-context 2 #x41 #x61))
                     "UTF-8 literal context combines both RFC lookup tables")
      (%brotli-check (= 28 (deflate-kit::%brotli-literal-context 3 #x7f #x80))
                     "Signed literal context combines signed classes")

      (dolist (window-bits '(10 16 18 24))
        (%brotli-check-output
         (brotli-decode (rewindow simple-br window-bits))
         simple
         (format nil "full decode with WBITS ~D" window-bits)))

      (let* ((block-bits
               '(1 0 1 0 0 0 1 0
                 1 0 0 0 0 0 0 0 0
                 0 0
                 1
                 0 0))
             (reader (deflate-kit::%make-brotli-bit-reader (pack-bits block-bits)))
             (state (deflate-kit::%brotli-read-block-state reader 2)))
        (%brotli-check (= 0 (deflate-kit::%brotli-switch-block-if-needed reader state))
                       "multiple block types begin at type zero")
        (deflate-kit::%brotli-decrement-block-count state)
        (%brotli-check (= 1 (deflate-kit::%brotli-switch-block-if-needed reader state))
                       "multiple block types switch to type one"))

      (let* ((uncompressed
              (%brotli-octets #x20 #x00 #x10 #x41 #x42 #x43 #x03))
            (over-limit-uncompressed
              uncompressed)
            (metadata-and-uncompressed
              (%brotli-octets #x2c #x00 #x4d #x10 #x00 #x08 #x41 #x42 #x43 #x03))
            (distance-cache-seed
              (%brotli-octets #x82 #x00 #x00 #x00 #x44 #x50 #x28 #x12 #x10))
            (static-dictionary
              (%brotli-octets #x62 #x00 #x00 #x00 #x04 #x40 #x08 #x10 #x00)))
        (%brotli-check-output
         (brotli-decode uncompressed :unbounded t) (%octets "ABC")
         "unbounded decode accepts the valid uncompressed meta-block")
        (%brotli-check-output
         (let ((stream (make-brotli-stream :unbounded t)))
           (brotli-stream-push stream uncompressed)
           (brotli-stream-finish stream))
         (%octets "ABC")
         "unbounded stream accepts the valid uncompressed meta-block")
        (%brotli-check-output (brotli-decode uncompressed) (%octets "ABC")
                              "uncompressed meta-block followed by final empty block")
        (%brotli-check-output (brotli-decode metadata-and-uncompressed)
                              (%octets "ABC")
                              "RFC metadata meta-block followed by uncompressed and final empty blocks")
        (%brotli-check-output (brotli-decode distance-cache-seed)
                              (%octets "AAAAA")
                              "RFC one-symbol simple prefix trees decode an explicit distance")
        (%brotli-check-output (brotli-decode static-dictionary)
                              (%octets "left")
                              "one-shot decode resets distance history before static dictionary references")
        (let ((condition
                (%brotli-check-condition
                 'brotli-size-limit-exceeded
                 (lambda ()
                   (brotli-decode over-limit-uncompressed :max-output-bytes 2))
                 "over-limit uncompressed meta-block is rejected")))
          (check-output-limit condition 2
                              "over-limit uncompressed meta-block")))

      (let ((output (make-array 0 :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer 0)))
        (%brotli-check
         (= 5 (deflate-kit::%brotli-copy-dictionary-word output 4 4097 0 5 5))
         "static dictionary transform copies its transformed word")
        (%brotli-check-output
         output (%octets "Time ")
         "non-identity static dictionary transform uppercases and appends a space"))

      (let ((condition
              (%brotli-check-condition
               'brotli-invalid-data
               (lambda () (brotli-decode (%brotli-octets #x00)))
               "minimal truncated input is rejected")))
        (%brotli-check (typep condition 'deflate-error)
                       "Brotli invalid-data inherits DEFLATE-ERROR")
        (%brotli-check (eq :truncated-input (inflate-error-reason condition))
                       "truncated input exposes the shared error reason")
        (let* ((position (deflate-error-position condition))
               (detail (inflate-error-detail condition))
               (report (format nil "~A" condition)))
          (%brotli-check (integerp position)
                         "invalid-data exposes an integer decoder position")
          (%brotli-check (eql position detail)
                         "invalid-data exposes the shared error detail")
          (%brotli-check
           (and (search (format nil "byte ~D" position) report)
                (search (deflate-error-message condition) report))
           "invalid-data report includes position and message")))

      (%brotli-check-condition
       'brotli-invalid-data
       (lambda ()
         (brotli-decode
          (concatenate '(vector (unsigned-byte 8)) simple-br (%brotli-octets 0))))
       "trailing bytes are rejected")
      (%brotli-check-condition
       'brotli-invalid-data
       (lambda ()
         (brotli-decode (subseq simple-br 0 (1- (length simple-br)))))
       "truncated CLI stream is rejected")
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
               'brotli-invalid-data
               (lambda () (brotli-decode (%brotli-octets #x11)))
               "reserved window code is rejected")))
        (check-invalid-reason condition :reserved-window-size
                              "reserved window code reports its reason"))
      (let ((condition
              (%brotli-check-condition
               'brotli-invalid-data
               (lambda ()
                 (deflate-kit::%brotli-build-huffman-table (%brotli-octets 1 1 1)))
               "oversubscribed Huffman tree is rejected")))
        (check-invalid-reason condition :oversubscribed-tree
                              "oversubscribed tree reports its reason"))
      (let ((condition
              (%brotli-check-condition
               'brotli-invalid-data
               (lambda ()
                 (deflate-kit::%brotli-build-huffman-table (%brotli-octets 2 2)))
               "incomplete Huffman tree is rejected")))
        (check-invalid-reason condition :incomplete-tree
                              "incomplete tree reports its reason"))
      (let ((condition
              (%brotli-check-condition
               'brotli-invalid-data
               (lambda ()
                 ;; #x02 encodes RLEMAX=0, then a one-symbol Huffman tree whose only
                 ;; symbol is 0. All 64 entries decode to 0, violating RFC 7932 §7.3's
                 ;; requirement for NTREES=2 distinct map values.
                 (deflate-kit::%brotli-read-context-map
                  (deflate-kit::%make-brotli-bit-reader (%brotli-octets #x02)) 64 2))
               "context map that omits a tree is rejected")))
        (check-invalid-reason condition :invalid-context-map
                              "invalid context map reports its reason"))
      (let ((output (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0)))
        (let ((condition
                (%brotli-check-condition
                 'brotli-invalid-data
                 (lambda ()
                   (deflate-kit::%brotli-copy-reference output 1 1 10))
                 "backward distance before output is rejected")))
          (check-invalid-reason condition :distance-before-output
                                "invalid distance reports its reason"))
        (let* ((cache #(1 4 11 15))
               (distance (deflate-kit::%brotli-resolve-short-distance cache 4))
               ;; The stream makes two distance-1 copies, leaving cache
               ;; #(1 1 4 11). Distance symbol 10 resolves as cache[1]-1
               ;; (RFC 7932 §4), yielding 0. The direct helper assertion instead
               ;; checks code 4 against the initial cache.
               (zero-distance-stream
                 (%brotli-octets #x42 #x01 #x00 #x04 #x44 #x50
                                 #x29 #x00 #x48 #x29 #x20 #x3e)))
          (%brotli-check (zerop distance)
                         "short-distance cache code 4 resolves to zero")
          (let ((condition
                  (%brotli-check-condition
                   'brotli-invalid-data
                   (lambda () (brotli-decode zero-distance-stream))
                   "zero short-cache distance is rejected through decode")))
            (check-invalid-reason condition :invalid-distance
                                  "zero short-cache distance reports invalid-distance"))))

      (let ((condition
              (%brotli-check-condition
               'brotli-size-limit-exceeded
               (lambda () (brotli-decode simple-br :max-output 8))
               "one-shot output limit is enforced")))
        (check-output-limit condition 8 "one-shot :max-output"))
      (let ((condition
              (%brotli-check-condition
               'brotli-size-limit-exceeded
               (lambda () (brotli-decode simple-br :max-output-bytes 8))
               "one-shot :max-output-bytes limit is enforced")))
        (check-output-limit condition 8 "one-shot :max-output-bytes"))
      (let ((condition
              (%brotli-check-condition
               'brotli-size-limit-exceeded
               (lambda ()
                 (let ((stream (make-brotli-stream :max-output-bytes 8)))
                   (brotli-stream-push stream simple-br)
                   (brotli-stream-finish stream)))
               "stream output limit is enforced")))
        (check-output-limit condition 8 "stream :max-output-bytes"))
      (let ((condition
              (%brotli-check-condition
               'brotli-size-limit-exceeded
               (lambda ()
                 (let ((stream (make-brotli-stream :max-output 8)))
                   (brotli-stream-push stream simple-br)
                   (brotli-stream-finish stream)))
               "stream :max-output limit is enforced")))
        (check-output-limit condition 8 "stream :max-output"))

      (let ((condition
              (%brotli-check-condition
               'brotli-size-limit-exceeded
               (lambda ()
                 (let ((stream (make-brotli-stream :max-input-bytes 2)))
                   (brotli-stream-push stream (subseq simple-br 0 2))
                   (brotli-stream-push stream (subseq simple-br 2 5))))
               "stream input limit is enforced")))
        (%brotli-check (typep condition 'deflate-output-limit)
                       "Brotli size-limit inherits DEFLATE-OUTPUT-LIMIT")
        (%brotli-check (eq :input-limit (inflate-error-reason condition))
                       "input limit exposes the shared error reason")
        (%brotli-check
         (= 5 (inflate-error-observed condition))
         "input limit reports the full attempted input total")
        (%brotli-check (= 2 (deflate-error-position condition))
                       "input limit reports the first excess byte position")
        (%brotli-check
         (and (search "byte 2" (format nil "~A" condition))
              (search (deflate-error-message condition)
                      (format nil "~A" condition)))
         "input limit report includes position and message"))

      (let ((condition
              (%brotli-check-condition
               'brotli-invalid-data
               (lambda ()
                 (brotli-decode simple-br :max-output 8 :max-output-bytes 9))
               "one-shot output aliases conflict")))
        (check-invalid-reason condition :conflicting-output-limits
                              "one-shot output aliases conflict by contract"))
      (let ((condition
              (%brotli-check-condition
               'brotli-invalid-data
               (lambda ()
                 (brotli-decode simple-br :unbounded t :max-output 8))
               "one-shot unbounded and max-output conflict")))
        (check-invalid-reason condition :conflicting-output-limits
                              "one-shot unbounded and max-output conflict by contract"))
      (let ((condition
              (%brotli-check-condition
               'brotli-invalid-data
               (lambda ()
                 (brotli-decode simple-br :unbounded t :max-output-bytes 8))
               "one-shot unbounded and max-output-bytes conflict")))
        (check-invalid-reason condition :conflicting-output-limits
                              "one-shot unbounded and max-output-bytes conflict by contract"))
      (let ((condition
              (%brotli-check-condition
               'brotli-invalid-data
               (lambda ()
                 (make-brotli-stream :max-output 8 :max-output-bytes 9))
               "stream output aliases conflict")))
        (check-invalid-reason condition :conflicting-output-limits
                              "stream output aliases conflict by contract"))
      (let ((condition
              (%brotli-check-condition
               'brotli-invalid-data
               (lambda () (make-brotli-stream :unbounded t :max-output 8))
               "stream unbounded and max-output conflict")))
        (check-invalid-reason condition :conflicting-output-limits
                              "stream unbounded and max-output conflict by contract"))
      (let ((condition
              (%brotli-check-condition
               'brotli-invalid-data
               (lambda () (make-brotli-stream :unbounded t :max-output-bytes 8))
               "stream unbounded and max-output-bytes conflict")))
        (check-invalid-reason condition :conflicting-output-limits
                              "stream unbounded and max-output-bytes conflict by contract"))

      (unless (plusp *brotli-assertion-count*)
        (error "Brotli test suite ran no checks"))
      (format t "Brotli focused tests passed (~D checks; 3 LUT CRC assertions; 5 multilingual fixture assertions)~%"
              *brotli-assertion-count*)
      t)))
