(require :asdf)

(pushnew (uiop:pathname-directory-pathname
          (truename (merge-pathnames "../README.md" *load-truename*)))
         asdf:*central-registry* :test #'equal)
(asdf:load-system "cl-deflate-kit")

(defparameter *aitools-root*
  (let ((root (uiop:getenv "AITOOOLS_ROOT")))
    (unless (and root (plusp (length root)))
      (error "Set AITOOOLS_ROOT to the aitools checkout at revision 1b20c3ab."))
    (uiop:ensure-directory-pathname (truename root))))
(defparameter *benchmark-fasl-prefix*
  (format nil "cl-deflate-kit-~36R-~36R-"
          (get-universal-time) (get-internal-real-time)))

(defun %compile-and-load-aitools-file (relative name)
  (let* ((source (merge-pathnames relative *aitools-root*))
         (output (merge-pathnames
                  (format nil "~A~A.fasl" *benchmark-fasl-prefix* name)
                  (uiop:temporary-directory))))
    (unless (probe-file source)
      (error "The aitools benchmark source is missing: ~A" relative))
    (multiple-value-bind (fasl warnings failure)
        (compile-file source :output-file output)
      (declare (ignore warnings))
      (when failure
        (error "Could not compile the aitools benchmark source: ~A" relative))
      (load fasl))))

(%compile-and-load-aitools-file
 "packages/core/text/src/domain/package.lisp" "package")
(%compile-and-load-aitools-file
 "packages/core/text/src/domain/binary.lisp" "binary")
(%compile-and-load-aitools-file
 "packages/core/text/src/domain/codec-conditions.lisp" "codec-conditions")
(%compile-and-load-aitools-file
 "packages/core/text/src/domain/codec-crc32.lisp" "codec-crc32")
(%compile-and-load-aitools-file
 "packages/core/text/src/domain/codec-deflate.lisp" "codec-deflate")

(defun %random-octets (size)
  (let ((output (make-array size :element-type '(unsigned-byte 8)))
        (state 2463534242))
    (loop for index below size
          do (setf state (logand #xffffffff
                                 (+ (* state 1664525) 1013904223))
                   (aref output index) (ldb (byte 8 24) state)))
    output))

(defun %text-octets (size)
  (let* ((text (concatenate 'string
                            "The deflate kit reads a bounded byte stream and "
                            "preserves the surrounding gzip member metadata. "
                            "A decoder checks each back-reference, checksum, "
                            "and output limit before returning the result. "))
         (output (make-array size :element-type '(unsigned-byte 8))))
    (loop for index below size
          do (setf (aref output index)
                   (char-code (char text (mod index (length text)))))
          finally (return output))))

(defun %seconds (function input)
  (sb-ext:gc :full t)
  (let ((start (get-internal-real-time)))
    (funcall function input)
    (/ (float (- (get-internal-real-time) start) 1.0)
       internal-time-units-per-second)))

(defun %median (values)
  (nth (floor (length values) 2) (sort (copy-list values) #'<)))

(defun %paired-samples (kit-function aitools-function input rounds)
  (let ((kit-times nil)
        (aitools-times nil)
        (kit-wins 0))
    (dotimes (index rounds)
      (let ((kit-time nil)
            (aitools-time nil))
        (if (evenp index)
            (setf kit-time (%seconds kit-function input)
                  aitools-time (%seconds aitools-function input))
            (setf aitools-time (%seconds aitools-function input)
                  kit-time (%seconds kit-function input)))
        (push kit-time kit-times)
        (push aitools-time aitools-times)
        (when (< kit-time aitools-time)
          (incf kit-wins))))
    (values (%median kit-times) (%median aitools-times) kit-wins)))

(defun %aa-noise (function input pairs)
  (let ((noise nil))
    (dotimes (index pairs)
      (declare (ignore index))
      (let* ((first (%seconds function input))
             (second (%seconds function input)))
        (push (* 100.0 (/ (abs (- first second)) (min first second))) noise)))
    (values (%median noise) (apply #'max noise))))

(defun %run-case (payload-name operation input aitools-compressed)
  (let* ((kit-function
           (ecase operation
             (:deflate #'deflate-kit:deflate)
             (:inflate #'deflate-kit:inflate)
             (:crc32 #'deflate-kit:crc32)))
         (aitools-function
           (ecase operation
             (:deflate #'aitools.text.domain:deflate)
             (:inflate #'aitools.text.domain:inflate)
             (:crc32 #'aitools.text.domain:crc32)))
         (measured-input (if (eq operation :inflate)
                             aitools-compressed
                             input)))
    (funcall kit-function measured-input)
    (funcall aitools-function measured-input)
    (sb-ext:gc :full t)
    (multiple-value-bind (kit-median aitools-median wins)
        (%paired-samples kit-function aitools-function measured-input 7)
      (multiple-value-bind (kit-aa-median kit-aa-max)
          (%aa-noise kit-function measured-input 5)
        (multiple-value-bind (aitools-aa-median aitools-aa-max)
            (%aa-noise aitools-function measured-input 5)
          (format t
                  "~A ~A kit=~,3Fms aitools=~,3Fms speedup=~,3fx kit-wins=~D/7 A/A-kit=~,2F/~,2F% A/A-aitools=~,2F/~,2F%~%"
                  payload-name operation (* 1000 kit-median)
                  (* 1000 aitools-median)
                  (/ aitools-median kit-median) wins
                  kit-aa-median kit-aa-max
                  aitools-aa-median aitools-aa-max))))))

(let* ((size (* 2 1024 1024))
       (payloads (list (cons "high-entropy" (%random-octets size))
                       (cons "text" (%text-octets size)))))
  (format t "Platform ~A ~A; SBCL ~A; input=~D bytes; pairs=7; A/A pairs=5 per arm.~%"
          (software-type) (machine-type) (lisp-implementation-version) size)
  (dolist (payload payloads)
    (let* ((name (car payload))
           (input (cdr payload))
           (kit-compressed (deflate-kit:deflate input :level 6))
           (aitools-compressed (aitools.text.domain:deflate input)))
      (assert (equalp input (deflate-kit:inflate kit-compressed :unbounded t)))
      (assert (equalp input (aitools.text.domain:inflate aitools-compressed)))
      (assert (= (deflate-kit:crc32 input)
                 (aitools.text.domain:crc32 input)))
      (dolist (operation '(:deflate :inflate :crc32))
        (%run-case name operation input aitools-compressed)))))
