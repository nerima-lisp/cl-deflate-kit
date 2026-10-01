(in-package #:deflate-kit)
(define-condition deflate-error (error)
  ((message :initarg :message :reader deflate-error-message)
   (position :initarg :position :initform nil :reader deflate-error-position))
  (:report (lambda (c s) (format s "DEFLATE error~@[ at byte ~D~]: ~A"
                                  (deflate-error-position c)
                                  (deflate-error-message c)))))
(define-condition deflate-output-limit (deflate-error)
  ((limit :initarg :limit :reader deflate-output-limit-limit))
  (:report (lambda (c s) (format s "DEFLATE output exceeds limit ~D"
                                  (deflate-output-limit-limit c)))))

(define-condition inflate-invalid-data (deflate-error)
  ((reason :initarg :reason :reader inflate-error-reason)
   (detail :initarg :detail :initform nil :reader inflate-error-detail))
  (:report (lambda (c s)
             (format s "Invalid DEFLATE data~@[ (~A)~]: ~A"
                     (inflate-error-reason c)
                     (deflate-error-message c)))))

(define-condition inflate-size-limit-exceeded (deflate-output-limit)
  ((reason :initarg :reason :reader inflate-error-reason)
   (observed :initarg :observed :reader inflate-error-observed))
  (:report (lambda (c s)
             (format s "DEFLATE output exceeds limit ~D (observed ~D)"
                     (deflate-output-limit-limit c)
                     (inflate-error-observed c)))))

(export '(inflate-invalid-data inflate-size-limit-exceeded
          inflate-error-reason inflate-error-detail inflate-error-observed))
(defun fail (message &optional position)
  (error 'deflate-error :message message :position position))
