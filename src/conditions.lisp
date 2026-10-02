(in-package #:deflate-kit)

(define-condition deflate-error (error)
  ((message :initarg :message :reader deflate-error-message)
   (position :initarg :position :initform nil :reader deflate-error-position))
  (:report (lambda (condition stream)
             (format stream "DEFLATE error~@[ at byte ~D~]: ~A"
                     (deflate-error-position condition)
                     (deflate-error-message condition)))))

(define-condition deflate-output-limit (deflate-error)
  ((limit :initarg :limit :reader deflate-output-limit-limit)))

(define-condition inflate-invalid-data (deflate-error)
  ((reason :initarg :reason :reader inflate-error-reason)
   (detail :initarg :detail :initform nil :reader inflate-error-detail)))

(define-condition inflate-size-limit-exceeded (deflate-output-limit)
  ((reason :initarg :reason :reader inflate-error-reason)
   (observed :initarg :observed :reader inflate-error-observed)))

(export '(deflate-error deflate-error-message deflate-error-position
          deflate-output-limit deflate-output-limit-limit
          inflate-invalid-data inflate-error-reason inflate-error-detail
          inflate-size-limit-exceeded inflate-error-observed))
