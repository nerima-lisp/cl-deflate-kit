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
(define-condition deflate-input-limit (deflate-error)
  ((limit :initarg :limit :reader deflate-input-limit-limit)))

(define-condition inflate-invalid-data (deflate-error)
  ((reason :initarg :reason :reader inflate-error-reason)
   (detail :initarg :detail :initform nil :reader inflate-error-detail)))

(define-condition inflate-size-limit-exceeded (deflate-output-limit)
  ((reason :initarg :reason :reader inflate-error-reason)
   (observed :initarg :observed :reader inflate-error-observed)))
(define-condition zstd-invalid-data (deflate-error)
  ((reason :initarg :reason :reader inflate-error-reason)
   (detail :initarg :detail :initform nil :reader inflate-error-detail)
   (observed :initarg :observed :initform nil :reader inflate-error-observed)))
(define-condition zstd-size-limit-exceeded (deflate-output-limit)
  ((reason :initarg :reason :reader inflate-error-reason)
   (observed :initarg :observed :reader inflate-error-observed))
  (:report (lambda (condition stream)
             (format stream "Zstandard size limit exceeded~@[ at byte ~D~]."
                     (deflate-error-position condition)))))
(define-condition zstd-input-limit-exceeded (deflate-input-limit)
  ((reason :initarg :reason :reader inflate-error-reason)
   (observed :initarg :observed :reader inflate-error-observed))
  (:report (lambda (condition stream)
             (format stream "Zstandard input size limit exceeded~@[ at byte ~D~]."
                     (deflate-error-position condition)))))

(export '(deflate-error deflate-error-message deflate-error-position
          deflate-output-limit deflate-output-limit-limit
          deflate-input-limit deflate-input-limit-limit
          inflate-invalid-data inflate-error-reason inflate-error-detail
          inflate-size-limit-exceeded inflate-error-observed
          zstd-invalid-data zstd-size-limit-exceeded zstd-input-limit-exceeded))
