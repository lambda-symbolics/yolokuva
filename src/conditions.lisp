(in-package #:yolokuva)

;;;; -- Conditions --

(define-condition image-error (error)
  ((message :initarg :message :reader image-error-message
            :documentation "A sentence describing the failure.")
   (pathname :initarg :pathname :initform nil :reader image-error-pathname
             :documentation "The image file involved, when there is one.")
   (stage :initarg :stage :reader image-error-stage
          :documentation "The stage that failed: :RECOGNITION, :DECODING, :RESIZING, or :WRITING.")
   (cause :initarg :cause :initform nil :reader image-error-cause
          :documentation "The underlying condition, when one was caught."))
  (:report (lambda (condition stream)
             (write-string (image-error-message condition) stream)))
  (:documentation "An image could not be recognized, measured, decoded, or prepared."))

(defparameter *image-error-class* 'image-error
  "The condition class yolokuva signals; a host may name a subclass with the same initargs.")

(defun image--fail (stage message &key pathname cause)
  "Signal *IMAGE-ERROR-CLASS* for STAGE with MESSAGE."
  (error *image-error-class* :message message :pathname pathname :stage stage :cause cause))
