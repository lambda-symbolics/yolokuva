(defpackage #:yolokuva
  (:documentation "Recognize, measure, size, and encode images for multimodal model prompts.")
  (:use #:cl)
  (:export
   ;; conditions
   #:image-error
   #:image-error-message
   #:image-error-pathname
   #:image-error-stage
   #:image-error-cause
   #:*image-error-class*
   ;; formats
   #:*image-inspection-prefix-octets*
   #:image-format
   #:image-dimensions
   #:image-inspect
   #:image-inspect-stream
   #:image-read-octets
   #:image-mime-type
   ;; limits
   #:*image-maximum-dimension*
   #:*image-patch-size*
   #:*image-maximum-patches*
   #:image-dimensions-fit-p
   #:image-target-dimensions
   #:image-preparation
   ;; data urls
   #:image-data-url
   ;; preparation through opticl
   #:image-write-prepared))
