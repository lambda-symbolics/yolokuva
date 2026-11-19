(in-package #:yolokuva)

;;;; -- Provider Size Limits --

;;; Providers generally bound images by their longest side and by how many square
;;; patches they cover. The defaults are the OpenAI high-detail limits Codex
;;; uses, you can pass other limits as keyword arguments.

(defparameter *image-maximum-dimension* 2048
  "The default longest permitted image side in pixels.")

(defparameter *image-patch-size* 32
  "The default side of one square image token patch in pixels.")

(defparameter *image-maximum-patches* 2500
  "The default largest permitted number of image token patches.")

(defun image-dimensions-fit-p (width height &key (maximum-dimension *image-maximum-dimension*)
                                                 (patch-size *image-patch-size*)
                                                 (maximum-patches *image-maximum-patches*))
  "Return true when a WIDTH by HEIGHT image is within the size limits."
  (and (<= width maximum-dimension)
       (<= height maximum-dimension)
       (<= (* (ceiling width patch-size) (ceiling height patch-size)) maximum-patches)))

(defun image-target-dimensions (width height &key (maximum-dimension *image-maximum-dimension*)
                                                  (patch-size *image-patch-size*)
                                                  (maximum-patches *image-maximum-patches*))
  "Return the largest width and height within the size limits that keep WIDTH:HEIGHT.

Images larger than the limit are first scaled to the longest side, then, if 
they still cover too many patches, by the square root of the patch excess,
rounded down to whole patches."
  (flet ((fits-p (width height)
           (image-dimensions-fit-p width height :maximum-dimension maximum-dimension
                                                :patch-size        patch-size
                                                :maximum-patches   maximum-patches)))
    (if (fits-p width height)
        (values width height)
        (let* ((side-scale    (min 1.0d0 (/ maximum-dimension (coerce (max width height) 'double-float))))
               (scaled-width  (max 1 (round (* width side-scale))))
               (scaled-height (max 1 (round (* height side-scale)))))
          (if (fits-p scaled-width scaled-height)
              (values scaled-width scaled-height)
              (let* ((patch        (coerce patch-size 'double-float))
                     (scale        (sqrt (/ (* patch patch maximum-patches)
                                            (* (coerce scaled-width 'double-float) scaled-height))))
                     (patches-wide (/ (* scaled-width scale) patch))
                     (patches-high (/ (* scaled-height scale) patch))
                     (adjusted     (* scale (min (/ (floor patches-wide) patches-wide)
                                                 (/ (floor patches-high) patches-high)))))
                (values (max 1 (floor (* scaled-width adjusted)))
                        (max 1 (floor (* scaled-height adjusted))))))))))

(defun image-preparation (format width height &rest limits &key pathname &allow-other-keys)
  "Return the output format, width, and height that make an image fit
into the size limits.

PNGs and JPEGs within the limits are left untouched, other images are turned into PNGs
with target dimensions.
WebP images are kept if they fit, but refused at the :RESIZING stage otherwise, because
we cannot decode them here yet.

LIMITS are the keyword arguments of IMAGE-TARGET-DIMENSIONS; failures are labeled by PATHNAME."
  (multiple-value-bind (target-width target-height)
      (apply #'image-target-dimensions width height
             (loop for (key value) on limits by (lambda (plist) (nthcdr 2 plist))
                   unless (eq key :pathname)
                     append (list key value)))
    (let ((fits-p (and (= width target-width) (= height target-height))))
      (cond
        ((eq format :webp)
         (unless fits-p
           (image--fail :resizing
                        (format nil "WebP image ~A is ~Dx~D; use one within the prompt image limits."
                                (or pathname "") width height)
                        :pathname pathname))
         (values :webp width height))
        ((and fits-p (member format '(:png :jpeg)))
         (values format width height))
        (t
         (values :png target-width target-height))))))
