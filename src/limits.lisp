(in-package #:yolokuva)

;;;; -- Provider Size Limits --

;;; Providers bound a prompt image by its longest side and by how many square
;;; patches it covers. The defaults are the OpenAI high-detail limits Codex
;;; uses; pass other limits as keyword arguments.

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

An image already within them keeps its size. A larger one is first scaled to
the longest side, then, when it still covers too many patches, by the square
root of the patch excess, rounded down to whole patches."
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
  "Return the output format, width, and height that make an image fit the size limits.

A PNG or JPEG within the limits is kept as it is; anything else becomes a PNG
of the target dimensions. A WebP image is kept when it fits, and refused at
the :RESIZING stage otherwise, because it cannot be decoded here. LIMITS are
the keyword arguments of IMAGE-TARGET-DIMENSIONS; PATHNAME labels failures."
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
