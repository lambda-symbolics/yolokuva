(in-package #:yolokuva)

;;;; -- Preparing Images through opticl --

(defun image--decode (pathname format)
  "Decode the FORMAT image file PATHNAME through opticl."
  (handler-case
      (with-open-file (stream pathname :element-type '(unsigned-byte 8))
        (opticl:read-image-stream stream (ecase format
                                           (:png "png")
                                           (:jpeg "jpeg")
                                           (:gif "gif"))))
    (error (condition)
      (image--fail :decoding (format nil "Image ~A could not be decoded: ~A" pathname condition)
                   :pathname pathname :cause condition))))

(defun image--eight-bit (image)
  "Return decoded IMAGE in an eight-bit representation PNG can store."
  (etypecase image
    (opticl:rgba-image (opticl:coerce-image image 'opticl:8-bit-rgba-image))
    (opticl:rgb-image (opticl:coerce-image image 'opticl:8-bit-rgb-image))
    (opticl:gray-alpha-image (opticl:coerce-image image 'opticl:8-bit-gray-alpha-image))
    (opticl:gray-image (opticl:coerce-image image 'opticl:8-bit-gray-image))))

(defun image-write-prepared (source output &key source-format output-format width height)
  "Write the prompt-ready form of image file SOURCE to the file OUTPUT.

SOURCE-FORMAT is what IMAGE-INSPECT reported; OUTPUT-FORMAT, WIDTH, and HEIGHT
are what IMAGE-PREPARATION chose. An image that keeps its format and size is
checked to decode and then copied unchanged; otherwise it is decoded, resized
when needed, and written as an eight-bit PNG. A WebP image is only ever copied."
  (if (eq source-format :webp)
      (uiop:copy-file source output)
      (let* ((decoded (image--decode source source-format))
             (same-size-p (and (= (array-dimension decoded 1) width)
                               (= (array-dimension decoded 0) height))))
        (cond
          ((and same-size-p (eq output-format source-format))
           (uiop:copy-file source output))
          ((eq output-format :png)
           (opticl:write-png-file
            output
            (image--eight-bit
             (if same-size-p
                 decoded
                 (opticl:resize-image decoded height width :interpolate :bilinear)))))
          (t
           (error "Cannot prepare a ~A image as ~A." source-format output-format)))))
  output)
