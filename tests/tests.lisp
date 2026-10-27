(defpackage #:yolokuva/tests
  (:use #:cl #:yolokuva)
  (:export #:run-tests))

(in-package #:yolokuva/tests)

(defvar *checks* 0
  "The number of assertions made by the current run.")

(defun check (value description)
  "Count one assertion and fail with DESCRIPTION unless VALUE is true."
  (incf *checks*)
  (unless value
    (error "yolokuva test failed: ~A" description)))

(defun octets (&rest values)
  "Return VALUES, numbers or ASCII strings, as one octet vector."
  (coerce (loop for value in values
                append (if (stringp value) (map 'list #'char-code value) (list value)))
          '(vector (unsigned-byte 8))))

(defun fail-stage (function)
  "Return the stage of the image error FUNCTION signals, or NIL."
  (handler-case (progn (funcall function) nil)
    (image-error (condition)
      (image-error-stage condition))))

(defparameter *tiny-png*
  (octets 137 80 78 71 13 10 26 10 0 0 0 13 "IHDR" 0 0 0 1 0 0 0 1 8 6 0 0 0 31 21 196 137
          0 0 0 13 "IDAT" 120 156 99 248 207 192 240 31 0 5 0 1 255 137 153 61 29
          0 0 0 0 "IEND" 174 66 96 130)
  "A valid one-pixel RGBA PNG.")

(defparameter *tiny-jpeg*
  (octets 255 216 255 192 0 11 8 0 2 0 3 1 1 17 0 255 217)
  "A JPEG whose frame header declares three by two pixels.")

(defun write-octets (pathname octets)
  "Write OCTETS to the file PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname :direction :output :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
    (write-sequence octets stream))
  pathname)

(defun test-recognition ()
  "Test format signatures and dimensions from leading octets."
  (check (equal (mapcar #'image-format
                        (list *tiny-png* *tiny-jpeg* (octets "GIF89a" 4 0 3 0)
                              (octets "RIFF" 0 0 0 0 "WEBP") (octets "plain text")))
                '(:png :jpeg :gif :webp nil))
         "every supported signature is recognized and others are not")
  (check (equal (multiple-value-list (image-dimensions :png *tiny-png*)) '(1 1))
         "PNG dimensions come from the header chunk")
  (check (equal (multiple-value-list (image-dimensions :jpeg *tiny-jpeg*)) '(3 2))
         "JPEG dimensions come from the frame header")
  (check (equal (multiple-value-list (image-dimensions :gif (octets "GIF89a" 4 0 3 0))) '(4 3))
         "GIF dimensions come from the screen descriptor")
  (check (equal (multiple-value-list
                 (image-dimensions :webp (octets "RIFF" 0 0 0 0 "WEBP" "VP8X" 0 0 0 0 0 0 0 0
                                                 99 0 0 49 0 0)))
                '(100 50))
         "extended WebP dimensions come from the canvas header")
  (check (equal (multiple-value-list (image-dimensions :png (octets 137 80 78 71))) '(0 0))
         "a truncated header gives no dimensions"))

(defun test-inspection ()
  "Test inspecting image files and refusing unusable ones."
  (let ((root (uiop:ensure-directory-pathname
               (merge-pathnames (format nil "yolokuva-tests-~D/" (random most-positive-fixnum))
                                (uiop:temporary-directory)))))
    (unwind-protect
         (let ((png       (write-octets (merge-pathnames "a.png" root) *tiny-png*))
               (jpeg      (write-octets (merge-pathnames "b.jpg" root) *tiny-jpeg*))
               (text      (write-octets (merge-pathnames "c.txt" root) (octets "not an image")))
               (empty-gif (write-octets (merge-pathnames "d.gif" root) (octets "GIF89a" 0 0 0 0))))
           (check (equal (multiple-value-list (image-inspect png)) '(:png 1 1))
                  "a PNG file inspects from its prefix")
           (check (equal (multiple-value-list (image-inspect jpeg)) '(:jpeg 3 2))
                  "a JPEG file inspects from its frame header")
           (check (equal (list (fail-stage (lambda () (image-inspect text)))
                               (fail-stage (lambda () (image-inspect empty-gif)))
                               (fail-stage (lambda () (image-inspect png :maximum-octets 10))))
                         '(:recognition :recognition :recognition))
                  "unsupported, dimensionless, and oversized files fail recognition")
           (check (equalp (image-read-octets png) *tiny-png*)
                  "a file's octets read back completely"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(defun test-limits ()
  "Test fitting dimensions within provider size limits."
  (check (equal (multiple-value-list (image-target-dimensions 800 600)) '(800 600))
         "an image within the limits keeps its size")
  (check (equal (multiple-value-list (image-target-dimensions 4096 1024)) '(2048 512))
         "a wide image is scaled to the longest permitted side")
  (check (equal (multiple-value-list (image-target-dimensions 2048 2048)) '(1600 1600))
         "an image covering too many patches is scaled to whole patches")
  (check (equal (multiple-value-list (image-target-dimensions 100 50 :maximum-dimension 20))
                '(20 10))
         "limits are keyword arguments")
  (check (equal (list (multiple-value-list (image-preparation :png 800 600))
                      (multiple-value-list (image-preparation :jpeg 4096 1024))
                      (multiple-value-list (image-preparation :gif 10 10))
                      (multiple-value-list (image-preparation :webp 10 10)))
                '((:png 800 600) (:png 2048 512) (:png 10 10) (:webp 10 10)))
         "fitting PNG and JPEG keep their format and everything else becomes PNG")
  (check (eq (fail-stage (lambda () (image-preparation :webp 4096 4096))) :resizing)
         "an oversized WebP image is refused"))

(defun test-data-urls ()
  "Test base64 data URLs, including both padding lengths."
  (check (equal (list (image-data-url (octets "Man") "image/png")
                      (image-data-url (octets "Ma") "image/png")
                      (image-data-url (octets "M") "image/png"))
                '("data:image/png;base64,TWFu" "data:image/png;base64,TWE="
                  "data:image/png;base64,TQ=="))
         "data URLs carry padded standard base64")
  (check (typep (image-data-url *tiny-png* "image/png") 'base-string)
         "data URLs are compact base strings"))

(defun test-preparation ()
  "Test copying fitting images and resizing others through opticl."
  (let ((root (uiop:ensure-directory-pathname
               (merge-pathnames (format nil "yolokuva-prepare-~D/" (random most-positive-fixnum))
                                (uiop:temporary-directory)))))
    (unwind-protect
         (let ((tiny   (write-octets (merge-pathnames "tiny.png" root) *tiny-png*))
               (large  (merge-pathnames "large.png" root))
               (broken (write-octets (merge-pathnames "broken.png" root)
                                     (subseq *tiny-png* 0 40))))
           (image-write-prepared tiny (merge-pathnames "copy.png" root)
                                 :source-format :png :output-format :png :width 1 :height 1)
           (check (equalp (image-read-octets (merge-pathnames "copy.png" root)) *tiny-png*)
                  "a fitting PNG is copied unchanged")
           (opticl:write-png-file large (opticl:make-8-bit-rgb-image 32 64))
           (multiple-value-bind (format width height)
               (image-preparation :png 64 32 :maximum-dimension 16)
             (image-write-prepared large (merge-pathnames "small.png" root)
                                   :source-format :png :output-format format
                                   :width width :height height))
           (check (equal (multiple-value-list (image-inspect (merge-pathnames "small.png" root)))
                         '(:png 16 8))
                  "an oversized image is resized to its target dimensions")
           (check (eq (fail-stage (lambda ()
                                    (image-write-prepared broken (merge-pathnames "x.png" root)
                                                          :source-format :png :output-format :png
                                                          :width 1 :height 1)))
                      :decoding)
                  "an image that does not decode fails at the decoding stage")
           (check (eq (fail-stage (lambda ()
                                    (image-write-prepared tiny (merge-pathnames "x.jpeg" root)
                                                          :source-format :png :output-format :jpeg
                                                          :width 1 :height 1)))
                      :resizing)
                  "an output format other than PNG fails at the resizing stage"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(defun run-tests ()
  "Run every yolokuva test and return true."
  (setf *checks* 0)
  (test-recognition)
  (test-inspection)
  (test-limits)
  (test-data-urls)
  (test-preparation)
  (format t "~&~D yolokuva checks passed.~%" *checks*)
  t)
