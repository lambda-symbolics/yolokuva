(in-package #:yolokuva)

;;;; -- Recognizing Formats --

(deftype octets ()
  "A vector of eight-bit octets."
  '(vector (unsigned-byte 8)))

(defun image--octets-at-p (octets start expected)
  "Return true when OCTETS holds the octet list EXPECTED beginning at START."
  (and (<= (+ start (length expected)) (length octets))
       (loop for octet in expected
             for index from start
             always (= (aref octets index) octet))))

(defun image--ascii-at-p (octets start expected)
  "Return true when OCTETS holds the ASCII string EXPECTED beginning at START."
  (image--octets-at-p octets start (map 'list #'char-code expected)))

(defun image--little-endian (octets start count)
  "Decode COUNT little-endian octets of OCTETS beginning at START."
  (loop for index from start below (+ start count)
        for shift from 0 by 8
        sum (ash (aref octets index) shift)))

(defun image--big-endian (octets start count)
  "Decode COUNT big-endian octets of OCTETS beginning at START."
  (loop with value = 0
        for index from start below (+ start count)
        do (setf value (+ (ash value 8) (aref octets index)))
        finally (return value)))

(defun image-format (octets)
  "Return the image format OCTETS begin with: :PNG, :JPEG, :GIF, :WEBP, or NIL."
  (cond
    ((image--octets-at-p octets 0 '(137 80 78 71 13 10 26 10))
     :png)
    ((image--octets-at-p octets 0 '(255 216 255))
     :jpeg)
    ((or (image--ascii-at-p octets 0 "GIF87a") (image--ascii-at-p octets 0 "GIF89a"))
     :gif)
    ((and (image--ascii-at-p octets 0 "RIFF") (image--ascii-at-p octets 8 "WEBP"))
     :webp)
    (t
     nil)))

(defun image--jpeg-dimensions (octets)
  "Return the width and height in a JPEG's first frame header, or 0 and 0."
  (let ((position 2)
        (length   (length octets)))
    (loop while (< (+ position 3) length)
          do (if (/= (aref octets position) #xff)
                 (incf position)
                 (progn
                   (loop while (and (< position length) (= (aref octets position) #xff))
                         do (incf position))
                   (when (>= position length)
                     (return))
                   (let ((marker (aref octets position)))
                     (incf position)
                     (cond
                       ((or (member marker '(#xd8 #xd9 #x01)) (<= #xd0 marker #xd7))
                        nil)
                       ((>= (+ position 1) length)
                        (return))
                       (t
                        (let ((segment-length (image--big-endian octets position 2)))
                          (when (or (< segment-length 2) (> (+ position segment-length) length))
                            (return))
                          (when (and (member marker '(#xc0 #xc1 #xc2 #xc3 #xc5 #xc6 #xc7
                                                      #xc9 #xca #xcb #xcd #xce #xcf))
                                     (<= (+ position 6) length))
                            (return-from image--jpeg-dimensions
                              (values (image--big-endian octets (+ position 5) 2)
                                      (image--big-endian octets (+ position 3) 2))))
                          (incf position segment-length))))))))
    (values 0 0)))

(defun image--webp-dimensions (octets)
  "Return a WebP image's width and height from its leading OCTETS, or 0 and 0."
  (cond
    ((image--ascii-at-p octets 12 "VP8X")
     (if (>= (length octets) 30)
         (values (1+ (image--little-endian octets 24 3))
                 (1+ (image--little-endian octets 27 3)))
         (values 0 0)))
    ((image--ascii-at-p octets 12 "VP8L")
     (if (and (>= (length octets) 25) (= (aref octets 20) #x2f))
         (let ((b0 (aref octets 21))
               (b1 (aref octets 22))
               (b2 (aref octets 23))
               (b3 (aref octets 24)))
           (values (1+ (logior b0 (ash (logand b1 #x3f) 8)))
                   (1+ (logior (ash b1 -6) (ash b2 2) (ash (logand b3 #x0f) 10)))))
         (values 0 0)))
    ((and (image--ascii-at-p octets 12 "VP8 ")
          (image--octets-at-p octets 23 '(157 1 42))
          (>= (length octets) 30))
     (values (logand (image--little-endian octets 26 2) #x3fff)
             (logand (image--little-endian octets 28 2) #x3fff)))
    (t
     (values 0 0))))

(defun image-dimensions (format octets)
  "Return FORMAT's pixel width and height from OCTETS, or 0 and 0 when they are not there.

The first *IMAGE-INSPECTION-PREFIX-OCTETS* octets suffice for every format but
JPEG, whose frame header can follow arbitrarily large metadata."
  (ecase format
    (:png
     (if (>= (length octets) 24)
         (values (image--big-endian octets 16 4) (image--big-endian octets 20 4))
         (values 0 0)))
    (:gif
     (if (>= (length octets) 10)
         (values (image--little-endian octets 6 2) (image--little-endian octets 8 2))
         (values 0 0)))
    (:jpeg
     (image--jpeg-dimensions octets))
    (:webp
     (image--webp-dimensions octets))))

(defun image-mime-type (format)
  "Return the media type of image FORMAT."
  (ecase format
    (:png
     "image/png")
    (:jpeg
     "image/jpeg")
    (:gif
     "image/gif")
    (:webp
     "image/webp")))


;;;; -- Inspecting Files --

(defparameter *image-inspection-prefix-octets* 30
  "The leading octets that identify every format and give every non-JPEG size.")

(defun image--read-rest (stream prefix)
  "Return PREFIX followed by every remaining octet of STREAM."
  (let ((result (make-array (length prefix) :element-type '(unsigned-byte 8)
                                            :adjustable t :fill-pointer t))
        (buffer (make-array 65536 :element-type '(unsigned-byte 8))))
    (replace result prefix)
    (loop for count = (read-sequence buffer stream)
          while (plusp count)
          do (loop for index below count
                   do (vector-push-extend (aref buffer index) result)))
    result))

(defun image-inspect-stream (stream &key pathname)
  "Return the format, width, and height of the image an octet STREAM holds.

Only a JPEG is read past its first *IMAGE-INSPECTION-PREFIX-OCTETS* octets.
PATHNAME labels failures. An unsupported format or missing dimensions signal
*IMAGE-ERROR-CLASS* at the :RECOGNITION stage."
  (let* ((prefix (make-array *image-inspection-prefix-octets*
                             :element-type '(unsigned-byte 8)))
         (count  (read-sequence prefix stream))
         (octets (subseq prefix 0 count))
         (format (image-format octets))
         (label  (or pathname "The image")))
    (unless format
      (image--fail :recognition (format nil "~A is not a PNG, JPEG, GIF, or WebP image." label)
                   :pathname    pathname))
    (when (eq format :jpeg)
      (setf octets (image--read-rest stream octets)))
    (multiple-value-bind (width height) (image-dimensions format octets)
      (unless (and (plusp width) (plusp height))
        (image--fail :recognition (format nil "~A has no valid pixel dimensions." label)
                     :pathname    pathname))
      (values format width height))))

(defun image--open-checked (pathname maximum-octets function)
  "Call FUNCTION with an octet stream on PATHNAME and its length, refusing files above MAXIMUM-OCTETS."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let ((length (file-length stream)))
      (when (and maximum-octets (> length maximum-octets))
        (image--fail :recognition
                     (format nil "~A is larger than the ~:D-byte image limit." pathname maximum-octets)
                     :pathname pathname))
      (funcall function stream length))))

(defun image-inspect (pathname &key maximum-octets)
  "Return the format, width, and height of the image file PATHNAME.

A file larger than MAXIMUM-OCTETS, when given, is refused before reading."
  (image--open-checked pathname maximum-octets
                       (lambda (stream length)
                         (declare (ignore length))
                         (image-inspect-stream stream :pathname pathname))))

(defun image-read-octets (pathname &key maximum-octets)
  "Return every octet of the file PATHNAME, refusing one larger than MAXIMUM-OCTETS."
  (image--open-checked pathname maximum-octets
                       (lambda (stream length)
                         (let ((octets (make-array length :element-type '(unsigned-byte 8))))
                           (unless (= (read-sequence octets stream) length)
                             (image--fail :recognition
                                          (format nil "~A could not be read completely." pathname)
                                          :pathname pathname))
                           octets))))
