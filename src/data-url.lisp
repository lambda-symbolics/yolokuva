(in-package #:yolokuva)

;;;; -- Data URLs --

(defparameter *image-base64-alphabet*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "The standard base64 alphabet.")

(defun image--write-base64 (octets stream)
  "Write OCTETS to STREAM as padded standard base64."
  (loop for index from 0 below (length octets) by 3
        for remaining = (- (length octets) index)
        for group = (logior (ash (aref octets index) 16)
                            (if (> remaining 1) (ash (aref octets (+ index 1)) 8) 0)
                            (if (> remaining 2) (aref octets (+ index 2)) 0))
        do (loop for position from 0 below 4
                 for shift from 18 downto 0 by 6
                 do (write-char (if (<= position (min remaining 3))
                                    (char *image-base64-alphabet* (ldb (byte 6 shift) group))
                                    #\=)
                                stream))))

(defun image-data-url (octets mime-type)
  "Return OCTETS as a base64 data URL of MIME-TYPE, as a base string."
  (with-output-to-string (stream nil :element-type 'base-char)
    (write-string "data:" stream)
    (write-string mime-type stream)
    (write-string ";base64," stream)
    (image--write-base64 octets stream)))
