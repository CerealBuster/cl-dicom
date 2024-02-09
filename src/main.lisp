(defpackage cl-dicom
  (:use :cl))

(in-package :cl-dicom)

(defun read-raw (p)
  (with-open-file (stream (asdf:system-relative-pathname :cl-dicom p)
                          :element-type '(unsigned-byte 8))
    (let ((file-content (make-array (file-length stream)
                                    :element-type '(unsigned-byte  8))))
      (read-sequence file-content stream)
      file-content)))

(defun make-16-bit-number (byte1 byte2 little-endian-p)
  (if little-endian-p
      (+ (ash byte2 8) byte1)
      (+ (ash byte1 8) byte2)))

(defun make-32-bit-number (byte1 byte2 byte3 byte4 little-endian-p )
    (if little-endian-p
        (+ (* byte4 16777216) (* byte3 65536) (* byte2 256) byte1)
        (+ (* byte1 16777216) (* byte2 65536) (* byte3 256) byte4)))

(defun get-tag (l little-endian-p)
  (let ((group-nr (apply #'make-16-bit-number (append (subseq l 0 2)
                                                      '(little-endian-p))))
        (element-nr (apply #'make-16-bit-number (append (subseq l 2 4)
                                                        '(little-endian-p)))))
    (logior (ash group-nr 16) element-nr)))

(defun get-value-representation (l)
  (map 'string #'code-char l))

(defun special-vr-p (vr)
  (some (lambda (s) (equal vr s)) '("OB" "OW" "OF" "SQ" "UN")))

(defun handle-value (tag vr vl c)
              (cond ((= tag #x7FE00010) (subseq c 12))
                    ((equal vr "SQ") (parse-data-item (subseq c 12 (+ 12 vl))))
                    ((equal vr "US") (apply #'make-16-bit-number
                                            (append (coerce (subseq c 8 (+ 8 vl)) 'list) '(t))))

                    ((equal vr "UL") (apply #'make-32-bit-number
                                            (append (coerce (subseq c 8 (+ 8 vl)) 'list) '(t))))
                    ((equal vr "OB") (arrows:as-> (subseq c 12 (+ 12 vl)) s
                                                 (coerce s 'list)
                                                 (format nil "~{ #b~4,'0b~}" s)))
                    ((special-vr-p vr)(arrows:->> (subseq c 12 (+ 12 vl))
                                                  (map 'string #'code-char)
                                                  (remove-if (lambda (c) (equal c #\^@)))
                                                  (string-trim " ")))
                    (t (arrows:->> (subseq c 8 (+ 8 vl))
                                   (map 'string #'code-char)
                                   (remove-if (lambda (c) (equal c #\^@)))
                                   (string-trim " ")))))

(defun get-value-length (l 32-bit-p little-endian-p)
  (if 32-bit-p
      (apply #'make-32-bit-number (append l '(little-endian-p)))
      (apply #'make-16-bit-number (append l '(little-endian-p)))))

(defun parse-data-element (l)
  (let* ((tag (get-tag (coerce (subseq l 0 4) 'list) t))
         (vr (get-value-representation (subseq l 4 6)))
         (vl (if (special-vr-p vr)
                 (get-value-length (coerce (subseq l 8 12) 'list) t t)
                 (get-value-length (coerce (subseq l 6 8) 'list) nil t)))
         (value (handle-value tag vr vl l )))
    (list :tag tag
          :vr vr
          :vl vl
          :value value)))

(defun parse-data-item (l)
  (let ((items '())
        (data-element-tag (get-tag (coerce (subseq l 0 4) 'list) t))
        (data-item-length (get-value-length  (coerce (subseq l 4 8) 'list) t nil)))
    (labels ((parse (c)
               (unless (or (null c) (= 0 (length c)))
                 (let* ((item (parse-data-element c))
                        (item-vr (getf item :vr))
                        (item-vl (getf item :vl)))
                   (push item items)
                   (parse (if (special-vr-p item-vr)
                              (subseq c (+ 12 item-vl))
                              (subseq c (+ 8 item-vl))))))))
      (parse (subseq l 8)))
   (nreverse items)))

(defun parse-dicom (l)
  (let ((data-items '()))
    (labels ((parse (c)
               (unless (or (null c) (= 0 (length c))
                           (let* ((data-item (parse-data-element c))
                                  (tag (getf data-item :tag))
                                  (vr (getf data-item :vr))
                                  (vl (getf data-item :vl)))
                             (push data-item data-items)
                             (parse (cond ((= tag #x7FE00010) '())
                                          ((special-vr-p vr)(subseq c (+ 12 vl)))
                                          (t (subseq c (+ 8 vl))))))))))
      (parse (subseq l 132)))
    (nreverse data-items)))

(defun read-dicom-file (p)
  (arrows:->> p
              (read-raw)
              (parse-dicom)))
