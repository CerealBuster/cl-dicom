(defsystem "cl-dicom"
  :version "0.1.0"
  :author "Raffael Golomingi"
  :license "GPLv3"
  :depends-on ("cl-autowrap"
               "arrows"
               "cffi")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "A DICOM reader for Common Lisp"
  :in-order-to ((test-op (test-op "cl-dicom/tests"))))

(defsystem "cl-dicom/tests"
  :author "Raffael Golomingi"
  :license "GPLv3"
  :depends-on ("cl-dicom"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-dicom"
  :perform (test-op (op c) (symbol-call :rove :run c)))
