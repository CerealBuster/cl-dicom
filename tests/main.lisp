(defpackage cl-dicom/tests/main
  (:use :cl
        :cl-dicom
        :rove))
(in-package :cl-dicom/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-dicom)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
