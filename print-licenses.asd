;; #|
;;   This file is a part of print-licenses project.
;; |#


(in-package :cl-user)
(defpackage print-licenses-asd
  (:use :cl :asdf))
(in-package :print-licenses-asd)

(defsystem "print-licenses"
  :version "0.1.0"
  :author "vindarel"
  :license "MIT"
  :depends-on (:alexandria
               :iterate)
  :components ((:file "print-licenses"))
  :description "Print the licenses used by the given project and its dependencies.")
