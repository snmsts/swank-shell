(in-package :cl-user)

(defpackage :swank-shell-asd
  (:use :cl :asdf))

(in-package :swank-shell-asd)

(asdf:defsystem swank-shell
  :description ""
  :author "SANO Masatoshi <snmsts@gmail.com>"
  :licence "MIT"
  :depends-on (:usocket :bordeaux-threads :trivial-utf-8 :babel :swank)
  :components
  ((:module src
    :serial t
    :components ((:file "client"))
    )))
