#|
  This file is a part of cl-rrt project.
  Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-rrt-test-asd
  (:use :cl :asdf))
(in-package :cl-rrt-test-asd)

(defsystem cl-rrt-test
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:cl-rrt
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "cl-rrt"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
