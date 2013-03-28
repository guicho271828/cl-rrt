#|
  This file is a part of cl-rrt project.
  Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Common Lisp implementation of RRT (Rapidily exploring Random Tree), a fast probabilistic multidimentional path-plannning algorithm.

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-rrt-asd
  (:use :cl :asdf))
(in-package :cl-rrt-asd)

(defsystem cl-rrt
  :version "0.1"
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:iterate
               :alexandria
               :cl-annot
               :anaphora)
  :components ((:module "src"
				:serial t
                :components
                ((:file "package")
				 (:file "rrt")
				 (:file "helper")				 
				 (:file "rrt-tree-list")
				 (:file "rrt-tree-tree"))))
  :description "Common Lisp implementation of RRT (Rapidily exploring Random Tree), a fast probabilistic multidimentional path-plannning algorithm."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op cl-rrt-test))))
