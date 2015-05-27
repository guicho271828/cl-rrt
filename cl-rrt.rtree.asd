#|
This file is a part of cl-rrt project.
Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
Common Lisp implementation of RRT (Rapidily exploring Random Tree), a fast probabilistic multidimentional path-plannning algorithm.

Author: Masataro Asai (guicho2.71828@gmail.com)
|#

(defsystem cl-rrt.rtree
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:iterate
                :alexandria
                :cl-syntax-annot
                :anaphora
                :optima
                :cl-rrt
                :spatial-trees
                :spatial-trees.nns)
  :components ((:module "src/rtree"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "rrt-tree-rtree"))))
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
