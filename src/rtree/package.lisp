(in-package :cl-user)
(defpackage cl-rrt.rtree
  (:use :cl
	:cl-syntax
        :rrt
        :rectangles
	:annot
	:annot.class
	:annot.eval-when
	:annot.doc
	:annot.slot
	:alexandria
        :trivia
	:iterate)
  (:shadowing-import-from :spatial-trees.nns
                          :nearest-neighbor-search)
  (:shadowing-import-from :rectangles
                          :make-rectangle
                          :intersection))
