(in-package :cl-rrt-test)
(in-suite :rrt)

(defmethod content-list ((p point))
  (list (x p) (y p)))

(test :rrt-tree-rtree
  (finishes
    (run-rrt-drawing 'rrt-tree-rtree "test-rrt-rtree.png"
                     :tree-class-options (list :dimension 2))))
