(in-package :cl-rrt-test)
(in-suite :rrt)

(defun new-vs (near random)
  "the length of edges are fixed, but many nodes are produced for each random point.
   the straight line is subdivided by the fixed length and all of them are added to the tree."
  (values-list
   (iter (with len = (dist random near))
         (for l from 1 below len by *edge-length*)
         (collecting
          (add near (mul (sub random near)
                         (/ l len)))))))

(defun run-split-rrt (class &rest args)
  (values
   (apply #'split-branch-rrt-search
          #'random-point
          #'new-vs
          :edge-prohibited-p #'touch-obstacles
          :finish-p #'finish-p
          :start-v *start*
          :max-nodes *max-nodes*
          :max-iteration MOST-POSITIVE-FIXNUM
          args)
   *start* *goal*))

(defun run-split-rrt-drawing (class path &rest args)
  (with-canvas (:width *xmax* :height *ymax*)
    (multiple-value-bind  (tree *start* *goal*)
        (apply #'run-split-rrt class
               :run-on-node
               (lambda (nearest new)
                 (draw-edge nearest new)
                 (draw-point new))
               args)
      (iter (for obs in *obstacles*)
            (draw-obst obs))
      (set-rgb-fill 1 0 0)
      (draw-point *start* (/ (max *xmax* *ymax*) 100))
      (set-rgb-fill 0 0 1)
      (draw-point *goal* (/ (max *xmax* *ymax*) 100))
      (set-rgb-fill 0 1 0)
      (ignore-errors
        (draw-point (content (finish-node tree))))
      (save-png (asdf:system-relative-pathname
                 :cl-rrt-test
                 path)))))

(test :split-tree
  (finishes
    (run-split-rrt-drawing
     'rrt-tree-tree
     "test-rrt-tree-tree-split.png")
    (run-split-rrt-drawing
     'rrt-tree-list
     "test-rrt-tree-list-split.png")
    (run-split-rrt-drawing
     'rrt-tree-rtree
     "test-rrt-tree-rtree-split.png"
     :tree-class-options (list :dimension 2))))
