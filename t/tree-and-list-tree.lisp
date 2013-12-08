(in-package :cl-rrt-test)
(in-suite :rrt)

(defun run-rrt (class &rest args)
  (values
   (apply #'rrt-search
          #'random-point
          #'new-v
          :edge-prohibited-p #'touch-obstacles
          :finish-p #'finish-p
          :start-v *start*
          :tree-class class
          :max-nodes *max-nodes*
          :max-iteration MOST-POSITIVE-FIXNUM
          args)
   *start* *goal*))


(define-condition path-not-found (condition)
  ())

(defun run-rrt-drawing (class path &rest args)
  (with-canvas (:width *xmax* :height *ymax*)
    (multiple-value-bind  (tree *start* *goal*)
        (apply #'run-rrt class
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
      (block finish
        (handler-bind ((error (lambda (c)
                                (declare (ignore c))
                                (signal 'path-not-found)
                                (return-from finish))))
          (draw-point (content (finish-node tree)))))
      (save-png (asdf:system-relative-pathname
                 :cl-rrt-test
                 path)))))


(test :rrt-tree-tree
  (finishes
    (run-rrt-drawing 'rrt-tree-tree "test-rrt-tree-tree.png")))

(test :rrt-tree-list
  (finishes
    (run-rrt-drawing 'rrt-tree-list "test-rrt-tree-list.png")))
