
(in-package :cl-rrt.rtree)
(use-syntax :annot)

(defun node-rect (node)
  @type rrt-tree-node node
  (let ((list (content-list (content node))))
    (make-rectangle :lows list :highs list)))

@eval-always
@export
@doc "an rrt-tree implementation which stores nodes in R-tree."
(defclass rrt-tree-rtree (rrt-tree-mixin)
  ((r :initarg :r
      :initform (spatial-trees:make-spatial-tree :r :rectfun #'node-rect)
      :reader rtree)
   (dimension :initarg :dimension
              :initform (error "dimension of V is missing!")
              :reader dimension)
   (%negative-infty :reader %negative-infty)
   (%positive-infty :reader %positive-infty)))

(defmethod initialize-instance :after ((tree rrt-tree-rtree) &rest args)
  @ignore args
  (let ((dim (dimension tree)))
    (setf (slot-value tree '%negative-infty)
          (make-list dim :initial-element MOST-NEGATIVE-DOUBLE-FLOAT)
          (slot-value tree '%positive-infty)
          (make-list dim :initial-element MOST-POSITIVE-DOUBLE-FLOAT))))

@export
(defgeneric content-list (content))

(defmethod content-list ((v sequence))
  (coerce v 'list))

(defmethod insert ((node rrt-tree-node) (tree rrt-tree-rtree))
  (spatial-trees:insert node (rtree tree)))

(defmethod reinitialize-instance :around ((tree rrt-tree-rtree) &rest args)
  @ignore args
  (call-next-method)
  
  tree)

(defmethod nearest-node (target-content (tree rrt-tree-rtree))
  (ematch tree
    ((rrt-tree-rtree r)
     (ematch (nearest-neighbor-search
              (content-list target-content) r
              (lambda (target-content-list node)
                @ignore target-content-list
                (configuration-space-distance
                 target-content (content node))))
       ((and best-node (rrt-tree-node content))
        (values best-node
                (configuration-space-distance
                 target-content content)
                content))))))

(defmethod nodes ((tree rrt-tree-rtree))
  (spatial-trees:search
   (make-rectangle :lows (%negative-infty tree)
                   :highs (%positive-infty tree))
   (rtree tree)))

