(in-package :cl-rrt)
(use-syntax :annot)

@export
@export-accessors
@doc "an rrt-tree implementation which uses
 a simple linear search method for nearest-search."
(defclass rrt-tree-list (rrt-tree-mixin)
  ((nodes :type list :accessor nodes :initform nil)))

(defmethod print-object ((tree rrt-tree-list) s)
  (print-unreadable-object (tree s :type t)
	(format s "NODES: ~a" (count-nodes tree))))

(defmethod reinitialize-instance :around ((tree rrt-tree-list) &rest args)
  @ignore args
  (call-next-method)
  (with-slots (root nodes finish) tree
	;; もし根に親がいればつながりを切る
	(awhen (parent root)
	  (disconnect it root))

	;; ノードを再編成。ルートからたどって全部入れなおす
	(setf nodes nil)
	(mapc-rrt-tree-node-recursively
	 (root tree)
	 (lambda (node)
	   (push node nodes))))
  tree)

(defmethod nearest-node (target-content (tree rrt-tree-list))
  (%nearest-node-list target-content (nodes tree)))

(defun %nearest-node-list (target-content lst)
  (iter
	(for node in lst)
	(for content = (content node))
	(for distance = (configuration-space-distance
					 content target-content))
	(finding (values node distance content)
			 minimizing distance)))

(defmethod insert ((node rrt-tree-node) (tree rrt-tree-list))
  (push node (nodes tree)))

