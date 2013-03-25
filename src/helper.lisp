(in-package :cl-rrt)

(annot:enable-annot-syntax)
@export
@doc "HELPER FUNCTION: ensure a node doesn't have a parent"
(defun orphanize (child)
  (awhen (parent child)
	(disconnect it child)))

@export
@doc "HELPER FUNCTION: disconnect all children from the
specified parent"
(defun neglect (parent)
  @type rrt-tree-node parent
  (mapcar (lambda (node) (disconnect parent node))
		  (children parent)))

@export
@doc "HELPER FUINCTION: removes the children of old-parent and
the new-parent takes all of them."
(defun adopt-children (new-parent old-parent)
  @type rrt-tree-node new-parent
  @type rrt-tree-node old-parent
  (mapcar (lambda (node)
			(disconnect old-parent node)
			(connect new-parent node))
		  (children old-parent)))