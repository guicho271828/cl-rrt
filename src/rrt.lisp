(in-package :cl-rrt)
(annot:enable-annot-syntax)

@export
@export-accessors
@doc "Node class of Random Tree."
(defclass rrt-tree-node ()
  ((parent :type (or null rrt-tree-node)
		   :accessor parent
		   :initarg :parent
		   :initform nil)
   (children :type list
			 :initform nil
			 :accessor children)
   (content :initarg :content
			:initform nil
			:accessor content)))

@export
@doc "
Identical to =(make-instance 'rrt-tree-node :content content)="
(defun rrt-node (content)
  (make-instance 'rrt-tree-node :content content))

@export
@doc "connect two nodes as a parent and a child."
(defun connect (parent child)
  @type rrt-tree-node parent
  @type rrt-tree-node child
  (push child (children parent))
  (setf (parent child) parent))

@export
@doc ""
(define-condition child-not-found (simple-condition)
  ((parent :type rrt-tree-node)
   (child :type rrt-tree-node)))

@export
@doc "disconnect a parent and its child.
signals CHILD-NOT-FOUND < SIMPLE-CONDITION."
(defun disconnect (parent child)
  @type rrt-tree-node parent
  @type rrt-tree-node child
  (if (find child (children parent))
	  (removef (children parent) child)
	  (signal 'child-not-found :parent parent :child child))
  (setf (parent child) nil))

@export
@doc "Map over the RRT-TREE-NODEs of the tree and
 return the results in a nested cons tree 
with the same structure as that of the original random-tree."
(defun map-rrt-tree-node-recursively (node fn)
  @type rrt-tree-node node
  @type function fn
  (aif (children node)
	   (list* (funcall fn node)
			  (mapcar (lambda (node)
						(map-rrt-tree-node-recursively node fn))
					  it))
	   (funcall fn node)))

@export
@doc "Mapc over the RRT-TREE-NODEs of the tree and returns nil.
 Only for the side effect."
(defun mapc-rrt-tree-node-recursively (node fn)
  @type rrt-tree-node node
  @type function fn
  (funcall fn node)
  (awhen (children node)
	(mapc (lambda (node)
			(map-rrt-tree-node-recursively node fn))
		  it)))

@export
@doc "Map over the contents of RRT-TREE-NODEs of the tree and
 return each result in a nested tree 
with the same structure as the original random-tree."
(defun map-rrt-tree-content-recursively (node fn)
  @type rrt-tree-node node
  @type function fn
  (map-rrt-tree-node-recursively
   node (lambda (node) (funcall fn (content node)))))

@export
@doc "Mapc over the contents of RRT-TREE-NODEs ofthe tree and returns nil.
 Only for the side effect."
(defun mapc-rrt-tree-content-recursively (node fn)
  @type rrt-tree-node node
  @type function fn
  (mapc-rrt-tree-node-recursively
   node (lambda (node) (funcall fn (content node)))))

@export
@export-accessors
@doc "The abstract interface mixin class of rrt-tree. User do not create
an instance of it, but rather inherit it. It has three slots and accessors with the same names:

+ root : the root node of the tree. of type rrt-tree-node.
+ nodes : an abstract slot which should be redefined in the inherited class.
   stores a container of the entire nodes.
+ finish : a slot which contains the last node of the computed path.
   It is bound to nil, it means the previous search has failed to find
   a path which satisfies the goal condition."
(defclass rrt-tree-mixin ()
  ((root :type (or null rrt-tree-node)
		 :accessor root
		 :initarg :root)
   (nodes :accessor nodes)
   (finish-node :type (or null rrt-tree-node)
		   :documentation ""
		   :initarg :finish-node
		   :accessor finish-node
		   :initform nil)))

(defmethod initialize-instance :after ((tree rrt-tree-mixin)
									   &rest args
									   &key root &allow-other-keys)
  @ignore args
  (insert root tree))

@export
@doc "This generic function should implement a method
which finds the nearest node in a `tree' to the `target'."
(defgeneric nearest-node (target tree))

@export
@doc "This generic function should provide a method
 to insert a `content' to the `tree'."
(defgeneric insert (content tree))

@export
@doc "This generic function should provide a method
to accumulate all leafs into a list."
(defgeneric leafs (tree))

@export
@doc "This generic function should provide a method
to count the number of leafs."
(defgeneric count-node (tree))

@export
@doc "This generic function should provide a method
to measure the distance between two points in C-space
 (configuration space)."
(defgeneric configuration-space-distance (point1 point2))

@export
@doc "RRT-search function.
let V as a type variable.

+ V :: a vector class which represents the point in C-space.
           (configuration-space-distance V V) should return a number.
+ (node V) :: an rrt-tree-node instance whose `content' slot is V.
           non-holonomic parameters like velocity and acceralation
           should be stored within (node V), not in V.

`rrt-search' returns the result tree as its primary value.  The
secondaly value is the total number of the nodes, and third value is
the number of iteration done in the search.

the arguments should be of types as listed in the following :

+ start-v : V
+ random-generator: (no args) -> V
+ new-v-generator: V, V -> V ; nearest, random -> actual
+ edge-prohibited-p: V, V -> bool ; nearest, new -> result
+ finish-p: V -> bool ; new -> result
+ run-on-node: V, V -> t ; nearest, new ->

+ container-generator: (no args) -> (container (node V))
+ container-insert: V, (container (node V)) -> (container (node V))
+ container-nearest-search: V, (container (node V)) -> V
"
(defun rrt-search
	(random-generator
	 new-v-generator
	 edge-prohibited-p
	 finish-p
	 &key
	 start-v
	 tree
	 (tree-class 'rrt-tree-tree)
	 (max-nodes 15)
	 (max-iteration 30)
	 run-on-node)
  (iter
	(with start-node =
		  (cond
			((and tree (root tree)) (root tree))
			(start-v (rrt-node start-v))
			(t (error "either start-v or tree needs to be specified. ~
                       tree should have its root node already.")))
	(with tree = 
		  (if tree
			  (reinitialize-instance tree :finish-node nil)
			  (make-instance tree-class :root start-node)))
	(generate i
			  from (count-node tree)
			  below max-nodes)
	(for j below max-iteration)
	(for random-v     = (funcall random-generator))
	(for nearest-node = (nearest-node random-v tree))
	(for nearest-v    = (content nearest-node))
	(for new-v = (funcall new-v-generator nearest-v random-v))
	(when (funcall edge-prohibited-p nearest-v new-v)
	  (next-iteration))
	(funcall-when run-on-node nearest-v new-v)
	(for new-node = (rrt-node new-v))
	(connect nearest-node new-node)
	(insert new-node tree)
	(when (funcall finish-p new-v)
	  (setf (finish-node tree) new-node)
	  (terminate))
	(next i)
	(finally
	 (return (values tree i j))))))

@export @doc "Returns the nodes of the computed path in a list, from
the root to the end. Returns nil if the path was not found. The list
contains the root of the tree."
(defun result-path-nodes (tree)
  @type rrt-tree-mixin tree
  (iter (with results = nil)
		(for node
			 initially (finish-node tree)
			 then (parent node))
		(while node)
		(push node results)
		(finally (return results))))

@export
@doc "Returns a list of C-space points of the computed paths
from the root to the end.  Returns nil if the path was not found. The
list contains the root of the tree."
(defun result-path (tree)
  @type rrt-tree-mixin tree
  (mapcar #'content (result-path-nodes tree)))

;; @export
;; @doc "returns a newly created `rrt-tree'. 
;; it selects the branch with the computed path
;; out of the branches directly connected to the root and
;; reassignes the branch to the root."
;; (defun next-branch (tree)
;;   @type rrt-tree-mixin tree
;;   (new (class-of tree)
;; 	   :root (aif (result-path-nodes tree)
;; 				  (second it)
;; 				  (random-elt (children (root tree))))))

@export
@doc "Destructively modifies and return an RRT-TREE.  If the
`tree' has a finish node, it finds a path from the root to
the end and then replace the root with the next node in that path.
Otherwise it choose one child of the root at random and replace the
root with it. In both cases the new root is orphanized."
(defun nnext-branch (tree)
  @type rrt-tree-mixin tree
  (let* ((new-root (aif (result-path-nodes tree)
						(second it)
						(random-elt (children (root tree))))))
	(reinitialize-instance tree :root new-root)
	tree))
