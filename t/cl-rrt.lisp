#|
  This file is a part of cl-rrt project.
  Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-rrt-test
  (:use :cl
		:iterate
		:vecto
        :cl-rrt
        :cl-test-more)
  (:import-from :alexandria :curry))
(in-package :cl-rrt-test)

(plan nil)

(defclass point ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))

(defun point (x y)
  (make-instance 'point :x x :y y))

(defun ^2 (x) (* x x))

(defun prod (p1 p2)
  (+ (* (x p1) (x p2))
	 (* (y p1) (y p2))))
(defun mul (p1 c)
  (point (* (x p1) c)
		 (* (y p1) c)))
(defun add (p1 p2)
  (point (+ (x p1) (x p2))
		 (+ (y p1) (y p2))))
(defun sub (p1 p2)
  (point (- (x p1) (x p2))
		 (- (y p1) (y p2))))
(defun norm (p)
  (sqrt (+ (^2 (x p))
		   (^2 (y p)))))
(defun dist (p1 p2)
  (norm (sub p1 p2)))

(defmethod configuration-space-distance ((p1 point) (p2 point))
  (dist p1 p2))

(ok (= (dist (point 0 0)
			 (point 3 4))
	   5))

(defvar *xmax* 300)
(defvar *ymax* 300)
(defvar *radius* 30)

(defclass obstacle ()
  ((p :accessor p :initarg :p)
   (r :accessor r :initarg :r)))

(defparameter *obstacles*
  (iter (repeat 30)
		(collect
			(make-instance 'obstacle
						   :p (point (random *xmax*)
									 (random *ymax*))
						   :r (random *radius*))))) ;; radius

(defun in-obstacle (p obst)
  (> (r obst) (dist p (p obst))))

(defun touch-obstacle (near new obst)
  (let* ((dp (sub new near))
		 (len (norm dp)))
	(unless (zerop len)
	  (iter (with dpn = (mul dp (/ 1 len)))
			(for dl to len by 1)
			(thereis (in-obstacle (add near (mul dpn dl)) obst))))))

(ok (not (touch-obstacle
		  (point 0 0)
		  (point 1 1)
		  (make-instance 'obstacle
						 :p (point 1 0)
						 :r 0.7))))
(ok (touch-obstacle
		 (point 0 0)
		 (point 1 1)
		 (make-instance 'obstacle
						:p (point 1 0)
						:r 0.8)))

(defun draw-obst (obst)
  (with-slots (p r) obst
	(with-slots (x y) p
	  (centered-circle-path x y r)
	  (stroke))))

(defun draw-point (p &optional (r 2))
  (with-graphics-state
	(set-rgba-stroke 0 0 0 0.2)
	(with-slots (x y) p
	  (centered-circle-path x y r)
	  (fill-path))))
(defun draw-edge (p1 p2)
  (move-to (x p1) (y p1))
  (line-to (x p2) (y p2))
  (stroke))

(defun random-point ()
  (point (random *xmax*) (random *ymax*)))

(defparameter *edge-length* 10)
(defun new-v (near random)
  (add near (mul (sub random near)
				 (/ *edge-length* (dist random near)))))

(defparameter *goal-dist* 10)
(defparameter *goal* (random-point))
(defparameter *start* (random-point))

(defun touch-obstacles (near new)
  (some (curry #'touch-obstacle near new)
		*obstacles*))
(defun finish-p (new)
  (> *goal-dist* (dist new *goal*)))


(with-canvas (:width *xmax* :height *ymax*)
  (iter (while (some (curry #'in-obstacle *goal*) *obstacles*))
		(setf *goal* (random-point)))
  (iter (while (some (curry #'in-obstacle *start*) *obstacles*))
		(setf *start* (random-point)))
  (let ((tree (rrt-search #'random-point
						  #'new-v
						  :edge-prohibited-p #'touch-obstacles
						  :finish-p #'finish-p
						  :start-v *start*
						  :max-nodes MOST-POSITIVE-FIXNUM
						  :max-iteration MOST-POSITIVE-FIXNUM
						  :run-on-node (lambda (nearest new)
										 (draw-edge nearest new)
										 (draw-point new)))))
	
	(iter (for obs in *obstacles*)
		  (draw-obst obs))
	(set-rgb-fill 1 0 0)
	(draw-point *start*)
	(set-rgb-fill 0 0 1)
	(draw-point *goal*)
	(set-rgb-fill 0 1 0)
	(draw-point (content (finish-node tree)))
	(save-png #.(asdf:system-relative-pathname
				 :cl-rrt-test
				 "test-rrt-tree-tree.png"))))

(with-canvas (:width *xmax* :height *ymax*)
  (iter (while (some (curry #'in-obstacle *goal*) *obstacles*))
		(setf *goal* (random-point)))
  (iter (while (some (curry #'in-obstacle *start*) *obstacles*))
		(setf *start* (random-point)))
  (let ((tree (rrt-search #'random-point
						  #'new-v
						  :edge-prohibited-p #'touch-obstacles
						  :finish-p #'finish-p
						  :start-v *start*
						  :max-nodes MOST-POSITIVE-FIXNUM
						  :max-iteration MOST-POSITIVE-FIXNUM
						  :run-on-node (lambda (nearest new)
										 (draw-edge nearest new)
										 (draw-point new)))))
	
	(iter (for obs in *obstacles*)
		  (draw-obst obs))
	(set-rgb-fill 1 0 0)
	(draw-point *start*)
	(set-rgb-fill 0 0 1)
	(draw-point *goal*)
	(set-rgb-fill 0 1 0)
	(draw-point (content (finish-node tree)))
	(save-png #.(asdf:system-relative-pathname
				 :cl-rrt-test
				 "test-rrt-tree-list.png"))))

;; blah blah blah.

(defun new-vs (near random)
  (values-list
   (iter (with len = (dist random near))
		 (for l from 1 below len by *edge-length*)
		 (collecting
		  (add near (mul (sub random near)
						 (/ l len)))))))


(with-canvas (:width *xmax* :height *ymax*)
  (iter (while (some (curry #'in-obstacle *goal*) *obstacles*))
		(setf *goal* (random-point)))
  (iter (while (some (curry #'in-obstacle *start*) *obstacles*))
		(setf *start* (random-point)))
  (let ((tree (split-branch-rrt-search
			   #'random-point
			   #'new-vs
			   :edge-prohibited-p #'touch-obstacles
			   :finish-p #'finish-p
			   :start-v *start*
			   :tree-class 'rrt-tree-list
			   :max-nodes MOST-POSITIVE-FIXNUM
			   :max-iteration MOST-POSITIVE-FIXNUM
			   :run-on-node (lambda (nearest new)
							  (draw-edge nearest new)
							  (draw-point new)))))
	
	(iter (for obs in *obstacles*)
		  (draw-obst obs))
	(set-rgb-fill 1 0 0)
	(draw-point *start*)
	(set-rgb-fill 0 0 1)
	(draw-point *goal*)
	(set-rgb-fill 0 1 0)
	(draw-point (content (finish-node tree)))
	(save-png #.(asdf:system-relative-pathname
				 :cl-rrt-test
				 "test-split-rrt.png"))))
(finalize)
