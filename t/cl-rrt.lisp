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
        :cl-rrt.rtree
        :fiveam)
  (:import-from :alexandria :curry))
(in-package :cl-rrt-test)

(def-suite :rrt)
(in-suite :rrt)

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

(defclass obstacle ()
  ((p :accessor p :initarg :p)
   (r :accessor r :initarg :r)))

(defun in-obstacle (p obst)
  (> (r obst) (dist p (p obst))))

(defun touch-obstacle (near new obst)
  (let* ((dp (sub new near))
         (len (norm dp)))
    (unless (zerop len)
      (iter (with dpn = (mul dp (/ 1 len)))
            (for dl to len by 1)
            (thereis (in-obstacle (add near (mul dpn dl)) obst))))))

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

(defun random-point-out-of-obstacles ()
  (iter (for p = (random-point))
        (finding p
                 such-that
                 (notany (curry #'in-obstacle p) *obstacles*))))

(defun new-v (near random)
  "the length of edge from the nearest to the new node is fixed."
  (let ((d (dist random near)))
    (if (zerop d)
        near
        (add near (mul (sub random near)
                       (/ *edge-length* d))))))

(defun touch-obstacles (near new)
  (some (curry #'touch-obstacle near new)
        *obstacles*))
(defun finish-p (new)
  (> *goal-dist* (dist new *goal*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tests

(test :distance
  (is (= (dist (point 0 0)
               (point 3 4))
         5)))

(test :touch-obstacle
  (is (not (touch-obstacle
            (point 0 0)
            (point 1 1)
            (make-instance 'obstacle
                           :p (point 1 0)
                           :r 0.7))))
  (is (touch-obstacle
       (point 0 0)
       (point 1 1)
       (make-instance 'obstacle
                      :p (point 1 0)
                      :r 0.8))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; parameters

(defparameter *xmax* 700)
(defparameter *ymax* 700)
(defparameter *radius* 80)
(defparameter *howmany* 50)
(defparameter *edge-length* 10)
(defparameter *goal-dist* 10)
(defparameter *max-nodes* 10000)
(defparameter *obstacles*
  (iter (repeat *howmany*)
        (collect
            (make-instance 'obstacle
                           :p (point (random *xmax*)
                                     (random *ymax*))
                           :r (random *radius*)))))


(defparameter *goal* (random-point-out-of-obstacles))
(defparameter *start* (random-point-out-of-obstacles))
