
(in-package :cl-rrt-test)
(def-suite :rrt.performance :in :rrt)
(in-suite :rrt.performance)

(defmacro with-timing (&body body)
  (let ((start (gensym)))
    `(let ((,start (get-internal-run-time)))
       ,@body
       (- (get-internal-run-time) ,start))))

(defun perform ()
  (let ((max-trials 10)
        (max-trials-per-environment 3)
        (hash (make-hash-table :test #'equal)))
    (ensure-directories-exist
     (asdf:system-relative-pathname :cl-rrt-test "environments/")
     :verbose t)
    (iter (generate trial from 1 to max-trials)
          (let* ((*obstacles*
                  (iter (repeat *howmany*)
                        (collect
                            (make-instance 'obstacle
                                           :p (point (random *xmax*)
                                                     (random *ymax*))
                                           :r (random *radius*)))))
                 (*goal* (random-point-out-of-obstacles))
                 (*start* (random-point-out-of-obstacles)))
            (handler-bind
                ((path-not-found
                  (lambda (c)
                    (declare (ignore c))
                    (format t "~&path not found, discarding current test environment~%")
                    (next-iteration))))
              (run-rrt-drawing
               'rrt-tree-tree
               (format nil "environments/edge-~a-~2,,,'0@a.png"
                       *edge-length* trial)))
            
            (next trial)
            (flet ((time-it (class fn &rest args)
                     (incf (gethash (list class fn) hash 0)
                           (with-timing
                             (iter (repeat max-trials-per-environment)
                                   (finishes (apply fn class args)))))))
              (iter (for fn in (list #'run-rrt #'run-split-rrt))
                    (time-it 'rrt-tree-list fn)
                    (time-it 'rrt-tree-tree fn)
                    (time-it 'rrt-tree-rtree fn
                             :tree-class-options (list :dimension 2))))))
    (maphash (lambda (key value)
               (format t "~&~a:~50t~5,2f [sec.]"
                       key (/ value INTERNAL-TIME-UNITS-PER-SECOND)))
             hash)))

(test :performance-small
  (let* ((*xmax* 700)
         (*ymax* 700)
         (*radius* 60)
         (*howmany* 70)
         (*edge-length* 10)
         (*goal-dist* 10)
         (*max-nodes* 10000))
    (print :small)
    (perform)))

(test :performance-medium
  (let* ((*xmax* 700)
         (*ymax* 700)
         (*radius* 60)
         (*howmany* 70)
         (*edge-length* 6)
         (*goal-dist* 6)
         (*max-nodes* 10000))
    (print :medium)
    (perform)))

(test :performance-large
  (let* ((*xmax* 700)
         (*ymax* 700)
         (*radius* 60)
         (*howmany* 70)
         (*edge-length* 3)
         (*goal-dist* 3)
         (*max-nodes* 10000))
    (print :large)
    (perform)))
