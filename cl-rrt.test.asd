#|
  This file is a part of cl-rrt project.
  Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

(defsystem cl-rrt.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of cl-rrt"
  :license "LLGPL"
  :depends-on (:cl-rrt
               :cl-rrt.rtree
               :fiveam
               :vecto)
  :components ((:module "t"
                :components
                ((:file "cl-rrt")
                 (:file "tree-and-list-tree")
                 (:file "split-edges")
                 (:file "rtree"))
                :serial t))
  :perform (load-op :after (op c) 
		    (eval (read-from-string "(fiveam:run! :cl-rrt)"))
		    (asdf:clear-system c)))

