#|
  This file is a part of cl-rrt project.
  Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

(defsystem cl-rrt.benchmark
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Benchmarking system of cl-rrt"
  :license "LLGPL"
  :depends-on (:cl-rrt
               :cl-rrt.rtree
               :cl-rrt.test
               :fiveam
               :vecto)
  :components ((:module "t"
                :components
                ((:file "benchmark"))
                :serial t))
  :perform (load-op :after (op c) 
		    (eval (read-from-string "(fiveam:run! :cl-rrt.benchmark)"))
		    (asdf:clear-system c)))

