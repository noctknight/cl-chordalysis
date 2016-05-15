;;
;; Load this file to get started with the Bayesian network generator.
;;

(in-package :cl-user)

(require :asdf)

(push :arthur *features*)

(defparameter *base-dir*
  #+arthur #P"/arthur/depot/franz/git/cl-chordalysis/"
  #-arthur #P"/data/data12/zhengxin_pro/alemmens/bayes/")

(loop for dir in '("statistics/"
                   "csv-parser-20140713-git/")
     do (pushnew (merge-pathnames dir *base-dir*) asdf:*central-registry*))

(loop for lib in '(:statistics :csv-parser)
     do (asdf:oos 'asdf:load-op lib :force t))

(compile-file (merge-pathnames #+arthur "bayes.cl" #-arthur "cpt.cl"
                               *base-dir*)
              :load-after-compile t)

(format t "~%;; OK, you can start running or developing now.~%")

