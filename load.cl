;;
;; LOAD this file to get started with the Bayesian network generator.
;;

(in-package :cl-user)

(require :asdf)

(push :laptop *features*)  ; Comment this line out if you have access to a real Allegrograph database.

#-laptop
(eval-when (compile load eval)
  (require :agraph
	   "/data/data12/zhengxin_pro/Heater/BIN/agraph-6.0.1/lib/agraph.fasl"))

(defparameter *base-pathname* *load-truename*)

(defparameter *base-dir*
  (let ((this-directory (pathname-directory *base-pathname*)))
    (make-pathname :directory this-directory)))

(loop for dir in '("statistics/" "csv-parser-20140713-git/" "yason-0.7.2/"
                   "alexandria-20140826-git/" "trivial-gray-streams-20140826-git/")
   do (pushnew (merge-pathnames dir *base-dir*) asdf:*central-registry*))

(loop for lib in '(:statistics :csv-parser :alexandria :yason)
   do (asdf:oos 'asdf:load-op lib :force t))

(loop for file in '("package.lisp" "specials.lisp" "util.lisp" "conditions.lisp"
                    "config.lisp"
                    "graph.lisp"
                    "log.lisp"
                    "data-source.lisp" "csv.lisp" 
                    "frequency-table.lisp"
                    "chordalysis.lisp"
                    #-laptop "agraph-to-csv.lisp")
   do (compile-file (merge-pathnames file *base-dir*)
                    :load-after-compile t))

