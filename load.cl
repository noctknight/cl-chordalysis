;;
;; Load this file to get started with the Bayesian network generator.
;;

(pushnew #P"/data/data12/zhengxin_pro/alemmens/bayes/statistics/"
         asdf:*central-registry*)

(asdf:oos 'asdf:load-op :statistics :force t)

(compile-file "cpt.cl" :load-after-compile t)

(format t "~%;; OK, you can start running or developing now.~%")

