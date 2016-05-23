(in-package :chordalysis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *log-timestamp* nil)

(defmacro with-logging (() &body body)
  `(call-with-logging (lambda () ,@body)))

(defun call-with-logging (thunk)
  (setq *log-timestamp*
        (multiple-value-bind (ss mm hh day month year)
            (decode-universal-time (get-universal-time))
          (format nil "~D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0D"
                  year month day hh mm ss)))
  (with-open-file (log-stream (log-file)
                              :direction :output :if-exists :supersede
                              :external-format :utf-8)
    (let ((*standard-output* (make-broadcast-stream *standard-output* log-stream)))
      (funcall thunk))))

(defun log-file ()
  (make-pathname :name (format nil "log-chordalysis-~A" *log-timestamp*)
                 :type "txt"
                 :defaults cl-user::*base-dir*))

(defun graphical-log-file ()
  (when *graphical-log-file*
    (make-pathname :name (format nil "~A-~A" (pathname-name *graphical-log-file*) *log-timestamp*)
                   :type (pathname-type *graphical-log-file*)
                   :defaults cl-user::*base-dir*)))

(defun graphical-log (message &key graph is-directed)
  (with-open-file (out (graphical-log-file) :direction :output :external-format :utf-8
                       :if-exists :append
                       :if-does-not-exist :create)
    (format out "~&TextCell[~S]~%" message)
    (format t "~&~A~%" message)
    (when graph
      (log-graph graph out is-directed))))


(defun log-graph (graph out is-directed)
  ;; Example output: Graph[{1 -> 2, 2 -> 3, 3 -> 4, 5 -> 4, 4 -> 1}]
  (format out "~&~%Graph[{")
  (let ((edges (if is-directed
                   (graph-edges graph)
                   (remove-duplicates (graph-edges graph) :test #'edges-have-same-vertices)))
        (edge-symbol (if is-directed "->" "<->")))
    (loop for (edge . more) on edges
       do (destructuring-bind (v1 . v2)
              edge
            (format out "~A ~A ~A" v1 edge-symbol v2)
            (when more
              (format out ", "))))
    (format out "}")
    ;; Add display options
    (format out ", VertexLabels -> \"Name\", VertexShapeFunction -> \"Diamond\"")
    ;;
    (format out "]~%")))

       
