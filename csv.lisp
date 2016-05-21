(in-package :chordalysis)

(defclass csv-data (data-source)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loading and preprocessing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-csv (&key (path (merge-pathnames *db-name* cl-user::*base-dir*))
                   (max-nr-columns *max-columns*))
  (format t "~&Loading CSV file ~S~%" path)
  (flet ((to-vector (list)
           (coerce (if (< (length list) max-nr-columns)
                       list
                       (subseq list 0 max-nr-columns))
                   'vector)))
    (with-open-file (in path :direction :input :external-format :utf-8)
      (let* ((headers (to-vector (csv-parser:read-csv-line in)))
             (rows (loop for line = (csv-parser:read-csv-line in)
                      while line
                      collect (to-vector line)))
             (result (make-instance 'csv-data
                                    :headers headers
                                    :rows rows
                                    :nr-rows (length rows))))
        ;; Compute column info.
        (setf (columns result) (make-array (nr-columns result)))
        (loop for i below (nr-columns result)
           do (setf (elt (columns result) i)
                    (compute-column i result)))
        ;;
        result))))
