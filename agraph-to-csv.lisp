(in-package :chordalysis)

(eval-when (compile load eval) (enable-!-reader))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Create a CSV file with all relevant person data from an AG database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *csv-directory* "/data/data12/zhengxin_pro/alemmens/")

(defstruct predicate-info
  predicate
  header
  column-number)
  
(defun agraph-to-csv (&key
                        (db-name "testdb")
                        (csv-filename (make-pathname :name db-name
                                                     :type "csv"
                                                     :defaults *csv-directory*)))
  (open-triple-store db-name)
  (register-namespace "credoo" "http://credoo.com/")
  (unwind-protect (agraph-to-csv* csv-filename)
    (close-triple-store)))

(defun agraph-to-csv* (csv-filename)
  (let* ((all-predicates (mapcar #'car (run-sparql "select distinct ?p where { ?s ?p ?o}"
                                                   :results-format :lists)))
         (predicates (loop for p in all-predicates
                        unless (or (part= !rdf:type p)
                                   ;; we don't want weibo for some reason
                                   (search "weibo" (part->terse p)))
                        collect p))
         (predicate-infos (loop for p in predicates
                             for i from 0
                             collect (make-predicate-info :predicate p
                                                          :header (part->terse p)
                                                          :column-number i))))
    (with-open-file (out csv-filename :direction :output :external-format :utf-8
                         :if-exists :supersede)
      (write-csv-row out (mapcar #'predicate-info-header predicate-infos))
      (let ((count 0))
        (tslet ((q -person !rdf:type !<http://credoo.com/Person>))
          ;; Collect row values for person.
          (let ((row
                 (loop for p in predicate-infos
                    for column-number from 0
                    collect (let ((pred (predicate-info-predicate p))
                                  (value nil))
                              (tslet ((triple person pred -obj))
                                ;; We assume maximum 1 value per person&predicate.
                                ;; That's not actually guaranteed anywhere.
                                (setq value (upi->value obj)))
                              value))))
            (write-csv-row out row))
          (when (zerop (mod (incf count) 1000))
            (format t "~&~D~%" count)))
        (values csv-filename count (length predicate-infos))))))

(defun write-csv-row (stream values)
  (loop for (value . more) on values
     do (progn
          (when value
            (format stream "~S" value))
          (when more
            (format stream ","))))
  (format stream "~%"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Just some statistics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stats (&key (db-name "testdb"))
  (open-triple-store db-name)
  (unwind-protect
      (do-stats)
    (close-triple-store)))

(defun do-stats ()
  "Looks for predicates and their unique values."
  (let ((preds (run-sparql "select distinct ?p where { ?s ?p ?o}"
                           :results-format :lists)))
    (format t "~d predicates~%" (length preds))
    (dolist (pred preds)
      (setq pred (car pred))
      (format t "~30a " (part->terse pred))
      (let ((objects (make-hash-table :test #'equal)))
        (tslet ((q -sub pred -obj))
	  (setf (gethash (upi->value obj) objects) t))
	(format t " ~5d unique values~%" (hash-table-count objects))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Adding rdf:type credoo:Person triples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;      
;;; Add an rdf:type credoo:Person triple for all distinct subjects
;;; in the database.
;;;

(defun add-type-triple ()
  (open-triple-store *db-name*)
  (unwind-protect
      (do-add-type-triple)
    (close-triple-store)))

(defun do-add-type-triple ()
  (dolist (item (run-sparql "select distinct ?s { ?s ?p ?o}"
                            :results-format :lists))
    (setq item (car item))
    #-laptop(add-triple item !rdf:type !<http://credoo.com/Person>))
  (commit-triple-store))

