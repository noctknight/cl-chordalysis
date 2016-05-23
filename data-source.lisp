(in-package :chordalysis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data sources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *data-source* nil)

;;;
;;; Column
;;;

(defclass column ()
  ((name :initarg :name :reader column-name :documentation "A keyword.")
   (index :initarg :index :type integer :reader column-index)
   (domain :initarg :domain :type hash-table :accessor column-domain
           :documentation "An EQUAL hash table with as keys all values that occur for that
column, and as hash values the number of occurrences for each value.")
   (unique-values :initform nil
                  :documentation "A sorted list of all unique values for this column.")
   (sort-predicate :initarg :sort-predicate :initform 'string-lessp :accessor sort-predicate)))

(defmethod print-object ((column column) stream)
  (print-unreadable-object (column stream :type t :identity nil)
    (format stream "~D ~S with ~D unique values and ~D actual values"
            (column-index column)
            (column-name column)
            (nr-unique-values column)
            (nr-values column))))

(defmethod unique-values ((column column))
  (or (slot-value column 'unique-values)
      (setf (slot-value column 'unique-values)
            (sort (hash-table-keys (column-domain column))
                  (sort-predicate column)))))

(defmethod nr-unique-values ((column column))
  (hash-table-count (column-domain column)))

(defmethod nr-values ((column column))
  (loop for nr-values being the hash-value of (column-domain column)
     sum nr-values))

;;;
;;; Data source
;;;

(defclass data-source ()
  ((headers :initarg :headers :accessor headers
            :documentation "A vector with header names.")
   (rows :initarg :rows :accessor rows
         :documentation "A list of vectors. Each row has the same number of elements as
the HEADERS vector.")
   (columns :initarg :columns :accessor columns
            :documentation "A vector of COLUMN objects.")
   (nr-rows :initarg :nr-rows :reader nr-rows)))

(defmethod print-object ((csv data-source) stream)
  (print-unreadable-object (csv stream :type t)
    (format stream "with ~D columns and ~D rows"
            (length (headers csv))
            (nr-rows csv))))

;;;
;;; Some basic functions
;;;

(defgeneric column-position (header-name data-source)
  (:documentation "Returns either NIL or an array index."))


(defmethod column-position (header-name (data data-source))
  (position header-name (headers data) :test #'string-equal))

(defmethod nr-columns ((data data-source))
  (length (headers data)))

(defun map-column (function column-index csv-data)
  "For each row in CSV-DATA, call FUNCTION with two values: [1] the value for the
specified column and [2] the row itself."
  (loop for row in (rows csv-data)
       for i from 0
       do (funcall function (aref row column-index) row)))

(defun update-cell (data-source row column-index value)
  (declare (ignore data-source))
  (setf (aref row column-index) value))


(defmethod find-column-named (column-name (data data-source))
  (or (find column-name (columns data) :key 'column-name :test #'string-equal)
      (error "Can't find column ~S in ~S" column-name data)))



(defmethod compute-column ((column-index integer) (data data-source))
  "Returns a COLUMN object."
  (make-instance 'column
                 :name (intern (elt (headers data) column-index) :keyword)
                 :index column-index
                 :domain (compute-domain column-index data)))

(defun compute-domain (column-index data-source)
  (let ((domain (make-hash-table :test #'equal)))
    (map-column (lambda (value row)
                  (declare (ignore row))
                  (when value  ; a value of NIL means a missing value
                    (incf (gethash value domain 0))))
                column-index
                data-source)
    domain))

(defmethod (setf column-domain) :after (value (column column))
  ;; Make sure UNIQUE-VALUES gets recomputed when necessary.
  (declare (ignore value))
  (setf (slot-value column 'unique-values) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cleaning up the data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clean-up-data (field-specs data-source)
  "FIELD-SPECS is a list of field specifications. Each field specification is a plist that
must contain a property :NAME and may contain any of the the following
properties: :MISSING, :TYPE, :QUANTILES.  Missing values will be replaced by NIL. Values
for numerical fields will be converted from strings to numbers (and will become NIL if
that conversion is not possible). Values for numerical fields with quantile intervals will
be converted to a keyword symbol that represents the quantile.  These operations will all
change DATA-SOURCE."
  (format t "~&Cleaning up data...~%")
  ;; First replace all empty strings by NIL to indicate a missing value.
  (loop for column across (columns data-source)
       do (mark-missing-values *default-missing* column data-source))
  ;; Now clean up according to given field specifications.
  (loop for spec in field-specs
       do (clean-up-column spec data-source))
  ;; Recompute all column domains.
  (loop for column across (columns data-source)
       do (setf (column-domain column)
                (compute-domain (column-index column) data-source))))

(defun clean-up-column (spec data-source)
  (destructuring-bind (&key name (missing '("")) type quantiles)
      spec
    (unless name
      (error "Missing field name in ~S" spec))
    (let ((column (find-column-named name data-source)))
      (when missing
        (assert (listp missing))
        (mark-missing-values missing column data-source))
      (when (and type (string-equal type :numerical))
        (convert-strings-to-numbers column data-source)
        (setf (sort-predicate column) '<))
      (when quantiles
        (put-numbers-into-quantiles quantiles column data-source)
        (setf (sort-predicate column) 'string-lessp)))))

(defun mark-missing-values (missing column data-source)
  "MISSING is a list of strings that represent a missing value.
Replace the value of all cells in COLUMN that have such a missing value by NIL.
Returns the number of replaced values."
  (let ((column-index (column-index column))
        (count 0))
    (map-column (lambda (value row)
                  (when (and (stringp value)
                             (find (string-trim '(#\space) value) missing
                                   :test #'string-equal))
                    (update-cell data-source row column-index nil)
                    (incf count)))
                column-index
                data-source)
    count))


(defun convert-strings-to-numbers (column data-source)
  "Converts all strings in COLUMN to numbers, whenever that's possible. If it's not
possible, replace the string by NIL. Returns two numbers: [1] the number of values that
were converted to a number, [2] the number of values that were replaced by NIL."
  (let ((column-index (column-index column))
        (number-count 0)
        (nil-count 0))
    (map-column (lambda (value row)
                  (when (stringp value)
                    (let ((number (try-to-read-number value)))
                      (cond (number
                             (update-cell data-source row column-index number)
                             (incf number-count))
                            (t (incf nil-count))))))
                column-index
                data-source)
    (values number-count nil-count)))

(defun try-to-read-number (string)
  "Returns either a number of NIL."
  (let ((*read-eval* nil))
    (let ((result (ignore-errors (read-from-string (string-trim '(#\space) string)))))
      (when (numberp result)
        result))))


(defstruct interval
  name  ; a keyword
  min   ; may be NIL
  max   ; may be NIL
  )


(defun make-quantile-intervals (quantiles)
  "QUANTILES is a list of interval borders, e.g. (0 10 20 50).
Returns the corresponding INTERVAL objects (adding a left-open interval at
the beginning and a right-open interval at the end)."
  (flet ((make (min max)
           (make-interval :name (intern (format nil "interval_~S_~S" min max) :keyword)
                          :min min
                          :max max)))
    (let ((prev-max nil))
      (append (loop for max in quantiles
                 collect (prog1 (make prev-max max)
                           (assert (or (null prev-max) (> max prev-max)))
                           (setq prev-max max)))
              ;; Add a right-open interval for numbers that are higher
              ;; than the highest specified maximum.
              (list (make prev-max nil))))))


(defun find-interval-for-number (x intervals)
  (or (find-if (lambda (interval)
                 (let ((min (interval-min interval))
                       (max (interval-max interval)))
                   (and (or (null min) (<= min x))
                        (or (null max) (< x max)))))
               intervals)
      (error "Can't find interval for ~S in ~S" x intervals)))

                    
(defun put-numbers-into-quantiles (quantiles column data-source)
  (let ((column-index (column-index column))
        (count 0))
    (let ((intervals (make-quantile-intervals quantiles)))
      (map-column (lambda (value row)
                    (when (numberp value)
                      (let ((interval (find-interval-for-number value intervals)))
                        (update-cell data-source row column-index (interval-name interval))
                        (incf count))))
                column-index
                data-source))
    count))
