(in-package :chordalysis)

(defclass frequency-table ()
  ((columns :initarg :columns :reader columns
            :documentation "A list of COLUMN objects, sorted by column index (lowest first).")
   (total :type number :documentation "The total of all frequencies in the array.")
   (entropy :type number)))

(defgeneric dimensions (table)
  (:documentation "Returns a list with, for each column, the number of unique values in that
column.")
  (:method ((table frequency-table))
    (mapcar #'nr-unique-values (columns table))))

(defclass simple-frequency-table (frequency-table)
  ((array :initarg :array
          :documentation "An array with one dimension for each column.")))

(defun degrees-of-freedom (table)
  (- (apply #'* (dimensions table))
     1))

(defun map-frequency-table-entries (table function)
  "Call FUNCTION for each entry (i.e. frequency) in the frequency table."
  (let* ((columns (columns table))
         (maxima (coerce (dimensions table) 'vector))
         (counters (coerce (loop for c in columns collect 0) 'vector))
         (current-counter (1- (length counters))))
    (loop while (and (> current-counter 0)
                     (< (elt counters current-counter)
                        (elt maxima current-counter)))
         do (let ((entry (frequency-table-entry table counters)))
              (funcall function entry)
              (if (>= (1+ (elt counters current-counter))
                      (elt maxima current-counter))
                  (decf current-counter)
                  (incf (elt counters current-counter)))))))

(defgeneric frequency-table-entry (frequency-table indices)
  (:documentation "Returns the specified entry of a frequency table.
INDICES is a vector of integers.")
  (:method ((table frequency-table) indices)
    ;; For simple in-memory frequency tables we just return the array element.
    (apply #'aref (slot-value table 'array)
           (coerce indices 'list))))

(defun make-simple-frequency-table (columns data-source)
  (let ((table (compute-frequency-array columns data-source)))
    (make-instance 'simple-frequency-table
                   :columns columns
                   :array table)))

(defun compute-frequency-array (columns data-source)
  (let* ((dimensions (mapcar #'nr-unique-values columns))
         (nr-entries (apply #'* dimensions))
         (table-info (format nil "frequency table for ~S, dimensions ~S (~D entries)"
                             (mapcar #'column-name columns)
                             dimensions
                             nr-entries)))
    (when (> nr-entries *max-frequency-table-size*)
      (format t "~&Skipping ~A, because it's too big.~%" table-info)
      (error 'frequency-table-too-big
             :field-names (mapcar #'column-name columns)
             :dimensions dimensions))
    ;; Create and fill the frequency table.
    (format t "~&Computing frequency table with ~D entries and dimensions ~S.~%"
            nr-entries
            dimensions)
    (let ((table (make-array dimensions :element-type 'fixnum :initial-element 0)))
      (dolist (row (rows data-source))
        (block out
          (let ((indices '()))
            (dolist (column columns)
              (let ((value (aref row (column-index column))))
                (when (null value) ; unknown value: we can't increment a frequency
                  (return-from out))
                (let ((index (or (position value (unique-values column) :test #'equal)
                                 (error "value ~s is not valid for ~s" value column))))
                  (push index indices))))
            ;; Increment the right frequency entry.
            (incf (apply #'aref table (nreverse indices))))))
      table)))



(defgeneric entropy-and-total (table nr-rows)
  (:documentation "Returns [1] the entropy, and [2] the total frequency of a frequency table.
  NR-ROWS is the total number of rows (persons) in the database. This is used to scale
the entropy.")
  (:method ((table frequency-table) (nr-rows integer))
    (unless (and (slot-boundp table 'entropy)
                 (slot-boundp table 'total))
      ;; First time: do the actual computation.
      (let* ((size (apply #'* (dimensions table)))
             (array (make-array size :displaced-to (slot-value table 'array) :element-type 'fixnum))
             (log-n (log nr-rows))
             (total-frequency 0)
             (e 0.0d0))
        (loop for frequency across array
           for partial-entropy = (if (zerop frequency)
                                     0
                                     (* frequency (- (log frequency) log-n)))
           do (progn (incf e partial-entropy)
                     (incf total-frequency frequency)))
        (setf (slot-value table 'entropy) (- (/ e nr-rows))
              (slot-value table 'total) total-frequency)
        ;; Get rid of the array because we've computed the important info now and the
        ;; array may take up lots of space.
        (setf (slot-value table 'array) nil)))
    ;; Return the values that we computed already.
    (values (slot-value table 'entropy)
            (slot-value table 'total))))
