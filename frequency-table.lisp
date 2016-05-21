(in-package :chordalysis)

(defclass frequency-table ()
  ((columns :initarg :columns :reader columns
            :documentation "A list of COLUMN objects.")
   (array :initarg :table :accessor array
          :documentation "An array with one dimension for each column.")
   (total :accessor total
          :documentation "The total of all frequencies in the array.")
   (entropy :accessor entropy :type number)))

#| LATER

(defun compute-node-frequency-table (node &key (verbose t))
  (init-cpt-for-node node)
  (when verbose
    (format t "~&Computing frequency table for ~S with parents ~S, dimensions ~S, df ~S.~%"
            (node-real-name node) (mapcar #'node-real-name (node-parents node))
            (array-dimensions (node-cpt node))
            (degrees-of-freedom (node-cpt node))))
  (ecase *datatype*
    (:csv
     (dolist (row (rows *data-source*))
       (increment-node-frequencies-for-row node row)))))


(defun increment-node-frequencies-for-row (node row)
  "Increment the frequency entries in NODE's frequency table, using the predicate values
for the current row/person."
  (block out
    (let ((indices '()))
      (flet ((add (node)
               (let* ((pos (or (column-position (node-upi node) *data-source*)
                               ;; We don't have a value for this so we can't increment a frequency.
                               ;; This probably shouldn't happen.
                               (return-from out)))
                      (value (aref row pos)))
                 (push (find-value-index value node) indices))))
        (dolist (parent (node-parents node))
          (add parent))
        (add node))
      (incf (apply #'aref (node-cpt node)
                   (nreverse indices))))))

(defun frequency-table-entropy (frequency-table)
  "Returns the entropy of a frequency table \(i.e. an n-dimensional array of frequencies)."
  (let* ((size (apply #'* (array-dimensions frequency-table)))
         (array (make-array size :displaced-to frequency-table))
         (total-frequency (loop for frequency across array sum frequency)))
    (if (zerop total-frequency)
        0
        (loop for frequency across array
           for p = (/ frequency total-frequency)
           unless (zerop p)
           sum (* p (log p))))))



(defun display-cpt (var node)
  (format t "~2%Variable: ~a~%" var)
  (let ((parents (node-parents node))
	(rev-parents (reverse (node-parents node))))
		 
    (dolist (parent parents)
      (format t "~10a " (truncatestr (node-name parent) 10)))
    (format t " | ")
    (format t "~10a~%" (node-name node))
		 

    (dolist (parent parents)
      (declare (ignore parent))
      (format t "~10a " ""))
		 
    (format t " | ")
		 
    (dolist (value (node-values node))
      (format t "~10a " value))
    (terpri)
		 
    (format t "--------------------------------------------------------------------------~%")
		 
    ; prepare to show all
    (dolist (parent parents)
      (setf (node-i parent) 0)
      (setf (node-max parent) (length (node-values parent))))
		 
    (block finish
		   
      (loop
	(let (indicies)
	  (dolist (parent parents)
	    ;(format t "~10a " (node-i parent))
	    (format t "~10a " (nth (node-i parent) (node-values parent)))
	    (push (node-i parent) indicies))
		     
	  (format t " | ")
		     
	  (dotimes (i (length (node-values node)))
	    (let ((count (apply #'aref (node-cpt node) (reverse (cons i indicies)))))
	      (if (floatp count)
		  (format t "~10,8f " count)
                  (format t "~10a " count))))
	  (terpri))
		     
	; advance to next one
	(block out
	  (dolist (parent rev-parents)
	    (if (>= (incf (node-i parent)) (node-max parent))
                (setf (node-i parent) 0)
		;; and loop around to incr next one
                (return-from out)))
		       
	  ; get here when we overflow off the end
	  (return-from finish))))))

		 
(defun truncatestr (str len)
  (if (> (length str) len)
      (format nil "~A.." (subseq str 0 (- len 2)))
      str))

|#
