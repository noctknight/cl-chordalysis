(in-package :chordalysis)

#|

This is a version of the Bayesian network generator that uses a CSV file instead of an
Allegrograph database as the data source. This should run a lot faster and it can be
useful for debugging or for trying out simple networks.

|#

#-laptop
(eval-when (compile load eval) (enable-!-reader))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Top level functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Top level functions you can run (see their documentation strings for more
details):

- RUN

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic data structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct frequency-info
  entropy
  total-frequency
  degrees-of-freedom)

(defclass graph (basic-graph)
  ((possible-edges :initform '() :accessor graph-possible-edges
                       :documentation "List of possible edges (GRAPH-EDGE structs) to add,
                       sorted by [1] enabledness [2] score [3] edge id.")
   (entropy-cache :initarg :entropy-cache :reader entropy-cache
                  :documentation "A hash-table from ordered lists of column numbers to
FREQUENCY-INFO structs.")))

(defstruct graph-edge 
  v1	         ; name of vertex 1 (a symbol)
  v2             ; name of vertex 2 (a symbol)
  id             ; a unique number (used for breaking ties consistently when sorting edges)
  (score 0)      ; same as delta-entropy; the edge with the best (i.e. highest) score will be added next
  (p-value 0)    ; used as termination criterion (stop when no edge with p-value lower than threshold)
  separators     ; list of vertices (i.e. symbols) separating V1 and V2.
  is-disabled    ; true if this edge is (temporarily) disabled because it would lead to a non-chordal graph
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Building a graph structure for data from the database.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-graph (&key (cutoff *max-distinct-values*))
  "Predicates that have more than CUTOFF distinct values are ignored."
  (let ((columns (select-predicates cutoff nil)))
    (mapc #'show-column columns)
    (make-instance 'graph
                   :entropy-cache (make-hash-table :test 'equal))))

(defun register-namespaces ()
  #-laptop
  (register-namespace "credoo" "http://credoo.com/"))

(defun show-column (column)
  (format t "~&Column ~S with ~D values and ~D unique values: ~S~%"
          (column-name column)
          (nr-values column)
          (nr-unique-values column)
          (unique-values column)))
      
(defun compute-edge-graph (&key (cutoff *max-distinct-values*))
  (ecase *datatype*
    (:csv (setq *data-source* (load-csv :path (merge-pathnames *db-name* cl-user::*base-dir*)))
          (clean-up-data *columns* *data-source*)
          (create-edge-graph-inner cutoff))
    (:allegrograph
     (open-triple-store *db-name*)
     ;; Get rid of nodes for visualizing the network.
     #-laptop(delete-triples :p !<http://credoo.com/connected>)
     ;;
     (unwind-protect
          (create-edge-graph-inner cutoff)
       (close-triple-store)))))

(defun create-edge-graph-inner (cutoff)
  (let ((columns (coerce (select-predicates cutoff nil) 'list)))
    (format t "~s vertices in the graph~%" (length columns))
    ;; Build an empty graph and create the initial possible edges.
    (let ((graph (build-graph :cutoff cutoff)))
      (format t "Generate edge list~%")
      ;; Initialize the possible edge list.
      (let* ((nr-edges 0)
             (edges (loop for (v1 . tail) on (mapcar #'column-name columns)
                       append (loop for v2 in tail
                                 collect (make-graph-edge :id (incf nr-edges)
                                                          :is-disabled nil
                                                          :v1 v1
                                                          :v2 v2)))))
        (setf (graph-possible-edges graph) edges)
        (sort-possible-edges graph)
        (format t "~&~D possible edges in the graph~%" (length edges))
        graph))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entropies and scores
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-edge-scores (graph)
  "Compute the score for all possible edges and then sort the graph's
possible edge list by their scores."
  (loop for i from 1
     for graph-edge in (graph-possible-edges graph)
     do (progn
          (format t "~%---- Initializing score for edge #~D from ~S to ~S (total: ~D edges) ----~%"
                  (graph-edge-id graph-edge)
                  (graph-edge-v1 graph-edge)
                  (graph-edge-v2 graph-edge)
                  (length (graph-possible-edges graph)))
          (update-score graph graph-edge)))
  (sort-possible-edges graph)
  graph)

(defun sort-possible-edges (graph)
  (setf (graph-possible-edges graph)
        (stable-sort (copy-list (graph-possible-edges graph))
                     (lambda (e1 e2)
                       (let ((score1 (graph-edge-score e1))
                             (score2 (graph-edge-score e2))
                             (disabled-1 (graph-edge-is-disabled e1))
                             (disabled-2 (graph-edge-is-disabled e2)))
                         (or
                          ;; Disabled edges should go to the bottom of the list.
                          (and (not disabled-1) disabled-2)
                          ;; If they're both enabled (or both disabled), look at the score
                          ;; and maybe at edge ID to break ties.
                          (and (eql disabled-1 disabled-2)
                               (or (> score1 score2)
                                   (and (= score1 score2)
                                        (< (graph-edge-id e1) (graph-edge-id e2)))))))))))


(defun degrees-of-freedom (table)
  (- (apply #'* (array-dimensions table))
     1))
    
(defun frequency-table-entropy-and-total (frequency-table nr-rows)
  "Returns [1] the entropy, and [2] the total frequency of a frequency table \(i.e. an
n-dimensional array of frequencies).
  NR-ROWS is the total number of rows (persons) in the database. This is used to scale
the entropy."
  (let* ((size (apply #'* (array-dimensions frequency-table)))
         (array (make-array size :displaced-to frequency-table :element-type 'fixnum))
         (log-n (log nr-rows))
         (total-frequency (loop for frequency across array sum frequency)))
    (if (zerop total-frequency)
        (values 0 0)
        (let* ((e (loop for frequency across array
                     for partial-entropy = (if (zerop frequency)
                                               0
                                               (* frequency (- (log frequency) log-n)))
                     sum partial-entropy))
               (entropy (- (/ e nr-rows))))
          (values entropy total-frequency)))))


(defun update-score (graph graph-edge)
  "Update delta-entropy and score for a graph-edge."
  (handler-case (multiple-value-bind (delta-entropy p-value)
                    (compute-delta-entropy-and-p-value-for-edge graph graph-edge)
                  (setf (graph-edge-score graph-edge) delta-entropy
                        (graph-edge-p-value graph-edge) p-value))
    (frequency-table-too-big ()
      ;; We can't really handle this so we give it an extremely bad score.
      (setf (graph-edge-score graph-edge) *worst-possible-score*
            (graph-edge-p-value graph-edge) *worst-possible-p-value*))))


(defun compute-delta-entropy-and-p-value-for-edge (graph graph-edge)
  "Returns [1] the score (i.e. the change in model entropy) and [2] the p-value for adding a
GRAPH-EDGE to the model."
  (let ((v1 (graph-edge-v1 graph-edge))
	(v2 (graph-edge-v2 graph-edge)))
    (format t "~%Recompute entropy and score for edge from ~s to ~s~%"
            v1 v2)
    (let ((seps (graph-edge-separators graph-edge)))
      (format t "~&Separators: ~S~%" seps)
      (multiple-value-bind (ha dfa)
          (entropy-and-freedom-and-total-for-vertices graph (cons v1 seps))
        (multiple-value-bind (hb dfb)
            (entropy-and-freedom-and-total-for-vertices graph (cons v2 seps))
          (multiple-value-bind (hab dfab nab)
              (entropy-and-freedom-and-total-for-vertices graph (list* v1 v2 seps))
            (multiple-value-bind (hsep dfsep)
                (if seps
                    (entropy-and-freedom-and-total-for-vertices graph seps)
                    (values 0 0))
              (format t "~&H(sep+A)=~S, H(sep+B)=~S, H(sep+AB)=~S, H(sep)=~S~%"
                      ha hb hab hsep)
              (format t "~&df(sep+A)=~S, df(sep+B)=~S, df(sep+AB)=~S, df(sep)=~S~%"
                      dfa dfb dfab dfsep)
              (format t "~&N = N(A+B) = ~S~%" nab)
              (let* ((delta-entropy (+ ha hb (- hab) (- hsep)))
                     (g (* 2 nab delta-entropy))
                     (delta-freedom (- (+ dfa dfb (- dfab) (- dfsep))))
                     (p-value (if (< delta-entropy 0)
                                  ;; This shouldn't happen but it does sometimes. Make sure
                                  ;; that these get to the bottom of the possible edge list.
                                  *worst-possible-p-value*
                                  (chi-square-p-value g delta-freedom))))
                (format t "~&Score = delta-entropy = dH = H(sep+A) + H(sep+B) - H(sep+A+B) - H(sep) = ~S~%"
                        delta-entropy)
                (format t "~&G = 2 * N * dH = ~S~%" g)
                (format t "~&delta freedom = -df(sep+A) - df(sep+B) + df(sep+AB) + df(sep) = ~S~%"
                        delta-freedom)
                (format t "~&p value of chi squared (G, delta freedom) = ~S~%"
                        p-value)
                (values delta-entropy p-value)))))))))
 
(defun entropy-and-freedom-and-total-for-vertices (graph vertices)
  "Returns [1] entropy and [2] degrees of freedom for the frequency table
that corresponds to VERTICES."
  (let* ((columns (columns-for-vertices vertices)) ; sorted
         (column-numbers (mapcar #'column-index columns)) 
         (cache (entropy-cache graph))
         (info (let ((cached (gethash column-numbers cache)))
                 (cond (cached
                        (format t "~&Using cached frequency info for ~S.~%" vertices)
                        cached)
                       (t
                        (format t "~&Computing frequency info for ~S.~%" vertices)
                        (setf (gethash column-numbers cache)
                              (compute-frequency-info graph columns)))))))
    (values (frequency-info-entropy info)
            (frequency-info-degrees-of-freedom info)
            (frequency-info-total-frequency info))))

(defun columns-for-vertices (vertices &key (sorted t))
  "Returns a list of columns for VERTICES. If SORTED is true \(which is the default), the
columns are sorted by column-index \(lowest index first)."
  (let ((columns (loop for v in vertices
                    collect (find-column-named v *data-source*))))
    (if sorted
        (sort columns #'< :key #'column-index)
        columns)))

(defun compute-frequency-info (graph columns)
  (declare (ignore graph)) ; later
  (let ((table (frequency-table-for-columns columns)))
    (multiple-value-bind (entropy total)
        (frequency-table-entropy-and-total table (nr-rows *data-source*))
      (make-frequency-info :entropy entropy
                           :total-frequency total
                           :degrees-of-freedom (degrees-of-freedom table)))))

(defun frequency-table-for-columns (columns)
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
      (dolist (row (rows *data-source*))
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

;;;
;;; Debugging
;;;

(defun display-scores (graph)
  (format t "~25a~25a~15a~15a~%" "id" "v1" "v2" "score  " "p-value")
  (dolist (e (graph-possible-edges graph))
    (format t "~25a~25a~9,7f      ~9,7f~%"
            (graph-edge-v1 e) (graph-edge-v2 e)
            (graph-edge-score e)
            (graph-edge-p-value e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Math/statistics functions (taken from clml library)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chi-square-p-value (x degrees-of-freedom)
  (let ((result (- 1.0d0 (chi-square-cdf x degrees-of-freedom))))
    (if (<= -1d-20 result 1d-20) ; prevent extremely 'small' numbers
        0.0
        #+nil(format t "~&P value of chi squared: ~S~%" result)
        result)))
  

(defun chi-square-cdf (x degrees-of-freedom)
  (handler-case (let ((k (* 1.0d0 degrees-of-freedom)))
                  (/ (statistics::lower-incomplete-gamma-half k (/ x 2.0d0))
                     (statistics::gamma-half k)))
    (floating-point-overflow (err)
      ;; Assume that this error occurs when the result is extremely
      ;; close to 0.  Some simple tests in Mathematica suggest that
      ;; this is the case when the degrees of freedom count is very
      ;; high.
      (declare (ignore err))
      (format t "~&Approximating chi square cdf to 0.0 because of floating point overflow.~%")
      0.0d0)
    (error (err)
      ;; The gamma functions can also throw an error. Presumably this
      ;; is also for results that are extremely close to 0.
      (declare (ignore err))
      (format t "~&Approximating chi square cdf to 0.0 because out of range for gamma function.~%")
      0.0d0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;; Dynamic edge addition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-adding-edges ()
  (when (eql :allegrograph *datatype*)
    (delete-triples :g (resource *result-graph*)) ; get rid of Gruff graph nodes
    (commit-triple-store)))

(defun add-best-edge (graph)
  (sort-possible-edges graph)
  (format t "~&~D possible edges (of which ~D disabled).~%"
          (length (graph-possible-edges graph))
          (length (remove-if-not #'graph-edge-is-disabled (graph-possible-edges graph))))
  (let ((e (pop (graph-possible-edges graph))))
    (cond ((adding-edge-is-ok graph
                              (graph-edge-v1 e) (graph-edge-v2 e))
           (add-edge graph e))
          (t
           (format t "~&Skipping edge ~S.~%"
                   (list (graph-edge-v1 e) (graph-edge-v2 e)))
           ;; Give edge a very bad score and add it back to the possible edge list so we
           ;; don't forget it and so that ENSURE-SEPARATOR-SET can find it.
           (setf (graph-edge-score e) *worst-possible-score*
                 (graph-edge-p-value e) *worst-possible-p-value*)
           (push e (graph-possible-edges graph))))))

(defun adding-edge-is-ok (graph v1 v2)
  "Returns true iff adding the edge v1->v2 keeps the graph chordal."
  (let ((simple-graph (simple-graph-copy graph)))
    (simple-add-edge simple-graph v1 v2)
    (is-chordal simple-graph)))

(defun simple-add-edge (graph v1 v2)
  "Add nodes V1 and V2 and edges V1->V2 and V2->V1 to a graph."
  (pushnew v1 (graph-nodes graph))
  (pushnew v2 (graph-nodes graph))
  (push (cons v1 v2) (graph-edges graph))
  (push (cons v2 v1) (graph-edges graph)))


(defun add-edge (graph graph-edge)
  (let ((v1 (graph-edge-v1 graph-edge))
        (v2 (graph-edge-v2 graph-edge)))
    (let ((message (format nil
                           "Adding edge #~D from ~S to ~S (with score = delta entropy = ~F and p-value = ~6,4f)"
                           (graph-edge-id graph-edge)
                           v1 v2
                           (graph-edge-score graph-edge)
                           (graph-edge-p-value graph-edge))))

      (format t "~%~A~%" message)

      (when (eql *datatype* :allegrograph)
        (add-triple (resource (string v1) "credoo")
                    (resource "connected" "credoo")
                    (resource  (string v2) "credoo")
                    :g (resource *result-graph*))
        (commit-triple-store))

      (simple-add-edge graph v1 v2)

      ;; For all edges that can be created in principle, check if [1] creating them is
      ;; possible at at all with the current nodes (if not, we don't need to do anything),
      ;; [2] creating them would create a non-chordal graph (if it would, we disable the
      ;; corresponding edge on the possible edge list), [3] the separators between v1 and
      ;; v2 has changed since last time (if so, we update the score).
      (loop for (v1 . tail) on (graph-nodes graph)
         do (dolist (v2 tail)
              (let* ((shortest-paths (shortest-paths* graph v1 v2))
                     (shortest-distance
                      ;; All paths are equally short, so we just take the length of the first.
                      (length (car shortest-paths))))
                (unless (<= shortest-distance 1) ; No link or directly connected: no need to do anything.
                  (let ((edge (find-possible-edge graph v1 v2)))
                    (when edge
                      (if (= 2 shortest-distance) 
                          ;; The shortest paths are of the form V1 - X - V2.
                          (let ((separator
                                 ;; Notes: [1] Some shortest paths can start with the same node,
                                 ;; so we need to remove duplicates. [2] This separator is a
                                 ;; minimal one because it's based on shortest paths. And it's
                                 ;; unique because we're only looking at paths with 1 intermediate
                                 ;; node between V1 and V2.
                                 (remove-duplicates (mapcar #'car shortest-paths))))
                            (ensure-separator-set graph edge separator))
                          ;; The shortest paths are of the form V1 - X - more nodes... - V2.
                          ;; This means that adding an edge V1-V2 would lead to a non-chordal
                          ;; graph (because it would get a cycle of length 4 or more), so we
                          ;; have to disable V1-V2 for now.
                          (disable-possible-edge edge))))))))

      ;; Show ordering and chordality.
      (let ((ordering (perfect-elimination-ordering (simple-graph-copy graph))))
        (format t "~%PEO: ~S~%" ordering)
        (format t "~&Graph chordality: ~S~%" (is-chordal (simple-graph-copy graph)))
        (format t "~&Graph has ~D edges: ~S~%"
                (length (graph-edges graph))
                (graph-edges graph)))

      (graphical-log message :graph graph))))

(defun ensure-separator-set (graph e separators)
  (let ((v1 (graph-edge-v1 e))
        (v2 (graph-edge-v2 e)))
    (when (null e)
      (error "there should be a possible graph edge from ~s to ~s" v1 v2))
    (when (graph-edge-is-disabled e)
      (format t "Bringing edge (~A ~A) back to possible~%" v1 v2))
    (setf (graph-edge-is-disabled e) nil)
    (when (set-difference separators (graph-edge-separators e))
      ;; The separator set between V1 and V2 has changed so we need to recompute.
      (format t "~&Separators between ~s and ~s changed from ~s to ~s~%"
              v1 v2 (graph-edge-separators e) separators)
      ;; Recompute score and update the position of this edge in the possible edge list.
      (setf (graph-edge-separators e) separators)
      (update-score graph e))))

(defun disable-possible-edge (edge)
  ;; Make the edge for (V1 . V2) disabled.
  (graphical-log (format nil "Moving edge (~A ~A) from possible to disabled edge list."
                         (graph-edge-v1 edge) (graph-edge-v2 edge)))
  (setf (graph-edge-is-disabled edge) t))

(defun find-possible-edge (graph v1 v2)
  "Returns a GRAPH-EDGE from POSSIBLE-EDGES or NIL."
  (find-if (lambda (e) (graph-edge-has-vertices e v1 v2))
           (graph-possible-edges graph)))

(defun graph-edge-has-vertices (graph-edge v1 v2)
  (or (and (eq (graph-edge-v1 graph-edge) v1)
           (eq (graph-edge-v2 graph-edge) v2))
      (and (eq (graph-edge-v1 graph-edge) v2)
           (eq (graph-edge-v2 graph-edge) v1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun select-predicates (cutoff names)
  "Returns a list of columns.  If NAMES is non-nil, it's a list with predicate names that
we're interested in \(so all other predicates are ignored).  CUTOFF is either NIL or an
integer. If it's an integer, predicates with more than CUTOFF possible values are
ignored."
  (flet ((is-irrelevant (name)
           (or (search "weibo" (string name) :test #'char-equal) ; we don't want weibo for some reason
               (and names
                    (not (find name names :test #'string-equal))))))
    (ecase *datatype*
      (:csv (loop for column across (columns *data-source*)
                 for name = (column-name column)
                 unless (or (is-irrelevant name)
                            (and cutoff (> (nr-unique-values column) cutoff)))
                 collect column))
      (:allegrograph
       (let ((preds (run-sparql "select distinct ?p where { ?s ?p ?o}"
                                :results-format :lists))
             (predicates-to-use))
         (dolist (pred preds)
           (setq pred (car pred))
           #-laptop
           (let ((name (part->terse pred)))
              (unless (or (part= !rdf:type pred)
                         (predicate-is-irrelevant pred))
               (let ((objects (make-hash-table :test #'equal)))
                 (tslet ((q -sub pred -obj))
                        (setf (gethash (upi->value obj) objects) t))
                 (let ((values (hash-table-keys objects)))
                   (when (or (null cutoff) (<= (length values) cutoff))
                     (push (cons pred (sort-values values)) predicates-to-use)))))))
         predicates-to-use)))))


(defun show-predicates (predicates)
  (dolist (predicatelist predicates)
    (destructuring-bind (pred &rest values) predicatelist
	(format t "Property: ~a  has ~d values~%"
                (if (eql *datatype* :csv)
                    pred
                    (part->terse pred))
                (length values))
      (let ((index 0))
	(dolist (val values)
	  (format t "~2d: ~s~%" index val)
	  (incf index)))
      (terpri))))
	

(defun sort-values (values)
  ;; if they look like numbers sort that way
  (sort values #'(lambda (a b)
		   (let ((int-a (ignore-errors (parse-integer a)))
			 (int-b (ignore-errors (parse-integer b))))
		     (if (and int-a int-b)
			 (<= int-a int-b)
			 (string<= a b))))))
			 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Output graph in formats that can be used by Bayesian net inference programs like PyMC
;;; or Jags.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric output-graph (graph format destination)
  (:documentation "Output GRAPH to DESTINATION in the specified FORMAT.  FORMAT is :JAGS
at the moment. DESTINATION is a stream designator."))

;;;
;;; Jags
;;;

(defmethod output-graph (graph (format (eql :jags)) stream)
  (output-jags graph stream))

(defun output-jags (graph &optional (stream t))
  (format stream "network_dict = {~%")
  (let ((nodes-and-tables
         (loop for v in (graph-nodes graph)
            for parents = (vertex-parents v graph)
            for columns = (columns-for-vertices (reverse (cons v parents)) :sorted nil)
            for table = (handler-case (frequency-table-for-columns columns)
                          (frequency-table-too-big ()
                            nil))
            when table
            collect (list v parents table))))
    (loop for (node-info . more) on nodes-and-tables
       for i from 1
       do (destructuring-bind (v parents table)
              node-info
            (output-node-to-jags v parents table stream i)
            (when more
              (format stream ","))
            (format stream "~%"))))
  (format stream "}~%"))

(defun output-node-to-jags (v parents table stream counter)
  (let ((possible-values (unique-values (find-column-named v *data-source*))))
    (format stream "  '~A': {~%" v)
    (format stream "    'index': ~D,~%" counter)
    (format stream "    'possibleValues': ~A,~%"
            (output-string-vector possible-values))
    (format stream "    'numberOfPossibleValues': ~D,~%" (length possible-values))
    (format stream "    'variableType': 'categorical',~%")
    (format stream "    'parents': ~A,~%"
            (output-string-vector parents))
    (format stream "    'probability': ")
    (display-nd-array table stream)
    (format stream "}")))

(defun output-string-vector (strings)
  (if strings
      (format nil "[~{'~A'~^, ~}]" strings)
      "None"))
                      
;;;
;;; Output array
;;;

(defun display-nd-array (arr &optional (stream t))
  (let ((dims (array-dimensions arr))
        (entry-count 0))
    (labels ((display (arr seen togo)
               (let ((max (pop togo))
                     (need-comma))
                 (cond ((null togo)
                        ;; At the bottom level, print out values.
                        (format stream "[")
                        (dotimes (i max)
                          (let ((val (apply #'aref arr (reverse (cons i seen)))))
                            (when need-comma
                              (format stream ", ")
                              (setq need-comma nil))
                            (format stream (if (integerp val) "~D" "~6,4f") val)
                            (incf entry-count)
                            (setq need-comma t)))
                        (format stream "]")
                        (when (> entry-count 20)
                          (terpri stream)
                          (setq entry-count 0)))
                       (t ;; Not at the bottom yet.
                        (format stream "[")
                        (dotimes (i max)
                          (when need-comma
                            (format stream ", ")
                            (setq need-comma nil))
                          (display arr (cons i seen) togo)
                          (setq need-comma t))
                        (format stream "]"))))))
      (display arr nil dims))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generating Bayesian network
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun undirected-to-bayesian (graph)
  "Returns a directed SIMPLE-GRAPH with the Bayesian network that's equivalent to the
given undirected GRAPH. GRAPH can be either a SIMPLE-GRAPH or a (full) GRAPH."
  ;; See slide on p.66 of Petitjean.
  (let* ((g (simple-graph-copy graph))
         (ordering (perfect-elimination-ordering g)))
    (flet ((pos (vertex)
             (position vertex (nodes ordering) :key #'temp-node-name)))
      (let ((new-edges (loop for (v1 . v2) in (graph-edges g)
                          when (< (pos v1) (pos v2))
                          collect (cons v1 v2))))
        (make-instance 'simple-graph
                       :nodes (graph-nodes g)
                       :edges new-edges)))))

(defun test-undirected-to-bayesian ()
  ;; This is the example from p.66 of Petitjean.  Our result is different from Petitjean's
  ;; example, but that's because there are many possible results and this function just
  ;; returns one of them.
  (let ((test-graph (test-graph-p66-petitjean :directed nil)))
    (undirected-to-bayesian test-graph)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generating and exporting models
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun equivalent-bayesian-model (model)
  (undirected-to-bayesian model))

(defun export-model (model filename)
  "Export a bayesian model to the specified file.
NODES that would have too big frequency tables are skipped."
  (with-open-file (out filename
                       :direction :output
                       :external-format :utf-8
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (output-graph model :jags out)
    (list model out)))

(defun generate-undirected-model (&key (max-iterations 15) (threshold *threshold*))
  
  "Generate and return a decomposable model (undirected graph) for the
triple store in *DB-NAME* on the local Allegrograph server. The system will
keep adding edges until either [1] MAX-ITERATIONS is reached, or [2]
there are no edges left with a score of at least SCORE-THRESHOLD.

  You can visualize the network that's being created by opening the
triple store in Gruff, going to View - Query View, running a query like

  select * where {
   graph <http://credoo.com/show> {?s ?p ?o}
   }

and then clicking on 'Create Visual Graph'."
  
  (let ((graph (compute-edge-graph)))
    #-laptop(open-triple-store *db-name*)
    (unwind-protect (progn
                      (initialize-edge-scores graph)
                      (display-scores graph)
                      (register-namespaces)
                      (start-adding-edges)
                      (loop for i from 1 to (or max-iterations 100000)
                         while (some (lambda (e) (<= (graph-edge-p-value e) threshold))
                                     (graph-possible-edges graph))
                         do (format t "~2%---- Step ~D ----~2%" i)
                           (add-best-edge graph))
                      ;; Get rid of nodes for visualizing the network.
                      #-laptop(delete-triples :p !<http://credoo.com/connected>))
      #+laptop :ignore
      #-laptop(close-triple-store))
    graph))

(defun run (&key (filename "graph.py") (skip-config nil))

  "Generate a Bayesian network (directed graph) for the data source in
in *DB-NAME* and write out the network specification to FILENAME.

  If SHOW-SCORES is true, the initial scores and entropies of all edges
will be printed to standard output.
 
  Loads configuration info from the file 'config.json' unless SKIP-CONFIG is true.

  You can visualize the network that's being created by opening the
triple store in Gruff, going to View - Query View, running a query like

  select * where {
   graph <http://credoo.com/show> {?s ?p ?o}
   }

and then clicking on 'Create Visual Graph'."

  (with-logging ()
    ;; Load configuration file if necessary.
    (unless skip-config
      (load-config))
    ;; Generate an undirected model, convert it to a Bayesian network and export that.
    (let* ((model (generate-undirected-model :max-iterations *max-iterations*
                                             :threshold *threshold*))
           (bayesian (equivalent-bayesian-model model)))
      (graphical-log "Bayesian equivalent" :graph bayesian :is-directed t)
      (setq *model* model) ; for debugging only
      (export-model bayesian
                    (make-pathname :name (format nil "~A-~A" (pathname-name filename) *log-timestamp*)
                                   :type (pathname-type filename)
                                   :defaults cl-user::*base-dir*)))))
