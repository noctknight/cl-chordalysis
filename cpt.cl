(eval-when (compile load eval)
  (require :agraph "/data/data12/zhengxin_pro/Heater/BIN/agraph-6.0.1/lib/agraph.fasl"))

(defpackage :user (:use :db.agraph :db.agraph.sparql))
(in-package :user)

(eval-when (compile load eval) (enable-!-reader))

(defparameter *db-name* "testdb")

(defparameter *result-graph* "http://credoo.com/show"
  "Only used for showing the generated model in Gruff.")

(defvar *model* nil
  "The current model. Used for debugging only.")

(defparameter *debug-all* t
  "Set to true if you want to see lots of debug information.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Top level functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Top level functions you can run (see their documentation strings for more
details):

- generate-undirected-model

- generate-bayesian-model

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Terminology
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

- vertex

  a symbol with the name of a node

- edge

  a cons (V1 . V2), where V1 is the parent and V2 is the child

- node

  a data structure that contains slots like name, frequency table,

... more ...

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic data structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Graph
;;

(defclass basic-graph ()
  ((nodes :initarg :nodes :initform '() :accessor graph-nodes
          :documentation "A list with vertex names (symbols).")
   (edges :initarg :edges :initform '() :accessor graph-edges
          :documentation "A list of cons (V1 . V2), meaning an edge pointing
from V1 to V2 (V1 is the parent, V2 the child). If the graph is undirected,
(v2 . v1) is also be present.")))

(defmethod print-object ((g basic-graph) stream)
  (print-unreadable-object (g stream :type t)
    (format stream "with ~D nodes ~S and ~D edges"
            (length (graph-nodes g))
            (graph-nodes g)
            (length (graph-edges g)))))

(defmethod vertex-parents ((v symbol) (g basic-graph))
  (loop for edge in (graph-edges g)
     when (eql v (cdr edge))
     collect (car edge)))

(defmethod vertex-children ((v symbol) (g basic-graph))
  (loop for edge in (graph-edges g)
     when (eql v (car edge))
     collect (cdr edge)))

(defmethod vertex-neighbors ((v symbol) (g basic-graph))
  (remove-duplicates (append (vertex-parents v g)
                             (vertex-children v g))))

(defmethod graph-description ((graph basic-graph))
  "Returns a graph description. A graph description is a list
of node specs where each node spec has the form (CHILD &REST PARENTS)."
  (loop for v in (graph-nodes graph)
     collect `(,v ,@(vertex-parents v graph))))

  
(defclass simple-graph (basic-graph)
  ((temp-nodes :accessor graph-temp-nodes
               :documentation "hash-table from vertices (symbols) to temp-nodes"))
  (:documentation "Basic graphs are used by some graph algorithms like chordality
checking."))

(defclass graph (basic-graph)
  ((ht :initarg :ht :accessor graph-ht
       :documentation " table of node name -> node")
   (origname-ht :initarg :origname-ht :accessor graph-origname-ht
                :documentation "table mapping original name to one of the extended names")
   (possible-edge-list :initform '() :accessor graph-possible-edge-list
                       :documentation "list of possible edges (GRAPH-EDGE structs) to add, sorted by score")
   (disabled-edge-list :initform '() :accessor graph-disabled-edge-list
                       :documentation "list of edges (GRAPH-EDGE structs) that may not be added")

   (frequency-table-cache :initform (make-hash-table) :reader graph-frequency-table-cache
                          :documentation "A mapping from nodes to their frequency tables.")))

;;
;; Node
;;

(defstruct temp-node ; used for some graph algorithms
  name ; a symbol (from the list of vertices)
  label ; a list of numbers, used during lexicographical breadth first search
  number ; used during LBFS
  ;; FOLLOWER is the temp-node with smallest number that is both neighbor of
  ;; this node and has number larger than this node (Tarjan 1984)
  follower
  ;; INDEX is used for Tarjan's zero fill-in check
  index
  )

(defstruct node
  name      ; terse string for node
  real-name ; a symbol (often a gensym)
  upi	 
  values ; list of values in sorted order 
  parents ; list of parent nodes
  cpt  	; conditional prob table
  
  ; for when it's used to display value
  i
  max
  
  ; for when writing out
  written
  )



(defun vertex-for-node (node graph &key errorp)
  (or (find (node-name node) (graph-nodes graph) :test #'string-equal)
      (when errorp
        (error "Can't find vertex for node ~S" node))))

;;
;; Helper functions/macros
;;

(defun simple-graph-version (graph)
  "Returns a copy of GRAPH that contains just (a copy of) the lists
with NODES and EDGES. This can be used for algorithms like graph
chordality check."
  (make-instance 'simple-graph
                 :nodes (copy-list (graph-nodes graph))
                 :edges (loop for (v1 . v2) in (graph-edges graph)
                             collect (cons v1 v2))))

(defun map-graph-node-names (graph function)
  "Calls FUNCTION with the name of each node in GRAPH."
  (loop for name being the hash-value of (graph-ht graph)
     do (funcall function name)))

(defmacro do-graph-node-names ((name graph) &body body)
  `(map-graph-node-names ,graph (lambda (,name) ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Little utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-equal (list-a list-b &key (test #'eql))
  (and (every (lambda (a) (find a list-b :test test))
              list-a)
       (every (lambda (b) (find b list-a :test test))
              list-b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Building a graph structure for data from the database.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-graph (&key (graphdesc nil) (db-name *db-name*)
		      (unique-nodes nil) (cutoff 1000))
  "If UNIQUE-NODES is true, we create unique nodes (gensym) for each node depending on
its parents. Predicates with more than CUTOFF distinct values are ignored."
  (open-triple-store db-name)
  ;; Get rid of nodes for visualizing the network.
  (delete-triples :p !<http://credoo.com/connected>)
  ;; The real work.
  (unwind-protect
       (do-build-graph graphdesc :unique-nodes unique-nodes :cutoff cutoff)
    (close-triple-store)))

(defun register-namespaces ()
  (register-namespace "credoo" "http://credoo.com/"))

(defun do-build-graph (graphdesc &key (unique-nodes nil) cutoff)
  (let ((predicatelists 
	 (mapcar #'(lambda (predlist)
		     ;; prepend the terse name to speed up
		     ;; selection
		     (cons (part->terse (car predlist))
			   predlist))
		 (do-select-predicates cutoff nil))))
    (format t "Predicatelists: ~S~%~%" predicatelists)
    (let ((nodetable (make-hash-table :test #'equal))
	  (origname  (make-hash-table :test #'equal)))
      (flet ((ensure (node-name)
               (ensure-node (uniqueify (string node-name) unique-nodes)
                            (string node-name)
                            nodetable predicatelists origname)))
        (dolist (gd graphdesc)
          (destructuring-bind (child-name &rest parents)
              gd
            (let ((child (ensure child-name)))
              (dolist (parent parents)
                (push (ensure parent) (node-parents child)))))))
      (make-instance 'graph
                     :ht nodetable :origname-ht origname
                     #| This breaks the model generation at the moment.
                     :nodes (mapcar #'first graphdesc)
                     :edges (loop for (child . parents) in graphdesc
                                 append (loop for p in parents
                                           append (list (cons child p)
                                                        (cons p child))))
                     |#
                     ))))

(defun ensure-node (name realname ht predlists origname-ht)
  ;; predlists is ("tersename" upi &rest values)
  (or (gethash name ht) ; already present
      (let ((predinfo (assoc realname predlists :test #'equal)))
	(when (null predinfo)
          (error "unknown predicate ~s~%" name))
	(setf (gethash realname origname-ht) name)
	(destructuring-bind (terse upi &rest values)
            predinfo
	  (setf (gethash realname origname-ht)
	    (setf (gethash name ht)
                  (make-node :name terse
                             :real-name realname
                             :upi  upi
                             :values values)))))))

(defun uniqueify (name uniquep)
  (if uniquep
      (string+ name "___" (gensym))
      name))

      
      
    
    
(defun initialize-cpts (graph)
  "Create initial cpt for each node in GRAPH."
  (do-graph-node-names (node graph)
    (init-cpt-for-node node)))


(defun init-cpt-for-node (node)
  ;; cpt will have
  ;;  this nodes values last
  (let (dims)
    (dolist (parent (node-parents node))
      (push (length (node-values parent)) dims))
    (push (length (node-values node)) dims)
    (setf (node-cpt node)
      (make-array (nreverse dims) :initial-element 0))))
    


(defun generate-cpt (graph &key (db-name *db-name*))
  (initialize-cpts graph)
  (open-triple-store db-name)
  (unwind-protect
      (do-generate-cpt graph)
    (close-triple-store)))

(defun do-generate-cpt (graph)
  (tslet ((q -person !rdf:type !<http://credoo.com/Person>))
    ;; Collect all info for person.
    (let (info)
      (tslet ((q person -pred -obj))
        (push (cons pred obj) info))
      (let ((*debug-all* nil))
        (do-graph-node-names (node graph)
          (do-generate-cpt-node node info))))))


(defun do-generate-cpt-one-node (node)
  (init-cpt-for-node node)
  (format t "~&Generating CPT for ~S with ~D parents, dimensions ~S, df ~S.~%"
          (node-real-name node) (length (node-parents node))
          (array-dimensions (node-cpt node))
          (degrees-of-freedom (node-cpt node)))
  (let ((preds (cons (node-upi node)
		     (mapcar #'node-upi (node-parents node)))))
    (tslet ((q -person !rdf:type !<http://credoo.com/Person>))
      ; Collect needed info for person.
      (let (info)
	(dolist (pred preds)
	  (tslet ((q person pred -obj))
	    (push (cons pred obj) info)))
	(do-generate-cpt-node node info)))))


(defun do-generate-cpt-node (node info)
  "Increment the frequency entries in NODE's frequency table, using the info
of the current person."
  (block out
    (let (dimsi)
      (flet ((add (vertex)
               (let ((val (assoc (node-upi vertex) info :test #'upi=)))
                 (if val 
                     (push (find-value-index (upi->value (cdr val)) vertex)
                           dimsi)
                     ;; Don't have a value for this.
                     (return-from out)))))
        (dolist (parent (node-parents node))
          (add parent))
        (add node))
      (setq dimsi (nreverse dimsi))
      (incf (apply #'aref (node-cpt node) dimsi)))))

(defun find-value-index (value node)
  "Return the index for a possible value of a node. Signals an error if
this is not possible."
  (or (position value (node-values node) :test #'equal)
      (error "value ~s is not valid for node ~s" value (node-name node))))


(defun change-counts-to-probabilities (graph)
  "Destructively convert all count tables of GRAPH's nodes to probability tables."
  (do-graph-node-names (node graph)
    (let ((parents (node-parents node))
          (rev-parents (reverse (node-parents node))))
      ;; Prepare to show all.
      (dolist (parent parents)
        (setf (node-i parent) 0)
        (setf (node-max parent) (length (node-values parent))))
          
      (block finish
            
        (loop
           (let (indicies)
             (dolist (parent parents)
               (push (node-i parent) indicies))

             (let ((count 0))
                   
               ;; Compute the total count.
               (dotimes (i (length (node-values node)))
                 (incf count (apply #'aref (node-cpt node)
                                    (reverse (cons i indicies)))))
                   
               (unless (zerop count)
                 (setq count (float count 1.0d0)))
                   
               ;; Convert counts to probabilities.
               (dotimes (i (length (node-values node)))
                 (if (zerop count)
                     ;; If we have no samples then assume a uniform distribution.
                     (let ((share (/ 1 (float (length (node-values node)) 1.0d0))))
                       (setf (apply #'aref (node-cpt node) (reverse (cons i indicies)))
                             share))
                     (let ((this (apply #'aref (node-cpt node) (reverse (cons i indicies)))))
                       (setf (apply #'aref (node-cpt node) (reverse (cons i indicies)))
                             (/ this count)))))))
               
           ;; Advance to next one.
           (block out
             (dolist (parent rev-parents)
               (if (>= (incf (node-i parent)) (node-max parent))
                   (setf (node-i parent) 0)
                   ;; and loop around to incr next one
                   (return-from out)))
                 
             ;; Get here when we overflow off the end.
             (return-from finish)))))))



(defun display-cpts (graph)
  (maphash #'(lambda (k node) (display-cpt k node))
           (graph-ht graph)))


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
	    (push (node-i parent) indicies)
	    )
		     
	  (format t " | ")
		     
	  (dotimes (i (length (node-values node)))
	    (let ((count (apply #'aref (node-cpt node) (reverse (cons i indicies)))))
	      (if* (floatp count)
		 then (format t "~10,8f " count)
		 else
		      (format t "~10a " count))))
	  (terpri))
		     
	; advance to next one
	(block out
	  (dolist (parent rev-parents)
	    (if* (>= (incf (node-i parent)) (node-max parent))
	       then (setf (node-i parent) 0)
		    ; and loop around to incr next one
	       else (return-from out)))
		       
	  ; get here when we overflow off the end
	  (return-from finish))))))
				 
		   
		 
(defun truncatestr (str len)
  (if (> (length str) len)
      (string+ (subseq str 0 (- len 2)) "..")
      str))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating edge info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct graph-edge 
  v1	; name of v1 (a symbol)
  v2    ; name of v2 (a symbol)
  node  ; child node holding cpt
  entropy ; this is actually the delta-entropy
  score   ;
  separators ; list of nodes separating V1 and V2.
  )


(defun compute-edge-graph (&key (cutoff 1000))
  (open-triple-store *db-name*)
  ;; Get rid of nodes for visualizing the network.
  (delete-triples :p !<http://credoo.com/connected>)
  ;;
  (unwind-protect
      (create-edge-graph-inner cutoff)
    (close-triple-store)))


(defun create-edge-graph-inner (cutoff)
  (let ((preds (mapcar #'(lambda (preddesc) (intern (part->terse (car preddesc))))
                       (do-select-predicates cutoff nil))))
    (format t "~s vertices in the graph~%" (length preds))
    (let (graphdesc)
      ;; Create a list with all the potential edges.
      (do ((pp preds (cdr pp)))
	  ((null (cdr pp)))
	(do ((qq (cdr pp) (cdr qq)))
	    ((null qq))
	  (push (list (car pp) (car qq)) graphdesc)))

      (format t "~s possible edges in the graph~%" (length graphdesc))
      
      (format t "Build graph~%")
      (let ((graph (build-graph :graphdesc graphdesc :unique-nodes t)))
	
	(format t "Generate CPTs~%")
	(generate-cpt graph) ; keep counts, don't convert to probabilities
	
	(format t "Generate edge list~%")
	(let (graph-edges)
	  (do-graph-node-names (node graph)
            (when (node-parents node)
              ;; a child with cpt
              (push (make-graph-edge :v1 (intern (node-name node))
                                     :v2 (intern (node-name (car (node-parents node))))
                                     :node node)
                    graph-edges)))
	  (setf (graph-possible-edge-list graph) graph-edges)
	  graph)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entropies and scores
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-edge-scores (graph)
  "Compute the score for all possible edges and then sort the graph's
possible edge list by their scores."
  (loop for i from 1
     for graph-edge in (graph-possible-edge-list graph)
     do (progn
          (format t "~%---- Initializing score for edge #~D (out of ~D) ----~%"
                  i
                  (length (graph-possible-edge-list graph)))
          (update-score graph graph-edge)))
  (setf (graph-possible-edge-list graph)
        (sort (graph-possible-edge-list graph) #'< :key #'graph-edge-score))
  graph)

(defun initialize-edge-score (graph graph-edge)
  (let ((v1 (graph-edge-v1 graph-edge))
        (v2 (graph-edge-v2 graph-edge)))
  (multiple-value-bind (hab dfab nab)
      (entropy-and-freedom-and-total-for-vertices graph (list v1 v2))
    (let* ((g (* 2 nab hab))
           (score (chi-square-p-value g dfab)))
      (setf (graph-edge-entropy graph-edge) hab
            (graph-edge-score graph-edge) score)))))

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

(defun degrees-of-freedom (table)
  (- (apply #'* (array-dimensions table))
     1))
    
(defun frequency-table-entropy-and-total (frequency-table)
  "Returns [1] the entropy, and [2] the total frequency of a
frequency table \(i.e. an n-dimensional array of frequencies)."
  (let* ((size (apply #'* (array-dimensions frequency-table)))
         (array (make-array size :displaced-to frequency-table))
         (total-frequency (loop for frequency across array sum frequency)))
    (if (zerop total-frequency)
        (values 0 0)
        (let ((entropy (loop for frequency across array
                          for p = (/ frequency total-frequency)
                          unless (zerop p)
                          sum (* p (log p)))))
          (values (- entropy) total-frequency)))))


(defun update-score (graph graph-edge)
  "Update delta-entropy and score for a graph-edge."
  (multiple-value-bind (delta-entropy score)
      (compute-delta-entropy-and-score-for-edge graph graph-edge)
    (setf (graph-edge-entropy graph-edge) delta-entropy
          (graph-edge-score graph-edge) score)))


(defun compute-delta-entropy-and-score-for-edge (graph graph-edge)
  "Returns [1] the change in model entropy and [2] the score for adding a
GRAPH-EDGE to the model."
  (let ((v1 (graph-edge-v1 graph-edge))
	(v2 (graph-edge-v2 graph-edge)))
    (format t "~%~%Recompute entropy and score for edge from ~s to ~s~%"
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
              (format t "~&H(A) = ~S H(B)=~S H(AB)=~S H(sep)=~S~%"
                      ha hb hab hsep)
              (format t "~&df(A) = ~S df(B)=~S df(AB)=~S df(sep)=~S~%"
                      dfa dfb dfab dfsep)
              (format t "~&N = N(A+B) = ~S~%" nab)
              (let* ((delta-entropy (+ ha hb (- hab) (- hsep)))
                     (g (* 2 nab delta-entropy))
                     (delta-freedom (- (+ dfa dfb (- dfab) (- dfsep))))
                     (score (if (< delta-entropy 0)
                                ;; This shouldn't happen but it does sometimes.
                                ;; Return a big positive score to make sure these
                                ;; are at the bottom of the list.
                                1000000000
                                (chi-square-p-value g delta-freedom))))
                #|
                (when (< delta-entropy 0)
                  ;; This shouldn't happen but it does happen so we have to investigate.
                  (break "Delta entropy = ~D edge=~S~%table(A)=~S~%table(B)=~S~%table(A+B)=~S"
                         delta-entropy graph-edge
                         (frequency-table-for-vertices graph (cons v1 seps))
                         (frequency-table-for-vertices graph (cons v2 seps))
                         (frequency-table-for-vertices graph (list* v1 v2 seps))))
                |#
                (format t "~&Delta-entropy = dH = H(A) + H(B) - H(A+B) - H(sep) = ~S~%"
                        delta-entropy)
                (format t "~&G = 2 * N * dH =~S~%" g)
                (format t "~&delta freedom = -df(A) - df(B) + df(A+B) + df(sep) = ~S~%"
                        delta-freedom)
                (format t "~&score = p value of chi squared (g, delta freedom) = ~S~%"
                        score)
                (values delta-entropy score)))))))))
 
(defun entropy-and-freedom-and-total-for-vertices (graph vertices)
  "Returns [1] entropy and [2] degrees of freedom for the frequency table
that corresponds to VERTICES."
  (let ((table (frequency-table-for-vertices graph vertices)))
    (entropy-and-freedom-and-total-for-frequency-table table)))

(defun frequency-table-for-vertices (graph vertices)
  ;; DO: Make a non-destructive version of this!
  (let ((child (gethash (string (car vertices)) (graph-origname-ht graph)))
        (parents (mapcar #'(lambda (v) (gethash (string v) (graph-origname-ht graph)))
                         (cdr vertices))))
    (setf (node-parents child) parents)
    (do-generate-cpt-one-node child)
    (node-cpt child)))

  
(defun entropy-and-freedom-and-total-for-frequency-table (table)
  (multiple-value-bind (entropy total)
      (frequency-table-entropy-and-total table)
    (values entropy (degrees-of-freedom table) total)))

;;;
;;; Debugging
;;;

(defun display-scores (graph)
  (format t "~25a~25a~15a~15a~%" "v1" "v2" "entropy" "score")
  (dolist (graph-edge (graph-possible-edge-list graph))
    (format t "~25a~25a~9,7f~9,7f~%"
            (graph-edge-v1 graph-edge) (graph-edge-v2 graph-edge)
            (graph-edge-entropy graph-edge)
            (graph-edge-score graph-edge))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Math/statistics functions (taken from clml library)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chi-square-p-value (x degrees-of-freedom)
  (let ((result (- 1.0d0 (chi-square-cdf x degrees-of-freedom))))
    #+nil(format t "~&P value of chi squared: ~S~%" result)
    result))
  

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
;;;;; dynamic edge addition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-adding-edges ()
  (delete-triples :g (resource *result-graph*)) ; get rid of Gruff graph nodes
  (commit-triple-store))

(defun add-best-edge (graph)
  (let ((e (pop (graph-possible-edge-list graph))))
    (cond ((adding-edge-is-ok graph
                              (graph-edge-v1 e) (graph-edge-v2 e))
           (add-edge graph e))
          (t
           (format t "~&Skipping edge ~S.~%"
                   (list (graph-edge-v1 e) (graph-edge-v2 e)))
           ;; Add edge back at the end so we don't forget it and
           ;; so that ENSURE-SEPARATOR-SET can find it.
           (setf (graph-possible-edge-list graph)
                 (append (graph-possible-edge-list graph)
                         (list e)))))))

(defun adding-edge-is-ok (graph v1 v2)
  "Returns true iff adding the edge v1->v2 keeps the graph chordal."
  (let ((simple-graph (simple-graph-version graph)))
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
    (format t "~%Adding edge from ~S to ~S (with delta entropy ~6,4f and score ~6,4f)"
            v1 v2
            (graph-edge-entropy graph-edge)
            (graph-edge-score graph-edge))
      
    (add-triple (resource (string v1) "credoo")
                (resource "connected" "credoo")
                (resource  (string v2) "credoo")
                :g (resource *result-graph*))
    (commit-triple-store)

    (simple-add-edge graph v1 v2)

    ;; Find minimal separators.
    (do ((vv (graph-nodes graph) (cdr vv)))
        ((null (cdr vv)))
      (do ((xx (cdr vv) (cdr xx)))
          ((null xx))
        (let ((v1 (car vv))
              (v2 (car xx)))
          (let* ((sps (shortest-paths* graph v1 v2))
                 (len-sp (length (car sps))))
            (if* (<= len-sp 1)
		 thenret               ; no link or directly connected
                 elseif (eq 2 len-sp)
		 then (ensure-separator-set graph 
					    v1
					    v2
					    (mapcar #'car sps) ; separators for each path
					    )
		 else (disable-possible-edge graph v1 v2)))))))

  ;; Show ordering and chordality.
  (let ((ordering (perfect-elimination-ordering (simple-graph-version graph))))
    (format t "~%PEO: ~S~%" ordering)
    (format t "Graph chordality: ~S" (is-chordal (simple-graph-version graph)))))
	

(defun ensure-separator-set (graph v1 v2 separators)
  (multiple-value-bind (graph-edge disabled)
      (locate-possible-edge graph v1 v2)
    (when (null graph-edge)
      (error "there should be a possible graph edge from ~s to ~s" v1 v2))
    (cond ((set-difference separators (graph-edge-separators graph-edge))
           (format t "between ~s and ~s separators changed from ~s to ~s~%"
                   v1 v2 (graph-edge-separators graph-edge) separators)
           (remove-graph-edge-from-possible graph graph-edge disabled)
           (setf (graph-edge-separators graph-edge) separators)
           (update-score graph graph-edge)
           (insert-graph-edge-in-possible graph graph-edge))
          (disabled
           ;; then just enable it
           (format t "bringing edge ~s ~s back to possible~%"
                   (graph-edge-v1 graph-edge)
                   (graph-edge-v2 graph-edge))
           (remove-graph-edge-from-possible graph graph-edge disabled)
           (insert-graph-edge-in-possible graph graph-edge)))))


(defun disable-possible-edge (graph v1 v2)
  ;; mark this edge as disabled
  (let ((edge (remove-edge-from-possible graph v1 v2)))
    (when edge
      (push edge (graph-disabled-edge-list graph)))))

(defun remove-edge-from-possible (graph v1 v2)
  "Return the possible edge graph-edge object and splice out of the possible list.
May return NIL."
  (let ((graph-edge (find-if (lambda (e) (graph-edge-has-vertices e v1 v2))
                             (graph-possible-edge-list graph))))
    (when graph-edge
      (setf (graph-possible-edge-list graph)
            (remove graph-edge (graph-possible-edge-list graph) :count 1))
      graph-edge)))

(defun graph-edge-has-vertices (graph-edge v1 v2)
  (or (and (eq (graph-edge-v1 graph-edge) v1)
           (eq (graph-edge-v2 graph-edge) v2))
      (and (eq (graph-edge-v1 graph-edge) v2)
           (eq (graph-edge-v2 graph-edge) v1))))

(defun remove-graph-edge-from-possible (graph graph-edge disabled)
  (if disabled
     (setf (graph-disabled-edge-list graph)
           (delete graph-edge (graph-disabled-edge-list graph)))
     (setf (graph-possible-edge-list graph)
           (delete graph-edge (graph-possible-edge-list graph)))))

(defun insert-graph-edge-in-possible (graph graph-edge)
  "Insert graph-edge, ordering by score."
  (let ((score (graph-edge-score graph-edge)))
    (do ((prev nil edges)
	 (edges (graph-possible-edge-list graph) (cdr edges)))
	((null edges)
	 (if prev
             (setf (cdr prev) (list graph-edge))
	     (setf (graph-possible-edge-list graph) 
		   (list graph-edge))))
      (let ((this-graph-edge (car edges)))
	(when (> score (graph-edge-score this-graph-edge))
          ;; insert here
          (if prev
              (setf (cdr prev)
                    (cons graph-edge (cdr prev)))
              (push graph-edge
                    (graph-possible-edge-list graph)))
          (return))))))
				
			  
    
(defun locate-possible-edge (graph v1 v2)
  ;; return values
  ;;   edge
  ;;   t if disabled
  (do ((edges (graph-possible-edge-list graph) (cdr edges)))
      ((null edges))
    (let ((graph-edge (car edges)))
      (when (graph-edge-has-vertices graph-edge v1 v2)
        (return-from locate-possible-edge graph-edge))))
  
  (do ((edges (graph-disabled-edge-list graph) (cdr edges)))
      ((null edges))
    (let ((graph-edge (car edges)))
      (when (graph-edge-has-vertices graph-edge v1 v2)
        (return-from locate-possible-edge 
          (values graph-edge t))))))
  	    
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun inspect-predicates (&key (cutoff 10) names)
  "Shows some details for all predicates. If NAMES is supplied, it's
a list of predicate names. Otherwise you get to see them all."
  (open-triple-store *db-name*)
  (delete-triples :p !<http://credoo.com/connected>)
  (commit-triple-store)
  (unwind-protect
      (show-predicates (do-select-predicates cutoff names))
    (close-triple-store)))

(defun do-select-predicates (cutoff names)
  ;; return list of 
  ;; (predicate-upi value1 ... valuen)
  (let ((preds (run-sparql "select distinct ?p where { ?s ?p ?o}"
                           :results-format :lists))
	(predicates-to-use))
    (dolist (pred preds)
      (setq pred (car pred))
      (let ((name (part->terse pred)))
	(unless (or (part= !rdf:type pred)
		    (and names (not (find name names :test #'equal)))
                    ;; we don't want weibo for some reason
                    (search "weibo" name))
          (let ((objects (make-hash-table :test #'equal)))
            (tslet ((q -sub pred -obj))
              (setf (gethash (upi->value obj) objects) t))
            (let ((values (hash-table-keys objects)))
              (when (or (null cutoff) (<= (length values) cutoff))
                (push (cons pred (sort-values values)) predicates-to-use)))))))
    predicates-to-use))


(defun show-predicates (predicates)
  (dolist (predicatelist predicates)
    (destructuring-bind (pred &rest values) predicatelist
	(format t "Property: ~a  has ~d values~%" (part->terse pred) (length values))
      (let ((index 0))
	(dolist (val values)
	  (format t "~2d: ~s~%" index val)
	  (incf index)))
      (terpri))))
	

(defun hash-table-keys (h)
  (loop for key being the hash-key of h
     collect key))


(defun sort-values (values)
  ;; if they look like numbers sort that way
  (sort values #'(lambda (a b)
		   (let ((int-a (ignore-errors (parse-integer a)))
			 (int-b (ignore-errors (parse-integer b))))
		     (if (and int-a int-b)
			 (<= int-a int-b)
			 (string<= a b))))))
			     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Database statistics, showing counts per predicate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stats ()
  (open-triple-store *db-name*)
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
    (add-triple item !rdf:type !<http://credoo.com/Person>))
  (commit-triple-store))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minimal separators, shortest paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These algorithms work on instances of BASIC-GRAPH.

(defun minimal-separator (graph v1 v2)
  "Returns a list of vertices that separate V1 from V2. (So if those
vertices are all removed from the graph, there will be no path between
V1 and V2.)"
  ;; DO: Check that this is a minimal separator and not just a
  ;; separator.
  (mapcar #'car (shortest-paths graph v1 v2)))


(defun test-graph-p66-petitjean (&key directed)
  (make-instance 'simple-graph
                 :nodes '(a b c d e f g h i)
                 :edges (let ((one-way-edges '((d . a) (d . b)
                                               (c . a) (c . b) (c . d)
                                               (e . c)
                                               (f . e)
                                               (g . e) (g . f)
                                               (h . f)
                                               (i . g))))
                          (if directed
                              one-way-edges
                              (append one-way-edges
                                      (loop for (v1 . v2) in one-way-edges
                                           collect (cons v2 v1)))))))
                          

(defun test-shortest-paths ()
  (flet ((is-ok (g v1 v2 paths)
           (let ((result (shortest-paths g v1 v2)))
             (unless (set-equal paths result :test #'equalp)
               (error "The shortest path between ~S and ~S is ~S but should be ~S"
                      v1 v2 result paths)))))
    (let ((g (test-graph-p66-petitjean :directed nil)))
      (is-ok g 'd 'b '((d b)))
      (is-ok g 'd 'c '((d c)))
      (is-ok g 'a 'b '((a d b) (a c b)))
      (is-ok g 'd 'h '((d c e f h))))
    (let ((g2 (test-graph-p66-petitjean :directed t)))
      (is-ok g2 'e 'd '((e c d)))
      (is-ok g2 'g 'e '((g e)))
      (is-ok g2 'c 'a '((c a)))))
  :ok)
    

                                  
(defun shortest-paths (graph v1 v2)
  "Returns a list of the shortest paths from V1 to V2. Each path is a
list containing the vertices on the path to V2. 
V1 is included in the list."
  (loop for p in (shortest-paths* graph v1 v2)
     collect (cons v1 p)))
   
(defun shortest-paths* (graph v1 v2)
  "Returns a list of the shortest paths from V1 to V2. Each path is a
list containing the vertices on the path to V2. 
V1 is not included in the list."
  (dolist (edge (graph-edges graph)
           ;; no direct link so do it the hard way
           (mapcar #'(lambda (revpath) (cdr (reverse revpath)))
                   (shortest-paths-do graph
                                      `((,v1))
                                      v2)))
    (when (and (eq v1 (car edge))
               (eq v2 (cdr edge)))
      (return (list (list v2))))))


(defun shortest-paths-do (graph paths v2)
  (let ((newpaths '())
        (edges (graph-edges graph))
        (final nil))
    (loop
      (dolist (path paths)
	(let ((v1 (car path)))
	  (dolist (edge edges)
	    (when (and (eq v1 (car edge))
                       (not (member (cdr edge) path)))
              (cond ((eq (cdr edge) v2)
		     ;; final vertex
                     (when (not final)
                       ;; forget anything we've seen
                       (setq newpaths nil))
                     (setq final t)
                     (push (cons (cdr edge) path) newpaths)
                     (return)) ; no more looking at edges
                    ((not final)
                     ;; still searching, save this
                     (push (cons (cdr edge) path) newpaths))
                    (t ;; not a winner so ignore
                     :ignore))))))
      (when final
        (return-from shortest-paths-do newpaths))
      
      (if newpaths 
	  (setq paths newpaths
                newpaths nil)
          (return)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chordality, minimal separators, maximal cliques,
;;; perfect elimination ordering.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

References:

- Berry and Pogorelcnik, 2010: "A simple algorithm to generate the minimal
  separators and the maximal cliques of a chordal graph."

- Tarjan, 1984: "Simple linear-time algorithms to test chordality of
  graphs, test acyclicity of hypergraphs, and selectively reduce
  acyclic hypergraphs"

Tarjan 1984:"lexicographical search, defined as follows: For each
unnumbered vertex V, maintain a list of the numbers of the numbered
vertices adjacent to V, with the numbers in each list arranged in
decreasing order. As the next vertex to number, select the vertex
whose list is lexicographically greatest, breaking ties arbitrarily."

Tarjan 1984: "For any vertex V, the 'follower' of V is the vertex
of smallest number that is both adjacent to V and has number larger
than that of V."

|#

(defclass ordering ()
  ((nodes :initarg :nodes :accessor nodes
          :documentation "A vector of temp-nodes.")
   (name-table :initarg :name-table :reader name-table
               :documentation "A mapping of temp-node-names
(symbols) to temp-nodes.")))

(defmethod print-object ((ordering ordering) stream)
  (print-unreadable-object (ordering stream :type t)
    (format stream "~S"
            (loop for node across (nodes ordering)
                 collect (list (temp-node-name node) (temp-node-label node))))))

(defmethod find-node ((name symbol) (ordering ordering))
  (or (gethash name (name-table ordering))
      (error "Can't find node ~S in ~S" name ordering)))

(defun is-chordal (graph)
  (let ((ordering (lex-breadth-first-search graph)))
    (is-zero-fill-in graph ordering)))

(defun is-zero-fill-in (graph ordering)
  "Given an ordering for a graph, returns true iff the graph is
zero fill-in. See Tarjan 1984."
  (let ((peo (nodes ordering)))
    ;; "for i in [1,n]"
    (dotimes (i (length peo))
      ;; "w:=a-1(i); f(w):=w; index(w):=i;"
      (let ((w (elt peo i)))
        (setf (temp-node-follower w) w
              (temp-node-index w) i)
        ;; "for v such that {v,w} in E and a(v)<i" 
        (loop for (v-name . w-name) in (graph-edges graph)
           do (let ((v (find-node v-name ordering))
                    (v2 (find-node w-name ordering)))
                (when (eq v2 w)
                  (when (< (temp-node-number v) i)
                    ;; "index(v):=i; if f(v)=v -> f(v):=w"
                    (setf (temp-node-index v) i)
                    (when (eql (temp-node-follower v) v)
                      (setf (temp-node-follower v) w))))))
        ;; "for v such that {v,w} in E and a(v)<i"
        (loop for (v-name . w-name) in (graph-edges graph)
           do (let ((v (find-node v-name ordering))
                    (v2 (find-node w-name ordering)))
                (when (eq v2 w)
                  (when (< (temp-node-number v) i)
                    ;; "if index(f(v))<i -> reject fi"
                    (when (< (temp-node-index (temp-node-follower v)) i)
                      (return-from is-zero-fill-in
                        (values nil v (temp-node-follower v)))))))))))
  ;; "accept"
  t)


(defun perfect-elimination-ordering (graph)
  "Returns a perfect elimination ordering for a graph. Assumes that
the graph is chordal."
  (lex-breadth-first-search graph))

(defun lex-breadth-first-search (graph)
  "Returns an ORDERING for GRAPH in lexicographical breadth first order.
If GRAPH is chordal, the result is a 'perfect elimination ordering'."
  (let ((vertex-table (make-hash-table))
	(nodes '()))
    (dolist (vertex (graph-nodes graph))
      ;; "Initialize all labels as the empty string." (we use NIL instead)
      (let ((temp (make-temp-node :label nil :name vertex :number nil)))
	(setf (gethash vertex vertex-table) temp)
	(push temp nodes)))
    (setf (graph-temp-nodes graph) vertex-table)
    ;;
    (let* ((nr-nodes (length nodes))
	   (vector (make-array nr-nodes))
           (name-table (make-hash-table :size nr-nodes)))
      (loop for i from (1- nr-nodes) downto 0
	 do (let* ((node (pick-maximal-node nodes))
                   (name (temp-node-name node)))
	      (setf (elt vector i) node
		    (temp-node-number node) i
                    (gethash name name-table) node)
	      ;; "For each unnumbered vertex w adjacent to v
              (dolist (edge (graph-edges graph))
                (let ((v1 (car edge)))
                  (when (eq name v1)
                    (let* ((w-name (cdr edge))
                           (w (gethash w-name vertex-table)))
                      (assert w)
                      (when (not (temp-node-number w))
                        ;; "append i to label(w)"
                        (setf (temp-node-label w)
                              (append (temp-node-label w) (list i))))))))))
      ;; Return result
      (make-instance 'ordering :nodes vector :name-table name-table))))

(defun pick-maximal-node (nodes)
  ;; Berry: "Pick an unnumbered vertex whose label is maximal under
  ;; lexicographic order."
  (let ((max-node nil)
	(max-label nil))
    (dolist (node nodes)
      (when (null (temp-node-number node))
	(when (or (null max-label)
		  (label-greater (temp-node-label node) max-label))
	  (setq max-label (temp-node-label node)
		max-node node))))
    max-node))


(defun label-greater (label-a label-b)
  (let ((length-a (length label-a))
	(length-b (length label-b)))
    (loop for i from 0
       do (cond ((>= i length-a)
		 (return-from label-greater nil))
		((>= i length-b)
		 (return-from label-greater t))
		((> (elt label-a i) (elt label-b i))
		 (return-from label-greater t))
		((< (elt label-a i) (elt label-b i))
		 (return-from label-greater nil))))
    nil))

(defun label<= (label-a label-b)
  (not (label-greater label-a label-b)))

(defun find-min-separators-and-max-cliques (peo graph)
  "Given a temp-node array in perfect elimination order (as
returned by lex-breadth-first-search), returns a list with:
[1] a list with minimal separators, [2] a list with maximal cliques."
  (let ((min-seps '())
	(max-cliques (list (elt peo 0))))
    (loop for i below (1- (length peo))
       do (let* ((xi (elt peo i))
		 (xi+1 (elt peo (+ i 1)))
		 (label-i (temp-node-label xi))
		 (label-i+1 (temp-node-label xi+1)))
	    (when (label<= label-i label-i+1)
              (let ((min-sep (m-adjacent graph xi)))
                (when min-sep
                  (push min-sep min-seps)))
	      (push (cons xi+1 (m-adjacent graph xi+1)) max-cliques))))
    (list min-seps max-cliques)))

(defun temp-node-neighbors (graph temp-node)
  "Returns a list of temp-nodes that are neighbors of the given temp-node."
  (let ((name (temp-node-name temp-node))
        (temp-nodes (graph-temp-nodes graph)))
    (loop for (v1 . v2) in (graph-edges graph)
       when (eql v1 name)
       collect (let ((neighbor (gethash v2 temp-nodes)))
                 (assert neighbor)
                 neighbor))))

(defun m-adjacent (graph vertex)
  "This is Madj in paper by Berry."
  ;; Berry: "Madj(x) is the set of neighbors of x with a number higher
  ;; than that of x."
  (loop for neighbor in (temp-node-neighbors graph vertex)
     when (> (temp-node-number neighbor) (temp-node-number vertex))
     collect neighbor))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Output graph in formats that can be used by Bayesian net inference
;;; programs like PyMC or Jags.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric output-graph (graph format destination)
  (:documentation "Output GRAPH to DESTINATION in the specified FORMAT.
FORMAT is either :PYMC or :JAGS at the moment. DESTINATION is a stream
designator."))

(defun find-node-with-parents (vertex parent-vertices graph)
  "Returns a node in GRAPH whose name is VERTEX and with parents
of which the names match PARENT-VERTICES. Returns NIL if such a
node can't be found."
  (loop for node being the hash-value of (graph-ht graph)
     when (and (string-equal (node-name node) vertex)
               (set-equal parent-vertices
                          (mapcar #'node-name (node-parents node))
                          :test #'string-equal))
     do (return-from find-node-with-parents node)))

;;;
;;; Jags
;;;

(defmethod output-graph (graph (format (eql :jags)) stream)
  (output-jags graph stream))

(defun output-jags (graph &optional (stream t))
  (format stream "network_dict = {~%")
  (let ((nodes (loop for node being the hash-value of (graph-ht graph)
                    collect node)))
    (loop for (node . more) on nodes
       for i from 1
       do (progn
            (output-node-to-jags node stream i)
            (when more
              (format stream ","))
            (format stream "~%"))))
  (format stream "}~%"))


(defun output-node-to-jags (node stream counter)
  (let ((possible-values (node-values node)))
    (format stream "  '~A': {~%" (node-name node)) ; terse name, not internal name
    (format stream "    'index': ~D,~%" counter)
    (format stream "    'possibleValues': ~A,~%"
            (output-string-vector possible-values))
    (format stream "    'numberOfPossibleValues': ~D,~%" (length possible-values))
    (format stream "    'variableType': 'categorical',~%")
    (format stream "    'parents': ~A,~%"
            (output-string-vector (mapcar #'node-name (node-parents node))))
    (format stream "    'probability': ")
    (display-nd-array (node-cpt node) stream)
    (format stream "}")))

(defun output-string-vector (strings)
  (if strings
      (format nil "[~{'~A'~^, ~}]" strings)
      "None"))
                      
;;;
;;; PyMC
;;;

(defmethod output-graph (graph (format (eql :pymc)) stream)
  (generate-pymc graph stream))

(defun generate-pymc (graph &optional (stream t))
  (format stream "import pymc as py~%")
  (format stream "import numpy as np~%")
  (format stream "import random~%")

  (maphash #'(lambda (k node) k (setf (node-written node) nil))
           (graph-ht graph))
  
  (maphash #'(lambda (k node)
	       (declare (ignore node))
	       (generate-pymc-named-node graph k stream))
	   (graph-ht graph))
  
  (let (vars)
    (maphash #'(lambda (k v)
		 v
		 (push (string k)  vars)
		 (push k vars))
	     (graph-ht graph))
    (format stream "themodel={~{~s:~a~^, ~}}~%"  vars)
    (terpri)
    (terpri)))


(defun generate-pymc-named-node (graph name stream)
  (let ((node (gethash name (graph-ht graph))))
    
    (unless node
      (error "no such node as ~s" name))
    
    (when (node-written node)
      (return-from generate-pymc-named-node nil))
    
    (dolist (parent (node-parents node))
      (generate-pymc-named-node graph (node-name parent) stream))
    
    (setf (node-written node) t)
    
    (format stream "~%@py.stochastic(dtype=int)~%def ~a(value=0" name)
    (let ((args ""))
      (let ((before t))
	(dolist (parent (node-parents node))
	  (if* before then (format stream ", "))
	  (format stream "~a=~a" (node-name parent) (node-name parent))
	  (setq args (string+ args "[int(" (node-name parent) ")]"))
	  (setq before t)))
      (format stream "):~%")
    
      (format stream "        cpt = ")
      (display-nd-array (node-cpt node) stream)
      (terpri)
      (format stream "        if value < 0 or value >= ~d: return -np.inf~%"
              (length (node-values node)))
      (format stream "        return np.log(cpt~a[value])~%" args)
      (format stream ""))))


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
                            (format stream "~6,4f" val)
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
  "Returns a directed SIMPLE-GRAPH with the Bayesian network that's
equivalent to the given undirected GRAPH. GRAPH can be either a
SIMPLE-GRAPH or a (full) GRAPH."
  ;; See slide on p.66 of Petitjean.
  (let* ((g (simple-graph-version graph))
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
  ;; This is the example from p.66 of Petitjean.
  ;; Our result is different from Petitjean's example, but
  ;; that's because there are many possible results and this
  ;; function just returns one of them.
  (let ((test-graph (test-graph-p66-petitjean :directed nil)))
    (undirected-to-bayesian test-graph)))

(defun equivalent-bayesian-model (model)
  (let* ((bayesian (undirected-to-bayesian model))
         (description (graph-description bayesian))
         (bayesian-model (build-graph :graphdesc description)))
    (generate-cpt bayesian-model)
    ;(change-counts-to-probabilities bayesian-model)    
    bayesian-model))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generating and exporting models
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun export-model (model filename)
  "Export a bayesian model to the specified file."
  (with-open-file (out filename
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :overwrite)
    (output-graph model :jags out)
    (list model out)))

(defun generate-undirected-model (&key
                                    (max-iterations 15)
                                    (show-scores t)
                                    (score-threshold 0.05))
  
  "Generate and return a decomposable model (undirected graph) for the
triple store in *DB-NAME* on the local Allegrograph server. The system will
keep adding edges until either [1] MAX-ITERATIONS is reached, or [2]
there are no edges left with a score of at least SCORE-THRESHOLD.

  If SHOW-SCORES is true, the initial scores and entropies of all edges
will be printed to standard output.

  You can visualize the network that's being created by opening the
triple store in Gruff, going to View - Query View, running a query like

  select * where {
   graph <http://credoo.com/show> {?s ?p ?o}
   }

and then clicking on 'Create Visual Graph'."
  
  (let ((graph (compute-edge-graph)))
    (open-triple-store *db-name*)
    (unwind-protect (progn
                      (initialize-edge-scores graph)
                      (when show-scores
                        (display-scores graph))
                      (register-namespaces)
                      (start-adding-edges)
                      (loop for i from 1 to (or max-iterations 100000)
                         while (some (lambda (e) (<= (graph-edge-score e) score-threshold))
                                     (graph-possible-edge-list graph))
                         do (format t "~2%---- Step ~D ----~2%" i)
                           (add-best-edge graph)))
      (close-triple-store)
      graph)))

(defun generate-bayesian-model (&rest args
                                &key
                                  (filename "graph.py")
                                  (max-iterations 100)
                                  (show-scores t)
                                  (score-threshold 0.05))
  
  "Generate a Bayesian network (directed graph) for the triple store
in *DB-NAME* on the local Allegrograph server and write out the network
specification to FILENAME. The system will keep adding edges until
either [1] MAX-ITERATIONS is reached, or [2] there are no edges left
with a score of at least SCORE-THRESHOLD.

  If SHOW-SCORES is true, the initial scores and entropies of all edges
will be printed to standard output.

  You can visualize the network that's being created by opening the
triple store in Gruff, going to View - Query View, running a query like

  select * where {
   graph <http://credoo.com/show> {?s ?p ?o}
   }

and then clicking on 'Create Visual Graph'."

  (declare (ignorable max-iterations show-scores score-threshold))
  (setq args (remprop args filename))
  (let* ((model (apply #'generate-undirected-model args))
         (bayesian (equivalent-bayesian-model model)))
    (setq *model* model) ; for debugging only
    (export-model bayesian filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-undirected (&optional (n 30))
  (generate-undirected-model :max-iterations n)
  t)

(defun test-bayesian (&optional (n 50))
  (generate-bayesian-model :max-iterations n
                           :filename "graph.py"
                           :show-scores t
                           :score-threshold 0.05))
