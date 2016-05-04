;;
;; This file contains some experimental stuff that's not used at the moment
;; but I may need some parts of it later.
;;

(eval-when (compile load eval)
  (require :agraph "/data/data12/zhengxin_pro/Heater/BIN/agraph-6.0.1/lib/agraph.fasl"))


(defpackage :user (:use :db.agraph :db.agraph.sparql))
(in-package :user)


(eval-when (compile load eval) (enable-!-reader))

(declaim (optimize (debug 3)))

#|

* Debugging

(stats)  - shows predicates in the db
(select-predicates) - select predicates we might want to use

* Part 1: generate the Bayesian network based on Agraph data.
* generating a edge graph for all nodes, starting from scratch

* Part 2: convert cpts and Bayesian net to Python code

|#

(defparameter *db-name* "testdb")

(defparameter *result-graph* "http://credoo.com/show")


(defparameter *test-graph*
    ;; form is:  child parents
    '((if_time_c_ind    ffratio)
      (most_industry    tot_org_numb)
      (ffratio          if_redlt_ind if_bayonet_ind)
      (cst_score_ind    if_redlt_ind))
  "Predicate names we're interested in.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph data structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass simple-graph ()
  ((nodes :initarg :nodes :initform '() :accessor graph-nodes
          :documentation "A list with vertex names (symbols).")
   (edges :initarg :edges :initform '() :accessor graph-edges
          :documentation "A list of cons (v1 . v2) with (v2 . v1) also present.")
   (temp-nodes :accessor graph-temp-nodes
               :documentation "hash-table from vertices (symbols) to temp-nodes")))

(defclass graph ()
  ((ht :initarg :ht :accessor graph-ht
       :documentation " table of node name -> node")
   (origname-ht :initarg :origname-ht :accessor graph-origname-ht
                :documentation "table mapping original name to one of the extended names")
   (edge-ht :accessor graph-edge-ht :documentation "used when doing entropy stuff")
   (possible-edge-list :initform '() :accessor graph-possible-edge-list
                       :documentation "list of possible edges (GRAPH-EDGE structs) to add, sorted by score")
   (disabled-edge-list :initform '() :accessor graph-disabled-edge-list
                       :documentation "list of edges (GRAPH-EDGE structs) that may not be added")
   ;; Actual contents of graph
   (nodes :initform '() :accessor graph-nodes
          :documentation "names of the vertices (symbols)")
   (edges :initform '() :accessor graph-edges
          :documentation "list of cons (v1 . v2) with (v2 . v1) also present")))


(defstruct temp-node ; used for some graph algorithms
  name ; a symbol (from the list of vertices)
  label ; a list of numbers, used during lexicographical breadth first search
  number ; used during LBFS
  ;; FOLLOWER is the temp-node with smallest number that is both neighbor of
  ;; this node and has number larger than this node (Tarjan 1984)
  follower
  ;; INDEX is used for Tarjan's zero fill-in check
  index)

(defstruct node
  name    ; terse string for node
  upi	 
  values  ; list of values in sorted order 
  parents ; list of parent nodes
  cpt	  ; conditional prob table
  
  ; for when it's used to display value
  i
  max
  
  ; for when writing out
  written)

;;
;; Helper functions/macros
;;

(defun map-graph-node-names (graph function)
  "Calls FUNCTION with the name of each node in GRAPH."
  (loop for name being the hash-value of (graph-ht graph)
     do (funcall function name)))

(defmacro do-graph-node-names ((name graph) &body body)
  `(map-graph-node-names ,graph (lambda (,name) ,@body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Building a graph structure for data from the database.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-graph (&key (graphdesc *test-graph*)
		      (db-name *db-name*)
		      (unique-nodes nil)
		      (cutoff 1000) ; ignore preds with more distinct values
		      )
  "If UNIQUE-NODES is true, we create unique nodes (gensym) for each node depending on
its parents."
  (open-triple-store db-name)
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
      (make-instance 'graph :ht nodetable :origname-ht origname))))

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
                         :upi  upi
                         :values values)))))))

(defun uniqueify (name uniquep)
  (if uniquep
      (string+ name "___" (gensym))
      name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Count tables and conditional probability tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
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
      (do-graph-node-names (node graph)
        (do-generate-cpt-node node info)))))


(defun do-generate-cpt-one-node (node)
  (init-cpt-for-node node)
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
  (maphash #'display-cpt (graph-ht graph)))


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
  entropy 
  
  separators ; list of nodes separating V1 and V2.
  )

(defun compute-edge-graph (&key (cutoff 1000))
  (open-triple-store *db-name*)
  ;; Get rid of the nodes that visualize the network.
  (delete-triples :p !<http://credoo.com/connected>)
  ;;
  (unwind-protect
      (create-edge-graph-inner cutoff)
    (close-triple-store)))


(defun create-edge-graph-inner (cutoff)
  (let ((preds  ;'(if_time_c_ind  ffratio most_industry tot_org_numb)
	 (mapcar #'(lambda (preddesc)
		     (intern (part->terse (car preddesc))))
		 (do-select-predicates cutoff nil))))
    
    (format t "~s verticies in the graph~%" (length preds))
    
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
;;; Entropy and scoring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-edge-entropies (graph)
  "Compute entropy for all possibles edges and then sort the graph's
possible edge list by the edge entropies."
  (dolist (graph-edge (graph-possible-edge-list graph))
    (compute-edge-entropy graph-edge))
  (setf (graph-possible-edge-list graph)
        (sort (graph-possible-edge-list graph) 
              #'(lambda (a b) (> (graph-edge-entropy a)
                                 (graph-edge-entropy b)))))
  graph)

(defun compute-edge-entropy (graph-edge)
  (setf (graph-edge-entropy graph-edge) 
    (compute-cpt-entropy (node-cpt (graph-edge-node graph-edge)))))

(defun compute-cpt-entropy (cpt)
  (let ((size (apply #'* (array-dimensions cpt))))
    (let ((darr (make-array size :displaced-to cpt)))
      
      (let ((total 0)) ; total of all counts
	(dotimes (i size)
          (incf total (aref darr i)))
	(when (equal total 0)
          (return-from compute-cpt-entropy 0.0d0))
	(setq total (float total 1.0d0))
	(let ((entropy 0.0d0))
	  ;; DO: We should divide everything either by TOTAL (the
	  ;; total of all counts in the array) or by SIZE (the total
	  ;; number of counts in the array).
	  (dotimes (i size)
	    (let ((count (aref darr i)))
	      (when (> count 0)
                (let ((probability (/ count total)))
                  (incf entropy (* probability (log probability)))))))
	  ;; Note that entropy is actually not divided by size, so we
	  ;; need to separate that concept later.
	  (if (= size 1)
	      ;; DO: Maybe get rid of nodes with only 1 value??
	      0
	      (let ((degrees-of-freedom (- size 1)))
		(- (/ entropy degrees-of-freedom)))))))))

#+ignore
(defun recompute-entropy (graph graph-edge)
  (let ((vars (cons 
	       (graph-edge-v1 graph-edge)
	       (car (shortest-paths graph (graph-edge-v1 graph-edge)
				    (graph-edge-v2 graph-edge))))))
    
    (format t "recompute entropy for ~s to ~s, all vars: ~s~%" 
	    (graph-edge-v1 graph-edge) (graph-edge-v2 graph-edge)
	    vars)
    (let ((child (gethash (string (car vars)) (graph-origname-ht graph)))
	  (parents (mapcar #'(lambda (var) (gethash (string var) (graph-origname-ht graph))) (cdr vars))))
      ;(format t "child ~s  parents ~s~%" child parents)
      (setf (node-parents child) parents)
      (do-generate-cpt-one-node child)
      (let ((old (graph-edge-entropy graph-edge)))
	(setf (graph-edge-entropy graph-edge) (compute-cpt-entropy (node-cpt child)))
	(format t " entropy changed from ~6,4f to ~6,4f~%" old (graph-edge-entropy graph-edge)))
      )))

(defun recompute-entropy (graph graph-edge)
  (let ((v1 (graph-edge-v1 graph-edge))
	(v2 (graph-edge-v2 graph-edge))
	(seps (graph-edge-separators graph-edge)))
    
    (format t "recompute entropy for ~s to ~s, seps: ~s~%" v1 v2 seps)
    (let ((entropy
	   (+ ; ?? N ??
	    (compute-entropy-of-verticies graph (cons v1 seps))
	    (compute-entropy-of-verticies graph (cons v2 seps))
	    (- (compute-entropy-of-verticies graph (list* v1 v2 seps)))
	    (- (compute-entropy-of-verticies graph seps)))))
    
      (let ((old (graph-edge-entropy graph-edge)))
	(setf (graph-edge-entropy graph-edge)  entropy)
	(format t " entropy changed from ~6,4f to ~6,4f~%"
                old (graph-edge-entropy graph-edge))))))

(defun compute-entropy-of-verticies (graph verticies)
  (let ((vars verticies))
    (let ((child (gethash (string (car vars)) (graph-origname-ht graph)))
	  (parents (mapcar #'(lambda (var)
                               (gethash (string var) (graph-origname-ht graph)))
                           (cdr vars))))
      (setf (node-parents child) parents)
      (do-generate-cpt-one-node child)
      (compute-cpt-entropy (node-cpt child)))))
      
    
  
(defun display-entropies (graph)
  (format t "~25a~25a~15a~%" "v1" "v2" "entropy")
  (dolist (graph-edge (graph-possible-edge-list graph))
    (format t "~25a~25a~9,7f~%"
            (graph-edge-v1 graph-edge) (graph-edge-v2 graph-edge)
            (graph-edge-entropy graph-edge))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;; Adding edges
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-adding-edges ()
  (delete-triples :g (resource *result-graph*)) ; get rid of gruff graph nodes
  (commit-triple-store))

; (defun add-best-edge (graph)
  
(defun add-best-edge (graph)
  (let ((graph-edge (find-possible-edge-to-add graph)))
    (when graph-edge
      (setf (graph-possible-edge-list graph)
            (remove graph-edge (graph-possible-edge-list graph)))
      (add-edge graph graph-edge))))
  
(defun find-possible-edge-to-add (graph)
  (first (graph-possible-edge-list graph))
  #+later
  (find-if (lambda (edge)
             (adding-edge-is-ok graph
                                (graph-edge-v1 edge)
                                (graph-edge-v2 edge)))
           (graph-possible-edge-list graph)))

(defun adding-edge-is-ok (graph v1 v2)
  (let ((simple-graph (simple-graph-version graph)))
    (simple-add-edge simple-graph v1 v2)
    (is-chordal simple-graph)))

(defun simple-add-edge (graph v1 v2)
  "Add nodes V1 and V2 and add edges V1->V2 and V2->V1."
  (pushnew v1 (graph-nodes graph))
  (pushnew v2 (graph-nodes graph))
  (push (cons v1 v2) (graph-nodes graph))
  (push (cons v2 v1) (graph-nodes graph)))

    
(defun simple-graph-version (graph)
  "Returns a copy of GRAPH that contains just (a copy of) the lists
with NODES and EDGES. This can be used for algorithms like graph
chordality check. GRAPH can be either a SIMPLE-GRAPH or a (full) GRAPH."
  (make-instance 'simple-graph
                 :nodes (copy-list (graph-nodes graph))
                 :edges (copy-list (graph-edges graph))))

(defun add-edge (graph graph-edge)
  (let ((v1 (graph-edge-v1 graph-edge))
        (v2 (graph-edge-v2 graph-edge)))
    (format t "~%Adding edge (entropy ~6,4f) from ~s to ~s.~%"
            (graph-edge-entropy graph-edge)
            v1
            v2)
    ;; Add V1, V2, V1->V2 and V2->V1.
    (simple-add-edge graph v1 v2))
  ;;
  (make-edge-visible-in-gruff graph-edge)
  ;; Find minimal separators and non chordal situations.
  (loop for vv on (graph-nodes graph)
     do (loop for xx on (cdr vv)
           do (let ((v1 (car vv))
                    (v2 (car xx)))
                (let* ((paths (shortest-paths graph v1 v2))
                       (len-sp (length (car paths))))
                  (if* (<= len-sp 1)
                       thenret         ; no link or directly connected
                       elseif (= 2 len-sp)
                       then (ensure-separator-set graph 
                                                  v1
                                                  v2
                                                  (mapcar #'car paths) ; separators for each path
                                                  )
                       else (disable-possible-edge graph v1 v2))))))
  ;; Show perfect elimination ordering and chordality.
  (let* ((ordering (lex-breadth-first-search (simple-graph-version graph)))
         (peo (nodes ordering)))
    (format t "~%PEO: ~S~%"
            (loop for n across peo
               collect (list (temp-node-name n) (temp-node-label n))))
    (format t "~&Graph chordality: ~S~%"
            (is-chordal (simple-graph-version graph)))))


(defun make-edge-visible-in-gruff (graph-edge)
  (add-triple (resource (string (graph-edge-v1 graph-edge)) "credoo")
              (resource "connected" "credoo")
              (resource  (string (graph-edge-v2 graph-edge)) "credoo")
              :g (resource *result-graph*))
  (commit-triple-store))


(defun ensure-separator-set (graph v1 v2 separators)
  (multiple-value-bind (graph-edge disabled)
      (locate-possible-edge graph v1 v2)
    (cond ((null graph-edge)
           (error "there should be a possible graph edge from ~s to ~s" v1 v2))           
          ((set-difference separators (graph-edge-separators graph-edge))
           (format t "between ~s and ~s separators changed from ~s to ~s~%"
                   v1 v2 (graph-edge-separators graph-edge) separators)
           (remove-graph-edge-from-possible graph graph-edge disabled)
           (setf (graph-edge-separators graph-edge) separators)
           (recompute-entropy graph graph-edge)
           (insert-graph-edge-in-possible graph graph-edge))
          (disabled ; then just enable it
           (format t "bringing edge ~s ~s back to possible~%"
                   (graph-edge-v1 graph-edge) (graph-edge-v2 graph-edge))
           (remove-graph-edge-from-possible graph graph-edge disabled)
           (insert-graph-edge-in-possible graph graph-edge)))))

(defun disable-possible-edge (graph v1 v2)
  "Mark this edge as disabled."
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

#|
(do ((prev nil edges)
     (edges (graph-possible-edge-list graph) (cdr edges)))
    ((null edges))
  (let ((graph-edge (car edges)))
    (when (or (and (eq (graph-edge-v1 graph-edge) v1)
                   (eq (graph-edge-v2 graph-edge) v2))
              (and (eq (graph-edge-v1 graph-edge) v2)
                   (eq (graph-edge-v2 graph-edge) v1)))
      ;; Splice out.
      (if prev
          (setf (cdr prev) (cdr edges))
          (setf (graph-possible-edge-list graph) (cdr edges)))
      (return graph-edge)))))
|#

(defun graph-edge-has-vertices (graph-edge v1 v2)
  (or (and (eq (graph-edge-v1 graph-edge) v1)
           (eq (graph-edge-v2 graph-edge) v2))
      (and (eq (graph-edge-v1 graph-edge) v2)
           (eq (graph-edge-v2 graph-edge) v1))))

(defun remove-graph-edge-from-possible (graph graph-edge disabled)
  (if disabled
      (setf (graph-disabled-edge-list graph)
	    (delete graph-edge (graph-disabled-edge-list graph) :test #'eq))
      (setf (graph-possible-edge-list graph)
	    (delete graph-edge (graph-possible-edge-list graph) :test #'eq))))


(defun insert-graph-edge-in-possible (graph graph-edge)
  "Insert graph-edge, ordering by entropy."
  (let ((entropy (graph-edge-entropy graph-edge)))
    (do ((prev nil edges)
	 (edges (graph-possible-edge-list graph) (cdr edges)))
	((null edges)
	 (if prev
	     (setf (cdr prev) (list graph-edge))
	     (setf (graph-possible-edge-list graph) 
		   (list graph-edge))))
      (let ((this-graph-edge (car edges)))
	(when (> entropy (graph-edge-entropy this-graph-edge))
          ;; insert here
          (if prev
              (setf (cdr prev)
                    (cons graph-edge (cdr prev)))
              (push graph-edge
                    (graph-possible-edge-list graph)))
          (return))))))


(defun locate-possible-edge (graph v1 v2)
  "Returns: [1] edge, [2] T if disabled."
  (or (find-if (lambda (graph-edge) (graph-edge-has-vertices graph-edge v1 v2))
               (graph-possible-edge-list graph))
      (let ((e (find-if (lambda (graph-edge) (graph-edge-has-vertices graph-edge v1 v2))
                        (graph-disabled-edge-list graph))))
        (values e t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Separators, paths, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shortest-paths (graph v1 v2)
  "Returns the shortest paths from V1 to V2.
Return a list of lists, each list containing the vertices on the path to v2. 
V1 is not included in the list."
  (dolist (edge (graph-edges graph)
           ;; no direct link so do it the hard way
           (mapcar #'(lambda (revpath) (cdr (reverse revpath)))
                   (shortest-paths-do graph `((,v1)) v2)))
    (when (and (eq v1 (car edge))
               (eq v2 (cdr edge)))
      (return (list (list v2))))))


(defun shortest-paths-do (graph paths v2)
  (let (newpaths (edges (graph-edges graph)) final)
    (loop
       
       (dolist (path paths)
         (let ((v1 (car path)))
           (dolist (edge edges)
             (if* (and (eq v1 (car edge))
                       (not (member (cdr edge) path :test #'eq)))
                  then (if* (eq (cdr edge) v2)
                            then ; final vertex
			    (if* (not final)
                                 then ; forget anything we've seen
                                 (setq newpaths nil))
			    (setq final t)
			    (push (cons (cdr edge) path) newpaths)
			    (return) ; no more looking at edges
                            elseif final
                            thenret ; not a winner so ignore
                            else ; still searching, save this
			    (push (cons (cdr edge) path) newpaths))))))
       (if* final
            then (return-from shortest-paths-do newpaths))
       
       (if* newpaths 
            then (setq paths newpaths
                       newpaths nil)
            else (return)))))


(defun select-predicates (&key (cutoff 10) names)
  "Shows some details for all predicates \(except the ones named
'weibo.*'). If NAMES is supplied, it's a list of predicate names.
Otherwise you get to see them all."
  (open-triple-store *db-name*)
  (delete-triples :p !<http://credoo.com/connected>)
  (commit-triple-store)
  (unwind-protect
       (show-predicates (do-select-predicates cutoff names))
    (close-triple-store)))

(defun do-select-predicates (cutoff names)
  "Return list of (predicate-upi value1 ... valuen)"
  (let ((preds (run-sparql "select distinct ?p where { ?s ?p ?o}"
                           :results-format :lists))
	(predicates-to-use))
    
    (dolist (pred preds)
      (setq pred (car pred))
      (let ((name (part->terse pred)))
	(unless (or (part= !rdf:type pred)
		    (and names (not (find name names :test #'equal))))
          (if* (search "weibo" name) ; we don't want weibo for some reason
	       thenret		     ; ignore
	       else (let ((objects (make-hash-table :test #'equal)))
		      (tslet ((q -sub pred -obj))
			(setf (gethash (upi->value obj) objects) t))
		      (let ((values (hash-table-keys objects)))
			(when (or (null cutoff) (<= (length values) cutoff))
                          (push (cons pred (sort-values values)) predicates-to-use))))))))
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
;;; Database statistics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stats ()
  (open-triple-store *db-name*)
  (unwind-protect
      (do-stats)
    (close-triple-store)))

(defun do-stats ()
  "Show predicates and their unique values."
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


      
;;;  add rdf:type credoo:Person


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


(defun lex-breadth-first-search (graph)
  "Returns an ORDERING for GRAPH in lexicographical breadth first order.
If GRAPH is chordal, the result is a 'perfect elimination ordering'."
  (let ((vertex-table (make-hash-table))
	(nodes '()))
    (dolist (node-name (graph-nodes graph))
      ;; "Initialize all labels as the empty string." (we use NIL instead)
      (let ((temp (make-temp-node :label nil :name node-name :number nil)))
	(setf (gethash node-name vertex-table) temp)
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
                              (nconc (temp-node-label w) (list i))))))))))
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
  ""
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

(defun vertex-neighbors (graph temp-node)
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
  (loop for neighbor in (vertex-neighbors graph vertex)
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

;;;
;;; Jags
;;;

(defmethod output-graph (graph (format (eql :jags)) stream)
  (output-jags graph stream))

(defun output-jags (graph &optional (stream t))
  (format stream "network_dict = {~%")
  (let ((node-names (loop for node-name being the hash-key of (graph-ht graph)
                       collect node-name)))
    (loop for (node-name . more) on node-names
       for i from 1
       do (progn
            (output-jags-node graph node-name stream i)
            (when more
              (format stream ","))
            (format stream "~%"))))
  (format stream "}~%"))

(defun output-jags-node (graph name stream counter)
  (let ((node (gethash name (graph-ht graph))))
    (unless node
      (error "no such node as ~s" name))
    (let ((possible-values (node-values node)))
      (format stream "  '~A': {~%" name)
      (format stream "    'index': ~D,~%" counter)
      (format stream "    'possibleValues': ~A,~%"
              (output-string-vector possible-values))
      (format stream "    'numberOfPossibleValues': ~D,~%" (length possible-values))
      (format stream "    'variableType': 'categorical',~%")
      (format stream "    'parents': ~A,~%"
              (output-string-vector (mapcar #'node-name (node-parents node))))
      (format stream "    'probability': ")
      (display-nd-array (node-cpt node) stream)
      (format stream "}"))))

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


;;
;; Output array
;;

(defvar *need-comma* nil)

(defun display-nd-array (arr &optional (stream t))
  (let ((dims (array-dimensions arr))
	(*need-comma*))
    (display-inner-array arr nil dims stream)))


(defun display-inner-array (arr seen togo &optional (stream t))
  (macrolet ((check-comma ()
	       '(if* need-comma then (format stream ", ") (setq need-comma nil))))
    (let ((max (pop togo))
	  (need-comma))
      (cond ((null togo)
             ;; At the bottom level, print out values.
             (format stream "[")
             (dotimes (i max)
               (let ((val (apply #'aref arr (reverse (cons i seen)))))
                 (check-comma)
                 (format stream "~6,4f" val)
                 (setq need-comma t)))
             (format stream "]"))
            (t ;; Not at the bottom yet.
             (format stream "[")
             (dotimes (i max)
               (check-comma)
               (display-inner-array arr (cons i seen) togo stream)
               (setq need-comma t))
             (format stream "]"))))))

;;;
;;; Test for Yao
;;;

(defun test-jags (&key (filename "test-jags.py"))
  (let ((graph (compute-edge-graph)))
    (compute-edge-entropies graph)
    (open-triple-store *db-name*)
    (register-namespaces)
    (start-adding-edges)
    (add-best-edge graph)
    (add-best-edge graph)
    (loop repeat 13
       do (add-best-edge graph))
    (change-counts-to-probabilities graph)
    (with-open-file (out filename
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :overwrite)
      (output-graph graph :jags out)
      out)))


(defun test-for-yao (&key (filename "test-jags.py"))
  (let ((graph (build-graph :graphdesc *test-graph*)))
    (generate-cpt graph)
    (change-counts-to-probabilities graph)
    (with-open-file (out filename
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :overwrite)
      (output-graph graph :jags out)
      out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generating Bayesian network
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-bayesian-network (&key (max-iterations 15) (show-entropies t))
  (let ((graph (compute-edge-graph)))
    (compute-edge-entropies graph)
    (when show-entropies
      (display-entropies graph))
    (open-triple-store *db-name*)
    (register-namespaces)
    (start-adding-edges)
    (loop for i from 1 to max-iterations
       do (format t "~%---- Step ~D ----~%" i)
         (add-best-edge graph))
    graph))

(defun test (&optional (n 30))
  (generate-bayesian-network :max-iterations n)
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; New version of shortest-paths function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+maybe-later
(defun shortest-paths* (graph v1 v2)
  "Returns a list of the shortest paths from V1 to V2. Each path is a
list containing the vertices on the path to V2. V1 is included in the list."
  (let ((paths-table (make-hash-table)))
    (labels ((shortest (v)
               ;; Returns a list of shortest paths from V to V2.
               ;; Each shortest path includes V itself.
               (format t "~&Visiting ~S~%" v)
               (or (gethash v paths-table)
                   (setf (gethash v paths-table)
                         (if (eql v v2)
                             (list (list v))
                             (let ((children (vertex-children v graph)))
                               (when children
                                 (if (member v2 children)
                                     ;; Base case: direct path from V to V2.
                                     (list (list v v2))
                                     ;; No direct path from V to V2: add V to
                                     ;; all shortest paths for the neighbors.
                                     (let ((child-paths '())
                                           (shortest-length 1000000000))
                                       (dolist (child children)
                                         (let ((paths (shortest child)))
                                           (loop for path in paths
                                                do (let ((len (length path)))
                                                     (cond ((< len shortest-length)
                                                            (setq child-paths (list path)
                                                                  shortest-length len))
                                                           ((= len shortest-length)
                                                            (push path child-paths)))))))
                                       (loop for path in child-paths
                                          collect (progn (format t "~&Adding ~S to ~S~%" v path)
                                                         (cons v path))))))))))))
      (shortest v1))))
