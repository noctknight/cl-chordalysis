(in-package :chordalysis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic graphs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(defun edges-have-same-vertices (e1 e2)
  (destructuring-bind (v1 . v2)
      e1
    (destructuring-bind (w1 . w2)
        e2
      (or (and (eql v1 w1) (eql v2 w2))
          (and (eql v1 w2) (eql v2 w1))))))

(defclass simple-graph (basic-graph)
  ((temp-nodes :accessor graph-temp-nodes
               :documentation "hash-table from vertices (symbols) to temp-nodes"))
  (:documentation "Basic graphs are used by some graph algorithms like chordality
checking."))


(defstruct temp-node ; used for some graph algorithms
  name ; a symbol (from the list of vertices)
  label ; a list of numbers, used during lexicographical breadth first search
  number ; used during LBFS
  ;; FOLLOWER is the temp-node with smallest number that is both neighbor of
  ;; this node and has number larger than this node (Tarjan 1984)
  follower
  ;; INDEX is used for Tarjan's zero fill-in check
  index)


(defun subgraph (graph nodes)
  "Returns a subgraph of GRAPH that only contains the given nodes and all
edges that are based on those nodes."
  (let ((edges (loop for edge in (graph-edges graph)
                  when (destructuring-bind (v1 . v2)
                           edge
                         (and (find v1 nodes) (find v2 nodes)))
                  collect edge)))
    (make-instance 'basic-graph
                   :nodes nodes
                   :edges edges)))

                   
(defun simple-graph-copy (graph)
  "Returns a copy of GRAPH that contains just (a copy of) the lists with NODES and
EDGES. This can be used for algorithms like graph chordality check."
  (make-instance 'simple-graph
                 :nodes (copy-list (graph-nodes graph))
                 :edges (loop for (v1 . v2) in (graph-edges graph)
                             collect (cons v1 v2))))
                           
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
  (mapcar #'car (shortest-paths* graph v1 v2)))


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
    

(defun shortest-paths* (graph v1 v2)
  "Returns a list of the shortest paths from V1 to V2. Each path is a
list containing the vertices on the path to V2. 
V1 is not included in the list."
  (mapcar #'cdr (shortest-paths graph v1 v2)))

(defun shortest-paths (graph v1 v2)
  "Returns a list of the shortest paths from V1 to V2. Each path is a
list containing the vertices on the path to V2. V1 is included in the list."
  ;; Shortest paths from V1 to V2: [1] for each neighbor N of V1, compute all shortest
  ;; paths from N to V2, [2] then insert V1 in front of those shortest paths.
  (let ((visited
         ;; A hash-table with vertices as keys where each value is either T (for visited
         ;; nodes that do not reach V2) or a list of conses of shortest paths that reach
         ;; V2 and their lengths.
         (make-hash-table)))
    (labels ((walk (start)
               "Returns a list of conses, where each cons contains a path and a path length."
               (if (eql start v2)
                   (list (cons (list v2) 1))
                   (let ((previous (gethash start visited)))
                     (if previous
                         (if (listp previous) previous nil)
                         (let ((neighbors (vertex-children start graph)))
                           (setf (gethash start visited) t)
                           (let ((neighbor-paths (loop for n in neighbors
                                                    append (walk n))))
                             (when neighbor-paths
                               (setf (gethash start visited)
                                     (let ((shortest-length (reduce #'min (mapcar #'cdr neighbor-paths))))
                                       (loop for (path . length) in neighbor-paths
                                          when (= length shortest-length)
                                          collect (cons (cons start path) (1+ length)))))))))))))
      (mapcar #'car (walk v1)))))

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
;;; Little utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-equal (list-a list-b &key (test #'eql))
  (and (every (lambda (a) (find a list-b :test test))
              list-a)
       (every (lambda (b) (find b list-a :test test))
              list-b)))
