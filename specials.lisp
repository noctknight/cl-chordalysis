(in-package :chordalysis)

(defvar *data-source* nil)

(defvar *model* nil
  "The current model. Used for debugging only.")

(defparameter *datatype* #+laptop :csv #-laptop :csv
  "Either :ALLEGROGRAPH or :CSV at the moment.")

(defparameter *db-name* #+laptop "data/orphamine.csv" #-laptop "testdb"
  "The name of the default data source.")

(defparameter *result-graph* "http://credoo.com/show"
  "Only used for showing the generated model in Gruff.")

(defparameter *graphical-log-file* "graphical-log.txt")

(defparameter *config-file* "config.json")

(defparameter *worst-possible-score* -1e10
  "A score that guarantees that edges with this score will be at the bottom of the
possible edge list.")

(defparameter *worst-possible-p-value* 1e10
  "A score that guarantees that edges with this p-value will not cause the algorithm to
  continue.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Settings (can be changed from JSON config file)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *max-columns* 100
  "The maximum number of columns (also called 'fields' or 'predicates'). All other columns
will be ignored.")

(defparameter *max-distinct-values* 50
  "If a column has more than this number of distinct values, it is ignored.")

(defparameter *max-iterations* 50
  "The system will keep adding edges to the graph until either [1] *MAX-ITERATIONS* is
reached, or [2] there are no edges left with a score of at least *THRESHOLD*.")

(defparameter *threshold* 0.05
  "Keep going as long there's an edge left with a p-value that's lower than this
threshold.")

(defparameter *max-frequency-table-size* (* 10 1024 1024)
  "Frequency tables with more than this number of entries are not computed and the
corresponding edges will be skipped.")

(defparameter *default-missing* '("" "-")
  "A list with values that should be considered to be missing values.")

(defparameter *columns* nil
  #+nil
  '((:name "tot_money_ind" :type :numerical
     :quantiles (0 500 1000 2000 3000 5000 10000))
    (:name "max_money_ind" :type :numerical)
    (:name "age" :type :numerical
     :quantiles (10 20 30 40 60 80))))


