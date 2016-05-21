(in-package :chordalysis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Chordalysis configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

We read the configuration info from a JSON file.

Example (using '//' for comments, which is not legal JSON syntax).

{

// The maximum number of columns (or 'predicates', if you prefer).
// All other columns will be ignored.
"max_columns": 95,

// If a column has more than this number of distinct values, it is ignored.
"max_distinct_values": 50,

// Frequency tables with more than this number of entries are not computed and the
// corresponding edges will be skipped.
"max_frequency_table_size": 10000000,

// The system will keep adding edges to the graph until either [1] 'max_iterations' is
// reached, or [2] there are no edges left with a p-value of at least 'threshold'.
"max_iterations": 30,
"threshold": 0.05,

// A list with values that should be considered to be missing values.
"default_missing": ["", "-"],

// A list of column specifications. Each specification can include the following parts:
//
//  "name" [required]: the field name
//
//  "missing" [optional]: a list of values that should be interpreted as missing value
//    (in addition to the ones specified by 'default_missing'). Example: ["999"].
//
//  "type" [optional, default: "categorical"]: either "categorical" or "numerical".
//
//  "quantiles" [optional]: may be specified if the field type is "numerical".
//      If specified, this is a list of numbers that divide the quantile intervals.
//      Example: {0,10,20,50,100} will create 4 quantiles: 0 <= x < 10, 10 <= x < 20,
//      20 <= x < 50, 50 <= x < 100.

"columns":
  [{"name": "tot_money_ind",
    "type": "numerical", 
    "quantiles": [0, 500, 1000, 2000, 3000, 5000, 10000]},

   {"name": "max_money_ind",
    "type": "numerical"},

   {"name": "age",
    "type": "numerical",
    "quantiles": [10, 20, 30, 40, 60, 80]},

   {"name": "tot_score_ind",
    "type": "numerical",
    "missing": ["", "0.0"],
    "quantiles": [1, 10, 20, 30, 50, 100]}
  ]
}

|#

(defun load-config (&key (file (merge-pathnames "config.json" cl-user::*base-dir*)))
  (format t "~&Loading configuration file ~S~%" file)
  (let ((plist (load-json-file file)))
    (when plist
      (macrolet ((setting (special-variable key default)
                   `(let ((value (getf plist ,key)))
                      (setq ,special-variable (or value ,default)))))
        (list (setting *max-columns* :max_columns 100)
              (setting *max-distinct-values* :max_distinct_values 50)
              (setting *max-iterations* :max_iterations 50)
              (setting *threshold* :threshold 0.05)
              (setting *max-frequency-table-size* :max_frequency_table_size (* 10 1000 1000))
              (setting *default-missing* :default_missing '("" "-"))
              (setting *columns* :columns '()))))))

(defun load-json-file (file)
  (with-open-file (in file :external-format :utf-8 :if-does-not-exist nil)
    (when in
      (yason:parse in
                   :object-as :plist
                   :object-key-fn (lambda (string) (intern (string-upcase string) :keyword))))))
