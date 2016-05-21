(in-package :chordalysis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition frequency-table-too-big (error)
  ((field-names :initarg :field-names :reader field-names)
   (dimensions :initarg :dimensions :reader dimensions)))

(defmethod print-object ((err frequency-table-too-big) stream)
  (print-unreadable-object (err stream :type t)
    (format stream "~S ~S ~D entries"
            (field-names err)
            (dimensions err)
            (apply #'* (dimensions err)))))


  
