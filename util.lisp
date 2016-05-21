(in-package :chordalysis)

(defun hash-table-keys (h)
  (loop for key being the hash-key of h
     collect key))
