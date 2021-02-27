(cl:in-package #:clusters)


(defun cluster-values (data index-vector)
  (map 'vector
       (lambda (i) (aref data i))
       index-vector))
