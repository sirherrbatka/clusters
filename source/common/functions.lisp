(cl:in-package #:clusters)


(defun make-algorithm-state (parameters data &rest arguments)
  (apply #'make
         (algorithm-state-class parameters)
         (apply #'algorithm-state-initialization-list
                parameters data arguments)))


(defun obtain-result (state)
  (apply #'make
        (result-class state)
        (result-initialization-list state)))


(defun cluster (parameters data &rest arguments)
  (let ((algorithm-state (apply #'make
                                (algorithm-state-class parameters)
                                (apply #'algorithm-state-initialization-list
                                       parameters
                                       data
                                       arguments))))
    (run-algorithm algorithm-state)
    (obtain-result algorithm-state)))


(defun calculate-silhouette (result &optional distance-matrix)
  (calculate-silhouette* (parameters result)
                         result
                         distance-matrix))
