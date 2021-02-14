(cl:in-package #:clusters)


(defun make-algorithm-state (parameters data &rest arguments)
  (apply #'make
         (state-class parameters)
         (apply #'algorithm-state-initialization-list
                parameters data arguments)))


(defun obtain-result (state)
  (apply #'make 'result
         (result-initialization-list state)))


(defun cluster (parameters data &rest arguments)
  (let ((algorithm-state (apply #'make
                                (algorithm-state-class parameters)
                                (apply #'algorithm-state-initialization-list
                                       parameters
                                       data
                                       arguments))))
    (run-algorithm algorithm-state)
    (obtrain-result algorithm-state)))
