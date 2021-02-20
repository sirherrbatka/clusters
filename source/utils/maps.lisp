(cl:in-package #:clusters.utils)


(defun pmap (parallel type function &rest sequences)
  (if parallel
      (apply #'lparallel:pmap type function sequences)
      (apply #'map type function sequences)))


(defun transform (parallel function sequence &rest sequences)
  (if parallel
      (apply #'lparallel:pmap-into sequence function sequence sequences)
      (apply #'map-into sequence function sequence sequences)))


(defun pmap-into (parallel sequence function &rest sequences)
  (if parallel
      (apply #'lparallel:pmap-into sequence function sequences)
      (apply #'map-into sequence function sequences)))
