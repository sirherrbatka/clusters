(cl:in-package #:cl-data-structures.utils.metric)


(define-constant +sqrt2+ (sqrt 2))


(defun hellinger-metric (q p)
  (iterate
    (for a in-vector q)
    (for b in-vector p)
    (sum (~> (- (sqrt a) (sqrt b))
             (/ +sqrt2+)
             (expt 2))
         into result)
    (finally (return (sqrt result)))))
