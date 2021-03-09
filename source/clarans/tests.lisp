(cl:in-package #:clusters.clarans)


(defun metric (a b)
  (coerce (abs (- a b))
          'single-float))

(prove:plan 1)

(let* ((data (~> (concatenate 'vector
                              (iota 100 :start 0)
                              (iota 300 :start 500)
                              (iota 100 :start 200))
                 shuffle))
       (parameters (make 'parameters
                         :parallelp nil
                         :medoids-count 10
                         :max-neighbor 200
                         :distance-function #'metric))
       (clusters (clusters:cluster
                  parameters
                  data)))
  (declare (optimize (debug 3) (safety 3)))
  (print (clusters:cluster-contents clusters))
  (prove:is (length (clusters:cluster-contents clusters))
            10))

(prove:finish)
