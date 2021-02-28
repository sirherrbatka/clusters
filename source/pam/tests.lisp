(cl:in-package :clusters.pam)

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
                         :merge-threshold 50
                         :split-threshold 150
                         :split-merge-attempts-count 5
                         :medoids-count 10))
       (distance-matrix (clusters.utils:distance-matrix
                         nil
                         #'metric
                         data))
       (clusters (clusters:cluster
                  parameters
                  data
                  :distance-matrix distance-matrix)))
  (declare (optimize (debug 3) (safety 3)))
  (prove:is (length (clusters:cluster-contents clusters))
            10))

(prove:finalize)
