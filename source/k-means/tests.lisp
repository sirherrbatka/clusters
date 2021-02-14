(cl:in-package :clusters.k-means)

(prove:plan 2)

(defun generate-data-point (size)
  (map-into (make-array size :element-type 'single-float)
            (lambda () (random-in-range 0.0 500.0))))

(defparameter *data*
  (iterate
    (with data = (vect))
    (for i from 0 below 500)
    (vector-push-extend (generate-data-point 3)
                        data)
    (finally (return data))))

(defparameter *parameters*
  (make 'parameters
        :medoids-count 5
        :distortion-epsilon 50.0))

(let ((clusters (clusters:cluster *parameters* *data*)))
  (prove:is (~> clusters clusters:cluster-contents length) 5)
  (prove:ok (every (lambda (x) (not (zerop (length x))))
                   (~> clusters clusters:cluster-contents))))

(prove:finalize)
