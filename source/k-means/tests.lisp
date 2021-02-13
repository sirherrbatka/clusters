(cl:in-package :cl-user)
(defpackage :k-means-test-suite (:use :cl :prove :iterate))
(cl:in-package :k-means-test-suite)


(plan 2)

(defun generate-data-point (size)
  (cl-ds.utils:transform
      (lambda (x) (declare (ignore x))
        (serapeum:~>
         (serapeum:random-in-range 0 500)
         (coerce 'single-float)))
    (make-array size :element-type 'single-float)))


(defparameter *data*
  (iterate
    (with data = (serapeum:vect))
    (for i from 0 below 500)
    (vector-push-extend (generate-data-point 3)
                        data)
    (finally (return data))))


(let ((clusters (cl-ds.utils.cluster.k-means:k-means *data* 5 50.0)))
  (is (serapeum:~> clusters cl-ds.utils.cluster:cluster-contents length) 5)
  (ok (every (lambda (x) (not (zerop (length x))))
             (serapeum:~> clusters cl-ds.utils.cluster:cluster-contents))))

(finalize)
