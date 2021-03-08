(cl:in-package #:clusters.clarans)


(defun index-sample (state indexes)
  (let* ((sample-size (sample-size state))
         (result-size (min (length indexes)
                           sample-size))
         (result (make-array result-size :element-type 'fixnum))
         (shuffle-table (make-hash-table)))
    (iterate
      (for i from 0 below result-size)
      (setf (aref result i) i)
      (for random = (~>> (length indexes)
                         (random-in-range i)
                         (aref indexes)))
      (rotatef (aref result i)
               (gethash random shuffle-table random))
      (finally (return result)))))


(defun iota-vector (count)
  (iterate
    (with result = (make-array count))
    (for i from 0 below count)
    (setf (aref result i) i)
    (finally (return result))))


(defun assign-data-to-medoids ())


(defun calculate-cost (distance-function result)
  )
