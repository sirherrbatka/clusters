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


(defun contains (medoids medoid)
  (not (null (position medoids medoid :test 'eql))))


(defun random-medoid (data indexes medoids)
  (iterate
    (with n = (length indexes))
    (for medoid = (~>> n random (aref indexes) (aref data)))
    (finding medoid such-that (not (contains medoids medoid)))))


(defun random-neighbor (data indexes medoids y d distance-function)
  (let* ((n (length indexes))
         (k (length medoids))
         (cluster (random k))
         (medoid (random-medoid data indexes medoids)))
    (iterate
      (for i from 0 below n)
      (for distance = (funcall distance-function
                               (aref data (aref indexes i))
                               (aref data (aref indexes medoid))))
      (cond ((> (aref d i) distance)
             (setf (aref y i) cluster
                   (aref d i) distance))
            ((= (aref y i) cluster)
             (setf (aref d i) distance)
             (iterate
               (for j from 0 below k)
               (unless (= j cluster)
                 (next-iteration))
               (for distance = (funcall distance-function
                                        (aref data (aref indexes i))
                                        (aref data (aref medoids j))))
               (when (> (aref d i) distance)
                 (setf (aref d i) distance
                       (aref y i) j))))))
    (reduce #'+ d)))


(defun to-cluster-contents (y indexes medoids-counts)
  (iterate
    (with result = (map-into (make-array medoids-counts) #'vect))
    (for cluster in-vector y)
    (for index in-vector indexes)
    (vector-push-extend (aref result cluster) index)
    (finally (return result))))
