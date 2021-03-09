(cl:in-package #:clusters.clarans)


(defun contains (medoids medoid)
  (not (null (position medoid medoids :test 'eql))))


(-> random-medoid (fixnum simple-vector) fixnum)
(defun random-medoid (n medoids)
  (iterate
    (for medoid = (random n))
    (finding medoid such-that (not (contains medoids medoid)))))


(defun random-neighbor (parallelp data indexes medoids y d distance-function)
  (declare (type (simple-array double-float (*)) d)
           (type vector data)
           (type (simple-array fixnum (*)) y)
           (type simple-vector indexes medoids))
  (let* ((n (length indexes))
         (k (length medoids))
         (cluster (random k))
         (distance-function (ensure-function distance-function))
         (medoid (random-medoid n medoids)))
    (declare (type fixnum n k cluster medoid))
    (clusters.utils:pmap
     parallelp
     nil
     (lambda (index &aux
                 (datum (aref data index))
                 (distance (coerce (funcall distance-function
                                            datum
                                            (aref data medoid))
                                      'double-float)))
       (declare (type double-float distance)
                (optimize (speed 3))
                (type fixnum index))
       (cond ((> (aref d index) distance)
              (setf (aref y index) cluster
                    (aref d index) distance)
              nil)
             ((= (aref y index) cluster)
              (setf (aref d index) distance)
              (iterate
                (declare (type fixnum j))
                (for j from 0 below k)
                (unless (= j cluster)
                  (next-iteration))
                (for distance = (funcall distance-function
                                         datum
                                         (~>> (aref medoids j)
                                              (aref data))))
                (when (> (aref d index) distance)
                  (setf (aref d index) (coerce distance 'double-float)
                        (aref y index) j))))))
     indexes)))


(defun to-cluster-contents (y indexes medoids-counts)
  (iterate
    (with result = (map-into (make-array medoids-counts) #'vect))
    (for cluster in-vector y)
    (for index in-vector indexes)
    (vector-push-extend index (aref result cluster))
    (finally (return result))))
