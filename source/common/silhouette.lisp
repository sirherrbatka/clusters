(cl:in-package #:clusters)


(defun intra-cluster-distances (distance-matrix cluster)
  (map 'vector
       (lambda (c)
         (iterate
           (with length = (length cluster))
           (for k in-vector cluster)
           (when (eql c k)
             (next-iteration))
           (sum (clusters.utils:mref distance-matrix c k)
                into sum)
           (finally (return (if (eql 1 length)
                                nil
                                (coerce (/ sum (1- length))
                                        'single-float))))))
       cluster))


(defun distance-matrix (result &optional (indexes (indexes result)))
  (let ((data (data result))
        (distance-function (distance-function result))
        (value-key (key-function result)))
    (clusters.utils:distance-matrix nil
                                    (lambda (a b)
                                      (funcall distance-function
                                               (funcall value-key (aref data a))
                                               (funcall value-key (aref data b))))
                                    indexes)))


(defun inter-cluster-distances (distance-matrix cluster sample)
  (map 'vector
       (lambda (k)
         (iterate
           (for other-cluster in-vector sample)
           (when (eq other-cluster cluster)
             (next-iteration))
           (minimize (average-distance-to-element distance-matrix
                                                  k
                                                  other-cluster))))
       cluster))


(defun select-random-cluster-subsets (result distance-matrix-supplied)
  (bind ((sample-size (silhouette-sample-size result))
         (cluster-indexes (cluster-indexes result))
         (total-size (reduce #'+ cluster-indexes
                             :key #'length
                             :initial-value 0))
         (sample-ratio (min 1 (/ sample-size total-size)))
         (sample
          (map 'vector
               (lambda (cluster)
                 (let* ((size (length cluster))
                        (sample-size (ceiling (* size sample-ratio)))
                        (result (make-array sample-size
                                            :element-type 'fixnum)))
                   (map-into result
                             (compose (curry #'aref cluster)
                                      (curry #'random size)))))
               cluster-indexes))
         (total-size (reduce #'+ sample :initial-value 0 :key #'length))
         (whole-sample (make-array total-size)))
    (iterate
      (with i = 0)
      (for cluster in-vector sample)
      (iterate
        (for j from 0 below (length cluster))
        (setf (aref whole-sample i) (aref cluster j))
        (unless distance-matrix-supplied
          (setf (aref cluster j) i))
        (incf i)))
    (list* sample whole-sample)))


(defun average-distance-to-element (distance-matrix element cluster)
  (iterate
    (with count = (~> distance-matrix length clusters.utils:half-matrix-size->count))
    (for c in-vector cluster)
    (for distance = (clusters.utils:mref distance-matrix
                                         (the fixnum c)
                                         (the fixnum element)
                                         count))
    (sum distance into sum)
    (finally (return (coerce (/ sum (length cluster))
                             'single-float)))))


(defmethod silhouette :before ((object result))
  (unless (slot-boundp object '%silhouette)
    (setf (slot-value object '%silhouette) (calculate-silhouette object))))
