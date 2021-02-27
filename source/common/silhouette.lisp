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


(defun distance-matrix (parallelp state whole)
  (clusters.utils:distance-matrix parallelp
                                  (distance-function state)
                                  whole))


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


(defun select-random-cluster-subsets (state distance-matrix-supplied)
  (declare (optimize (debug 3)))
  (bind ((sample-size (silhouette-sample-size state))
         (key-function (key-function state))
         (cluster-contents (silhouette-cluster-contents state))
         (total-size (reduce #'+ cluster-contents
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
                             (compose key-function
                                      (curry #'aref cluster)
                                      (curry #'random size)))))
               cluster-contents))
         (sizes (scan #'+ sample :initial-value 0 :key #'length))
         (total-size (last-elt sizes))
         (whole-sample (make-array total-size)))
    (iterate
      (for cluster in-vector sample)
      (for offset in (cons 0 sizes))
      (iterate
        (for j from 0 below (length cluster))
        (for i from offset)
        (if distance-matrix-supplied
            (setf (aref whole-sample i) (aref cluster j))
            (shiftf (aref whole-sample i) (aref cluster j) i))))
    (list* sample whole-sample)))


(defun average-distance-to-element (distance-matrix element cluster)
  (iterate
    (for count = (clusters.utils:half-matrix-size->count distance-matrix))
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
