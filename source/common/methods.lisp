(cl:in-package #:clusters)


(defmethod parallelp ((instance parameters-holder))
  (~> instance parameters parallelp))


(defmethod distance-function ((instance parameters-holder))
  (~> instance parameters distance-function))


(defmethod key-function ((instance parameters-holder))
  (~> instance parameters key-function))


(defmethod silhouette-sample-size ((instance parameters-holder))
  (~> instance parameters silhouette-sample-size))


(defmethod silhouette-sample-count ((instance parameters-holder))
  (~> instance parameters silhouette-sample-count))


(defmethod algorithm-state-initialization-list
    append ((parameters parameters)
            data
            &rest arguments &key
            &allow-other-keys)
  (declare (ignore arguments))
  `(:parameters ,parameters
    :data ,data))


(defmethod result-initialization-list
    append ((state algorithm-state))
  `(:parameters ,(parameters state)
    :indexes ,(indexes state)
    :data ,(data state)))


(defmethod result-class ((parameters parameters))
  'result)


(defmethod result-class ((state algorithm-state))
  (~> state parameters result-class))


(defmethod cluster-contents ((result result))
  (map 'vector
       (curry #'cluster-values (data result))
       (cluster-indexes result)))


(defmethod calculate-silhouette* ((parameters parameters)
                                  clustering-result
                                  &optional distance-matrix)
  (bind (((:flet distance-difference (intra inter))
          (cond ((null intra) 0.0)
                ((null inter) -1.0)
                ((= intra inter) 0.0)
                (t (/ (- inter intra) (max intra inter)))))
         (silhouette-sample-count (silhouette-sample-count parameters))
         ((:flet silhouette (sample.whole))
          (iterate
            (with (sample . whole) = sample.whole)
            (with result = (make-array (length sample)
                                       :initial-element 0.0))
            (with distance-matrix =
                  (or distance-matrix
                      (distance-matrix clustering-result whole)))
            (for sub in-vector sample)
            (for i from 0)
            (for inter-distances = (inter-cluster-distances distance-matrix
                                                            sub
                                                            sample))
            (for intra-distances = (intra-cluster-distances distance-matrix
                                                            sub))
            (setf (aref result i)
                  (~> (map 'vector #'distance-difference
                           intra-distances inter-distances)
                      (reduce #'+ _)
                      (/ (length sub))))
            (finally (return result)))))
    (~>> silhouette-sample-count
         make-array
         (map-into _
                   (curry #'select-random-cluster-subsets
                          clustering-result
                          (~> distance-matrix not null)))
         (clusters.utils:pmap (parallelp parameters) 'list #'silhouette)
         (apply #'map 'vector #'+)
         (clusters.utils:transform
          nil
          (rcurry #'/ silhouette-sample-count)))))


(defmethod initialize-instance :after ((instance algorithm-state)
                                       &rest initargs)
  (declare (ignore initargs))
  (check-type (data instance) vector)
  (ensure (indexes instance)
    (iterate
      (with n = (~> instance data length))
      (with result = (make-array n :element-type 'fixnum))
      (for i from 0 below n)
      (setf (aref result i) i)
      (finally (return result)))))
