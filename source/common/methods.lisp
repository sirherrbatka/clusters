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
            &rest arguments
            &key
            &allow-other-keys)
  (declare (ignore arguments))
  `(:parameters ,parameters
    :data ,data))


(defmethod result-initialization-list
    append ((state algorithm-state))
  `(:parameters ,(parameters state)))


(defmethod result-class ((parameters parameters))
  'result)


(defmethod result-class ((state algorithm-state))
  (~> state parameters result-class))


(defmethod silhouette-cluster-contents ((state result))
  (cluster-contents state))


(defmethod calculate-silhouette* ((parameters parameters)
                                  clustering-result
                                  &optional distance-matrix)
  (bind (((:flet distance-difference (intra inter))
          (cond ((null intra) 0.0)
                ((null inter) -1.0)
                ((= intra inter) 0.0)
                (t (coerce (/ (- inter intra) (max intra inter))
                           'single-float))))
         (silhouette-sample-count (silhouette-sample-count clustering-result))
         ((:flet silhouette (sample.whole))
          (iterate
            (with (sample . whole) = sample.whole)
            (with result = (make-array (length sample)
                                       :element-type 'single-float))
            (with distance-matrix = (or distance-matrix
                                        (distance-matrix clustering-result
                                                         whole)))
            (for sub in-vector sample)
            (for i from 0)
            (for inter-distances = (inter-cluster-distances distance-matrix
                                                            sub
                                                            sample))
            (for intra-distances = (intra-cluster-distances distance-matrix
                                                            sub))
            (setf (aref result i)
                  (~> (map '(vector single-float) #'distance-difference
                           intra-distances inter-distances)
                      (reduce #'+ _)
                      (/ (length sub))
                      (coerce 'single-float)))
            (finally (return result)))))
    (~>> silhouette-sample-count
         make-array
         (map-into _
                   (curry #'select-random-cluster-subsets
                          clustering-result
                          (~> distance-matrix not null)))
         (lparallel:pmap 'list #'silhouette)
         (apply #'map '(vector single-float)
                (compose (rcurry #'coerce 'single-float)
                         #'+))
         (clusters.utils:transform
          nil
          (compose (rcurry #'coerce 'single-float)
                   (rcurry #'/ silhouette-sample-count))))))
