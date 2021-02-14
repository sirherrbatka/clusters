(cl:in-package #:clusters.k-means)


(defmethod clusters:algorithm-state-class ((parameters parameters))
  'algorithm-state)


(defmethod clusters:algorithm-state-initialization-list
    ((parameters parameters) data
     &rest all &key &allow-other-keys)
  (declare (ignore all))
  (let ((medoids-count (read-medoids-count parameters)))
    `(:medoids ,(make-array medoids-count
                            :adjustable t
                            :fill-pointer (read-medoids-count parameters))
      :clusters ,(~> (make-array medoids-count
                                 :adjustable t
                                 :fill-pointer medoids-count)
                     (map-into #'vect)))))


(defmethod clusters:run-algorithm ((state algorithm-state))
  (iterate
    (with distortion-epsilon = (read-distortion-epsilon state))
    (with iterations = (read-iterations state))
    (for i from 0)
    (while (or (null iterations) (< i iterations)))
    (assign-data-points-to-medoids state)
    (select-new-medoids state)
    (for distortion = (distortion state))
    (for prev-distortion
         previous distortion
         initially nil)
    (while (or (null prev-distortion)
               (< (abs (- distortion prev-distortion))
                  distortion-epsilon)))
    (finally (return state))))


(defmethod read-medoids-count ((state algorithm-state))
  (~> state clusters:parameters read-medoids-count))


(defmethod read-distortion-epsilon ((state algorithm-state))
  (~> state clusters:parameters read-distortion-epsilon))


(defmethod read-iterations ((state algorithm-state))
  (~> state clusters:parameters read-iterations))


(defmethod clusters:distance-function ((parameters parameters))
  #'clusters.metric:euclid)


(defmethod clusters:result-initialization-list
    append ((state algorithm-state))
  `(:cluster-contents ,(read-clusters state)))
