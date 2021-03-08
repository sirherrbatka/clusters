(cl:in-package #:clusters.pam)


(defmethod read-split-merge-attempts-count ((algorithm-state algorithm-state))
  (~> algorithm-state clusters:parameters read-split-merge-attempts-count))


(defmethod read-merge-threshold ((algorithm-state algorithm-state))
  (~> algorithm-state clusters:parameters read-merge-threshold))


(defmethod read-split-threshold ((algorithm-state algorithm-state))
  (~> algorithm-state clusters:parameters read-split-threshold))


(defmethod read-select-medoids-attempts-count ((algorithm-state algorithm-state))
  (~> algorithm-state clusters:parameters read-select-medoids-attempts-count))


(defmethod clusters:algorithm-state-class ((parameters parameters))
  'algorithm-state)


(defmethod clusters:algorithm-state-initialization-list
    append ((parameters parameters)
            data
            &rest all &key distance-matrix &allow-other-keys)
  (declare (ignore all))
  (let ((medoids-count (read-medoids-count parameters)))
    `(:medoids-count ,medoids-count
      :distance-matrix ,distance-matrix)))


(defmethod initialize-instance :after ((instance algorithm-state)
                                       &rest initargs)
  (declare (ignore initargs))
  (reset instance))


(defmethod clusters:run-algorithm ((state algorithm-state))
  (build-clusters state)
  state)


(defmethod clusters:result-initialization-list
    append ((state algorithm-state))
  (let ((cluster-contents (access-cluster-contents state))
        (distance-matrix (access-distance-matrix state)))
    `(:cluster-indexes ,cluster-contents
      :distance-matrix ,distance-matrix)))


(defmethod clusters:result-class
    ((parameters parameters))
  'pam-result)


(defmethod clusters:calculate-silhouette* :around
    ((parameters parameters)
     clustering-result
     &optional distance-matrix)
  (if (null distance-matrix)
      (call-next-method parameters
                        clustering-result
                        (read-distance-matrix clustering-result))
      (call-next-method)))
