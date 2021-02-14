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
