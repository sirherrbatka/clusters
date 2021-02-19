(cl:in-package #:clusters.pam)


(defmethod read-split-merge-attempts-count ((algorithm-state algorithm-state))
  (~> algorithm-state clusters:parameters read-split-merge-attempts-count))


(defmethod read-merge-threshold ((algorithm-state algorithm-state))
  (~> algorithm-state clusters:parameters read-merge-threshold))


(defmethod read-split-threshold ((algorithm-state algorithm-state))
  (~> algorithm-state clusters:parameters read-split-threshold))


(defmethod read-select-medoids-attempts-count ((algorithm-state algorithm-state))
  (~> algorithm-state clusters:parameters read-select-medoids-attempts-count))


(defmethod read-medoids-count ((algorithm-state algorithm-state))
  (~> algorithm-state clusters:parameters read-medoids-count))
