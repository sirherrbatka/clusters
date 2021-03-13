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


(defmethod initialize-instance :after ((instance parameters) &rest all)
  (declare (ignore all))
  (bind (((:accessors merge-threshold
                      select-medoids-attempts-count
                      split-merge-attempts-count
                      split-threshold
                      medoids-count)
          instance))
    (cl-ds.utils:check-value split-merge-attempts-count
      (check-type split-merge-attempts-count integer)
      (assert (>= split-merge-attempts-count 0)
              (split-merge-attempts-count)
              'cl-ds:argument-value-out-of-bounds
              :value split-merge-attempts-count
              :argument :split-merge-attempts-count
              :bounds '(>= split-merge-attempts-count 0)
              :format-control "SPLIT-MERGE-ATTEMPS-COUNT should be equal or above 0."))
    (cl-ds.utils:check-value split-threshold
      (check-type split-threshold (or null integer))
      (assert (or (null split-threshold) (> split-threshold 0))
              (split-threshold)
              'cl-ds:argument-value-out-of-bounds
              :value split-threshold
              :argument :merge-threshold
              :bounds '(> split-threshold 0)
              :format-control "MERGE-THRESHOLD should be greater then 0."))
    (cl-ds.utils:check-value merge-threshold
      (check-type merge-threshold (or null integer))
      (assert (or (null merge-threshold) (> merge-threshold 0)
                  (or (null split-threshold) (< merge-threshold split-threshold)))
              (merge-threshold)
              'cl-ds:argument-value-out-of-bounds
              :value merge-threshold
              :argument :merge-threshold
              :bounds '(> split-threshold merge-threshold 0)
              :format-control "MERGE-THRESHOLD should be greater then 0 and below split-threshold."))
    (cl-ds.utils:check-value select-medoids-attempts-count
      (check-type select-medoids-attempts-count integer)
      (assert (> select-medoids-attempts-count 0)
              (select-medoids-attempts-count)
              'cl-ds:argument-value-out-of-bounds
              :value merge-threshold
              :argument :select-medoids-attempts-count
              :bounds '(> select-medoids-attempts-count 0)
              :format-control "SELECT-MEDOIDS-COUNT should be greater then 0."))
    (cl-ds.utils:check-value medoids-count
      (check-type medoids-count integer)
      (assert (> medoids-count 1)
              (medoids-count)
              'cl-ds:argument-value-out-of-bounds
              :value medoids-count
              :argument :medoids-count
              :bounds '(> medoids-count 1)
              :format-control "MEDOIDS-COUNT should be greater then 1."))))


(defmethod initialize-instance :after ((instance algorithm-state) &rest all)
  (declare (ignore all))
  (bind (((:accessors (data clusters:data))
          instance)
         ((:accessors medoids-count) (clusters:parameters instance)))
    (cl-ds.utils:check-value data
      (assert (< medoids-count (length data))
              (data)
              'cl-ds:out-of-bounds
              :value data
              :bounds `(> ,medoids-count)
              :format-control "Can't cluster when medoids-count is above or equal the number of elements in data!"))))
