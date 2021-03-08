(cl:in-package #:clusters.clarans)


(defmethod clusters:result-initialization-list
    append ((state algorithm-state))
  nil)


(defmethod clusters:algorithm-state-class ((parameters parameters))
  'algorithm-state)


(defmethod pam ((state algorithm-state))
  (~> state clusters:parameters pam))


(defmethod sample-count ((state algorithm-state))
  (~> state clusters:parameters sample-count))


(defmethod sample-size ((state algorithm-state))
  (~> state clusters:parameters sample-size))


(defmethod clusters:result-initialization-list
    append ((state algorithm-state))
  (let ((cluster-contents (access-cluster-contents state)))
    `(:cluster-indexes ,cluster-contents
      :silhouette ,silhouette)))


(defmethod clusters:run-algorithm ((state algorithm-state))
  (let* ((data (clusters:data state))
         (indexes (clusters:indexes state))
         (pam (pam state))
         (parameters (clusters:parameters state))
         (distance-function (clusters:distance-function state)))
    (iterate
      (repeat (sample-count state))
      ;; todo
      )
    ))
