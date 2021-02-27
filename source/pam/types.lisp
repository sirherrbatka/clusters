(cl:in-package #:clusters.pam)


(defclass parameters (clusters:parameters)
  ((%split-threshold
    :initarg :split-threshold
    :reader read-split-threshold)
   (%merge-threshold
    :initarg :merge-threshold
    :reader read-merge-threshold)
   (%select-medoids-attempts-count
    :initarg :select-medoids-attempts-count
    :reader read-select-medoids-attempts-count)
   (%split-merge-attempts-count
    :initarg :split-merge-attempts-count
    :reader read-split-merge-attempts-count)
   (%medoids-count
    :initarg :medoids-count
    :reader read-medoids-count))
  (:default-initargs
   :split-threshold nil
   :merge-threshold nil
   :select-medoids-attempts-count 20
   :split-merge-attempts-count 0))


(defclass algorithm-state (clusters:algorithm-state)
  ((%cluster-contents
    :initarg :cluster-contents
    :accessor access-cluster-contents)
   (%medoids-count
    :initarg :medoids-count
    :accessor access-medoids-count
    :reader read-medoids-count)
   (%unfinished-clusters
    :initarg :unfinished-clusters
    :accessor access-unfinished-clusters)
   (%distance-matrix
    :initarg :distance-matrix
    :accessor access-distance-matrix)
   (%indexes
    :initarg :indexes
    :accessor access-indexes)
   (%cluster-size
    :initarg :cluster-size
    :accessor access-cluster-size))
  (:default-initargs
   :cluster-contents nil
   :medoids-count nil
   :indexes nil
   :cluster-size nil
   :distance-matrix nil
   :unfinished-clusters nil))


(defclass pam-result (clusters:result)
  ((%distance-matrix :initarg :distance-matrix
                     :reader read-distance-matrix)))
