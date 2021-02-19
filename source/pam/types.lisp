(cl:in-package #:clusters.pam)


(defclass parameters (clusters:parameters)
  ((%distance-function
    :initarg :distance-function
    :reader clusters:distance-function)
   (%minimal-cluster-size
    :initarg :minimal-cluster-size
    :reader minimal-cluster-size)
   (%split-threshold
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
    :reader read-medoids-count)
   (%cluster-sample-size
    :initarg :cluster-sample-size
    :reader read-cluster-sample-size))
  (:default-initargs
   :split-threshold nil
   :merge-threshold nil
   :select-medoids-attempts-count 20
   :minimal-cluster-size 1
   :split-merge-attempts-count 0))


(defclass algorithm-state (clusters:algorithm-state)
  ((%cluster-contents
    :initarg :cluster-contents
    :type vector
    :accessor access-cluster-contents)
   (%unfinished-clusters
    :initarg :unfinished-clusters
    :accessor access-unfinished-clusters)
   (%distance-matrix
    :initarg :distance-matrix
    :accessor access-distance-matrix)
   (%indexes
    :initarg :indexes
    :reader read-indexes)))
