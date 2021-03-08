(cl:in-package #:clusters.clarans)


(defclass parameters (clusters:parameters)
  ((%distance-function :initarg :distance-function
                       :reader clusters:distance-function)
   (%max-neighbor :initarg :max-neighbor
                  :reader max-neighbor)
   (%medoids-count :initarg :medoids-counts
                   :reader medoids-counts)))


(defclass algorithm-state (clusters:algorithm-state)
  ((%cluster-contents :initarg :cluster-contents
                      :accessor access-cluster-contents))
  (:default-initargs
   :cluster-contents nil))
