(cl:in-package #:clusters.k-means)


(defclass algorithm-state (clusters:algorithm-state)
  ((%clusters :initarg :clusters
              :type vector
              :reader read-clusters)
   (%medoids :initarg :medoids
             :type vector
             :accessor access-medoids))
  (:default-initargs
   :clusters (vect)
   :medoids (vect)))


(defclass parameters (clusters:parameters)
  ((%medoids-count :initarg :medoids-count
                   :type non-negative-fixnum
                   :reader read-medoids-count
                   :accessor medoids-count)
   (%iterations :initarg :iterations
                :reader read-iterations
                :accessor iterations)
   (%distortion-epsilon :initarg :distortion-epsilon
                        :type single-float
                        :accessor distortion-epsilon
                        :reader read-distortion-epsilon))
  (:default-initargs :iterations nil))
