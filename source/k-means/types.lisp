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
                   :reader read-medoids-count)
   (%iterations :initarg :iterations
                :reader read-iterations)
   (%distortion-epsilon :initarg :distortion-epsilon
                        :type single-float
                        :reader read-distortion-epsilon))
  (:default-initargs :iterations nil))


(defmethod read-medoids-count ((state algorithm-state))
  (~> state clusters:parameters read-medoids-count))


(defmethod read-distortion-epsilon ((state algorithm-state))
  (~> state clusters:parameters read-distortion-epsilon))


(defmethod read-iterations ((state algorithm-state))
  (~> state clusters:parameters read-iterations))


(defmethod initialize-instance :after ((object k-means-algorithm-state)
                                       &rest all)
  (declare (ignore all))
  (bind (((:slots %value-key %distortion-epsilon
                  %medoids-count %iterations)
          object))
    (ensure-functionf %value-key)
    (assert (typep %distortion-epsilon 'single-float))
    (check-type %medoids-count integer)
    (check-type %iterations (or integer null))))
