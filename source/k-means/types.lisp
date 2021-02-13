(cl:in-package #:clusters.k-means)


(defclass algorithm-state ()
  ((%data :initarg :data
          :type vector
          :reader read-data)
   (%value-key :initarg :value-key
               :initarg :key
               :type function
               :reader read-value-key)
   (%silhouette-sample-size :initarg :silhouette-sample-size
                            :reader silhouette-sample-size)
   (%silhouette-sample-count :initarg :silhouette-sample-count
                             :reader silhouette-sample-count)
   (%clusters :initarg :clusters
              :type vector
              :reader read-clusters)
   (%distortion-epsilon :initarg :distortion-epsilon
                        :type single-float
                        :reader read-distortion-epsilon)
   (%medoids :initarg :medoids
             :type vector
             :accessor access-medoids)
   (%iterations :initarg :iterations
                :reader read-iterations)
   (%medoids-count :initarg :medoids-count
                   :type non-negative-fixnum
                   :reader read-medoids-count))
  (:default-initargs
   :clusters (vect)
   :value-key #'identity
   :silhouette-sample-count 15
   :silhouette-sample-size 500
   :iterations nil
   :medoids (vect)
   :data (vect)))


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
