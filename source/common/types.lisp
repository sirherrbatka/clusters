(cl:in-package #:clusters)


(defclass parameters-holder ()
  ((%parameters :initarg :parameters
                :reader parameters)))


(defclass result (parameters-holder)
  ((%cluster-indexes :initarg :cluster-indexes
                     :type vector
                     :reader cluster-indexes)
   (%indexes :initarg :indexes
             :reader indexes)
   (%data :initarg :data
          :type vector
          :reader data)
   (%silhouette :initarg :silhouette
                :type (vector single-float)
                :reader silhouette)))


(defclass parameters ()
  ((%parallelp :initarg :parallelp
               :reader parallelp)
   (%key-function :initarg :key-function
                  :reader key-function)
   (%silhouette-sample-count :initarg :silhouette-sample-count
                             :reader silhouette-sample-count)
   (%silhouette-sample-size :initarg :silhouette-sample-size
                            :reader silhouette-sample-size))
  (:default-initargs
   :key-function #'identity
   :parallelp nil
   :silhouette-sample-count 15
   :silhouette-sample-size 500))


(defclass algorithm-state (parameters-holder)
  ((%data :initarg :data
          :reader data)
   (%indexes :initarg :indexes
             :accessor indexes))
  (:default-initargs :data (vect)
                     :indexes nil))
