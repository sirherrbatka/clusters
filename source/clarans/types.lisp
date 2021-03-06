(cl:in-package #:clusters.clarans)


(deftype index-array ()
  `(simple-array fixnum (*)))


(defclass parameters (clusters:parameters)
  ((%distance-function :initarg :distance-function
                       :accessor distance-function
                       :reader clusters:distance-function)
   (%max-neighbor :initarg :max-neighbor
                  :accessor max-neighbor)
   (%medoids-count :initarg :medoids-count
                   :accessor medoids-count)))


(defclass algorithm-state (clusters:algorithm-state)
  ((%y :initarg :y
       :accessor y)
   (%medoids :initarg :medoids
             :accessor medoids)
   (%distortion :initarg :distortion
                :accessor distortion)
   (%d :initarg :d
       :accessor d))
  (:default-initargs
   :y nil :medoids nil :distortion nil :d nil))
