(cl:in-package #:clusters.clara)


(defclass parameters (clusters:parameters)
  ((%distance-function :initarg :distance-function
                       :reader clusters:distance-function)
   (%sample-size :initarg :sample-size
                 :reader sample-size)
   (%sample-count :initarg :sample-count
                  :reader sample-count)
   (%pam :initarg :pam
         :reader pam)))


(defclass algorithm-state (clusters:algorithm-state)
  ((%cluster-contents :initarg :cluster-contents
                      :accessor access-cluster-contents))
  (:default-initargs :cluster-contents nil))
