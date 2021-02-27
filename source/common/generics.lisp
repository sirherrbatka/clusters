(cl:in-package #:clusters)


(defgeneric parallel-p (parameters))
(defgeneric parameters (result))
(defgeneric cluster-contents (result))
(defgeneric silhouette (result))
(defgeneric silhouette-cluster-contents (result))
(defgeneric data (result))
(defgeneric key-function (parameters))
(defgeneric algorithm-state-initialization-list
    (parameters data &rest arguments &key &allow-other-keys)
  (:method-combination append))
(defgeneric algorithm-state-class (parameters))
(defgeneric run-algorithm (state))
(defgeneric result-initialization-list (state)
  (:method-combination append))
(defgeneric result-class (parameters))
(defgeneric calculate-silhouette* (parameters result
                                   &optional distance-matrix))
