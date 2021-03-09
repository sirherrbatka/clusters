(cl:in-package #:clusters.clarans)


(defgeneric y (state))
(defgeneric medoids (state))
(defgeneric distortion (state))
(defgeneric d (state))

(defgeneric (setf y) (new-value state))
(defgeneric (setf medoids) (new-value state))
(defgeneric (setf distortion) (new-value state))
(defgeneric (setf d) (new-value state))
