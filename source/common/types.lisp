(cl:in-package #:clusters)


(defclass result ()
  ((%cluster-contents :initarg :cluster-contents
                      :type vector
                      :reader cluster-contents)
   (%distance-function :initarg :distance-function
                       :type function
                       :reader distance-function)
   (%silhouette-sample-size :initarg :silhouette-sample-size
                            :type fixnum
                            :reader silhouette-sample-size)
   (%silhouette-sample-count :initarg :silhouette-sample-count
                             :type fixnum
                             :reader silhouette-sample-count)
   (%key-function :initarg :key-function
                  :initform #'identity
                  :type function
                  :reader key-function)
   (%silhouette :initarg :silhouette
                :type (vector single-float)
                :reader silhouette)))
