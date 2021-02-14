(cl:in-package #:clusters.k-means)


(defmethod clusters:state-class ((parameters parameters))
  'algorithm-state)


(defmethod clusters:algorithm-state-initialization-list
    ((parameters parameters) data
     &rest all &key &allow-other-keys)
  (declare (ignore all))
  (let ((medoids-count (read-medoids-count parameters)))
    `(:medoids ,(make-array medoids-count
                            :adjustable t
                            :fill-pointer (read-medoids-count parameters))
      :clusters ,(~> (make-array medoids-count
                                 :adjustable t
                                 :fill-pointer medoids-count)
                     (map-into #'vect)))))
