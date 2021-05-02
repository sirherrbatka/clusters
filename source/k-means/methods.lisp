(cl:in-package #:clusters.k-means)


(defmethod clusters:algorithm-state-class ((parameters parameters))
  'algorithm-state)


(defmethod clusters:algorithm-state-initialization-list
    append ((parameters parameters) data
             &rest all &key &allow-other-keys)
  (declare (ignore all))
  (let ((medoids-count (read-medoids-count parameters)))
    `(:medoids ,(make-array medoids-count
                            :adjustable t
                            :fill-pointer medoids-count)
      :clusters ,(~> (make-array medoids-count
                                 :adjustable t
                                 :fill-pointer medoids-count)
                     (map-into #'vect)))))


(defmethod clusters:run-algorithm ((state algorithm-state))
  (iterate
    (with distortion-epsilon = (read-distortion-epsilon state))
    (with iterations = (read-iterations state))
    (for i from 0)
    (while (or (null iterations) (< i iterations)))
    (assign-data-points-to-medoids state)
    (select-new-medoids state)
    (for distortion = (distortion state))
    (for prev-distortion
         previous distortion
         initially nil)
    (while (or (null prev-distortion)
               (< (abs (- distortion prev-distortion))
                  distortion-epsilon)))
    (finally (return state))))


(defmethod read-medoids-count ((state algorithm-state))
  (~> state clusters:parameters read-medoids-count))


(defmethod read-distortion-epsilon ((state algorithm-state))
  (~> state clusters:parameters read-distortion-epsilon))


(defmethod read-iterations ((state algorithm-state))
  (~> state clusters:parameters read-iterations))


(defmethod clusters:distance-function ((parameters parameters))
  #'clusters.metric:euclid)


(defmethod clusters:result-initialization-list
    append ((state algorithm-state))
  `(:cluster-indexes ,(read-clusters state)))


(defmethod initialize-instance :after ((state algorithm-state)
                                       &rest all)
  (declare (ignore all))
  (select-initial-medoids state))


(defmethod initialize-instance :after ((instance parameters) &rest all)
  (declare (ignore all))
  (bind (((:accessors iterations
                      medoids-count
                      distortion-epsilon)
          instance))
    (cl-ds.utils:check-value distortion-epsilon
      (check-type distortion-epsilon real)
      (assert (> distortion-epsilon 0)
              (distortion-epsilon)
              'cl-ds:argument-value-out-of-bounds
              :value distortion-epsilon
              :argument :distorction-epsilon
              :bounds '(> distortion-epsilon 0)
              :format-control "Distortion-epsilon should be above 0."))
    (cl-ds.utils:check-value iterations
      (check-type iterations integer)
      (assert (> iterations 0)
              (iterations)
              'cl-ds:argument-value-out-of-bounds
              :value iterations
              :argument :iterations
              :bounds '(> iterations 0)
              :format-control "Iterations should be above 0."))
    (cl-ds.utils:check-value medoids-count
      (check-type medoids-count integer)
      (assert (> medoids-count 1)
              (medoids-count)
              'cl-ds:argument-value-out-of-bounds
              :value medoids-count
              :argument :medoids-count
              :bounds '(> medoids-count 1)
              :format-control "MEDOIDS-COUNT should be greater then 1."))))


(defmethod initialize-instance :after ((instance algorithm-state) &rest all)
  (declare (ignore all))
  (bind (((:accessors (data clusters:data))
          instance)
         ((:accessors medoids-count) (clusters:parameters instance)))
    (cl-ds.utils:check-value data
      (assert (< medoids-count (length data))
              (data)
              'cl-ds:out-of-bounds
              :value data
              :bounds `(> ,medoids-count)
              :format-control "Can't cluster when medoids-count is above or equal the number of elements in data!"))))
