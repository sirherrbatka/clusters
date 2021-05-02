(cl:in-package #:clusters.clarans)


(defmethod clusters:result-initialization-list
    append ((state algorithm-state))
  nil)


(defmethod clusters:algorithm-state-class ((parameters parameters))
  'algorithm-state)


(defmethod max-neighbor ((state algorithm-state))
  (~> state clusters:parameters max-neighbor))


(defmethod medoids-count ((state algorithm-state))
  (~> state clusters:parameters medoids-count))


(defmethod clusters:result-initialization-list
    append ((state algorithm-state))
  (bind (((:accessors y medoids distortion d medoids-count)
          state))
    `(:cluster-indexes ,(to-cluster-contents y
                                             (clusters:indexes state)
                                             medoids-count))))


(defmethod clusters:run-algorithm ((state algorithm-state))
  (bind ((data (clusters:data state))
         (indexes (clusters:indexes state))
         (parallelp (clusters:parallelp state))
         (n (length indexes))
         (distance-function (clusters:distance-function state))
         (medoids-count (medoids-count state))
         (new-medoids (make-array medoids-count
                                  :element-type 'fixnum
                                  :initial-element 0))
         (new-y (make-array n :initial-element 0 :element-type 'fixnum))
         (new-d (make-array n :initial-element 0.0d0
                              :element-type 'double-float))
         ((:accessors y medoids distortion d)
          state))
    (ensure medoids (make-array medoids-count
                                :initial-element 0
                                :element-type 'fixnum))
    (ensure y (make-array n :initial-element 0 :element-type 'fixnum))
    (ensure d (clusters.utils:seed data indexes medoids
                                   y distance-function))
    (ensure distortion (handler-case (reduce #'+ d)
                         (floating-point-overflow (e) (declare (ignore e))
                           most-positive-double-float)))
    (clusters.utils:copy-into new-d d)
    (clusters.utils:copy-into new-medoids medoids)
    (iterate
      (with max-neighbor = (max-neighbor state))
      (with neighbor = 1)
      (until (= neighbor max-neighbor))
      (random-neighbor parallelp data indexes new-medoids
                       new-y new-d distance-function)
      (for random-neighbor-distortion = (reduce #'+ new-d))
      (if (< random-neighbor-distortion distortion)
          (progn
            (setf neighbor 1
                  distortion random-neighbor-distortion)
            (clusters.utils:copy-into (the index-array medoids)
                                      (the index-array new-medoids))
            (clusters.utils:copy-into (the index-array y)
                                      (the index-array new-y))
            (clusters.utils:copy-into (the (simple-array double-float (*)) d)
                                      (the (simple-array double-float (*)) new-d)))
          (progn
            (clusters.utils:copy-into new-medoids medoids)
            (clusters.utils:copy-into new-y y)
            (clusters.utils:copy-into new-d d)
            (incf neighbor))))))


(defmethod initialize-instance :after ((instance parameters) &rest all)
  (declare (ignore all))
  (bind (((:accessors (distance-function distance-function)
                      (max-neighbor max-neighbor)
                      (medoids-count medoids-count))
          instance))
    (ensure-function distance-function)
    (cl-ds.utils:check-value max-neighbor
      (check-type max-neighbor integer)
      (assert (> max-neighbor 1)
              (max-neighbor)
              'cl-ds:argument-value-out-of-bounds
              :value max-neighbor
              :argument :max-neighbor
              :bounds '(> max-neighbor 1)
              :format-control "MAX-NEIGHBOR should be greater then 1."))
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
         ((:accessors max-neighbor medoids-count) (clusters:parameters instance)))
    (cl-ds.utils:check-value data
      (assert (<= max-neighbor (length data))
              (data)
              'cl-ds:out-of-bounds
               :value data
               :bounds `(>= ,max-neighbor)
               :format-control "Can't cluster when max-neighbor is above the number of elements in data!")
      (assert (< medoids-count (length data))
              (data)
              'cl-ds:out-of-bounds
              :value data
              :bounds `(> ,medoids-count)
              :format-control "Can't cluster when medoids-count is above or equal the number of elements in data!"))))
