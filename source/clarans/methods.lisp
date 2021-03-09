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
         (n (length indexes))
         (distance-function (clusters:distance-function state))
         (medoids-count (medoids-count state))
         (new-medoids (make-array medoids-count :initial-element 0))
         (new-y (make-array n :initial-element 0))
         (new-d (make-array n :initial-element 0.0d0
                              :element-type 'double-float))
         ((:accessors y medoids distortion d)
          state))
    (ensure medoids (make-array medoids-count :initial-element 0))
    (ensure y (make-array n :initial-element 0))
    (ensure d (clusters.utils:seed data indexes medoids
                                   y distance-function))
    (ensure distortion (reduce #'+ d))
    (clusters.utils:copy-into new-d d)
    (clusters.utils:copy-into new-medoids medoids)
    (iterate
      (with max-neighbor = (max-neighbor state))
      (with neighbor = 1)
      (until (= neighbor max-neighbor))
      (for random-neighbor-distortion
           = (random-neighbor data indexes new-medoids
                              new-y new-d distance-function))
      (if (< random-neighbor-distortion distortion)
          (progn
            (setf neighbor 1
                  distortion random-neighbor-distortion)
            (clusters.utils:copy-into medoids new-medoids)
            (clusters.utils:copy-into y new-y)
            (clusters.utils:copy-into d new-d)
            (next-iteration))
          (progn
            (clusters.utils:copy-into new-medoids medoids)
            (clusters.utils:copy-into new-y y)
            (clusters.utils:copy-into new-d d)
            (incf neighbor))))))
