(cl:in-package #:clusters.k-means)


(defun select-initial-medoids (state)
  (bind (((:slots %clusters %medoids) state)
         (data (clusters:data state))
         (medoids-count (read-medoids-count state)))
    (setf (fill-pointer %medoids) 0)
    (clusters.utils:draw-random-vector (clusters:indexes state)
                                       medoids-count
                                       %medoids)
    (map-into %medoids (curry #'aref data) %medoids)
    (adjust-array %clusters (fill-pointer %medoids)
                  :fill-pointer (fill-pointer %medoids))
    (map-into %clusters #'vect))
  state)


(defun assign-data-points-to-medoids (state)
  (bind (((:slots %clusters %medoids) state)
         (clusters %clusters)
         (locks (~> clusters length make-array
                    (map-into (lambda () (bt:make-lock)))))
         (data (clusters:data state))
         (medoids %medoids)
         (value-key (clusters:key-function state))
         (length (length medoids)))
    (declare (type fixnum length)
             (type simple-vector locks)
             (type function value-key)
             (type vector clusters medoids))
    (assert (eql length (length clusters)))
    (map nil (lambda (cluster)
               (setf (fill-pointer cluster) 0))
         clusters)
    (iterate
      (clusters.utils:pmap
       (clusters:parallelp state)
       nil
       (lambda (data-point
           &aux (data (funcall value-key (aref data data-point))))
         (iterate
           (declare (type fixnum i)
                    (type (simple-array single-float (*)) medoid)
                    (type single-float distance))
           (for i from 0 below length)
           (for medoid = (aref medoids i))
           (for distance = (clusters.metric:euclid
                            medoid data))
           (finding i minimizing distance into cluster)
           (finally
            (bt:with-lock-held ((aref locks cluster))
              (vector-push-extend data-point (aref clusters cluster))))))
       (clusters:indexes state))
      (while (~> (extremum %clusters #'< :key #'length)
                 length
                 zerop))
      (select-initial-medoids state)))
  state)


(defun distortion (state)
  (bind (((:slots %clusters %medoids) state)
         (data (clusters:data state))
         (value-key (clusters:key-function state)))
    (declare (type function value-key))
    (~>> (clusters.utils:pmap
          (clusters:parallelp state)
          '(vector single-float)
          (lambda (cluster medoid)
            (declare (type (vector t) cluster))
            (iterate
              (declare (type fixnum size i)
                       (type (simple-array single-float (*)) c)
                       (type single-float sum))
              (with sum = 0.0)
              (with size = (length cluster))
              (for i from 0 below size)
              (for c = (funcall value-key (aref data  (aref cluster i))))
              (iterate
                (declare (type fixnum size i)
                         (type single-float error))
                (with size = (length c))
                (for i from 0 below size)
                (for error = (- (the single-float (aref c i))
                                (the single-float (aref medoid i))))
                (incf sum (expt error 2)))
              (finally (return sum))))
          %clusters
          %medoids)
         (reduce #'+))))


(defun select-new-medoids (state)
  (bind (((:slots %medoids %clusters)
          state)
         (value-key (clusters:key-function state))
         (data (clusters:data state)))
    (setf %medoids
          (clusters.utils:pmap
           (clusters:parallelp state)
           'vector
           (lambda (cluster medoid)
             (iterate
               (with new-medoid = (make-array (length medoid)
                                              :element-type 'single-float
                                              :initial-element 0.0))
               (for c in-vector cluster)
               (map-into new-medoid
                         #'+
                         new-medoid
                         (funcall value-key (aref data c)))
               (finally
                (return (map-into new-medoid
                                  (rcurry #'/ (length cluster))
                                  new-medoid)))))
           %clusters
           %medoids)))
  state)
