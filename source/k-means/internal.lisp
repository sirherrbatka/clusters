(cl:in-package #:clusters.k-means)


(defun select-initial-medoids (state)
  (bind (((:slots %data %clusters %medoids %medoids-count %value-key) state))
    (setf (fill-pointer %medoids) 0)
    (clusters.utils:draw-random-vector %data %medoids-count %medoids)
    (map-into %medoids %value-key %medoids)
    (adjust-array %clusters (fill-pointer %medoids)
                  :fill-pointer (fill-pointer %medoids))
    (map-into %clusters #'vect))
  state)


(defun assign-data-points-to-medoids (state)
  (bind (((:slots %data %clusters %medoids %medoids-count %value-key) state)
         (clusters %clusters)
         (locks (~> clusters length make-array
                    (map-into (lambda () (bt:make-lock)))))
         (medoids %medoids)
         (value-key %value-key)
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
      (lparallel:pmap
       nil
       (lambda (data-point
           &aux (data (funcall value-key data-point)))
         (check-type data (simple-array single-float (*)))
         (let ((i (iterate
                    (declare (type fixnum i)
                             (type (simple-array single-float (*)) medoid)
                             (type single-float distance))
                    (for i from 0 below length)
                    (for medoid = (aref medoids i))
                    (for distance = (clusters.metric:euclid
                                     medoid data))
                    (finding i minimizing distance))))
           (bt:with-lock-held ((aref locks i))
             (vector-push-extend data-point (aref clusters i)))))
       %data)
      (while (~> (extremum %clusters #'< :key #'length)
                 length
                 zerop))
      (select-initial-medoids state)))
  state)


(defun distortion (state)
  (bind (((:slots %data %clusters %medoids %medoids-count %value-key) state)
         (value-key %value-key))
    (declare (type function value-key))
    (~>> (lparallel:pmap
          '(vector single-float)
          (lambda (cluster medoid)
            (declare (type (simple-array single-float (*)) medoid)
                     (type (vector t) cluster))
            (check-type medoid (simple-array single-float (*)))
            (iterate
              (declare (type fixnum size i)
                       (type (simple-array single-float (*)) c)
                       (type single-float sum))
              (with sum = 0.0)
              (with size = (length cluster))
              (for i from 0 below size)
              (for c = (funcall value-key (aref cluster i)))
              (check-type c (simple-array single-float (*)))
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


(defun obtain-result (state)
  (bind (((:slots %silhouette-sample-size %value-key %silhouette-sample-size %clusters)
          state))
    (make 'cluster:clustering-result
          :cluster-contents %clusters
          :distance-function #'cl-ds.utils.metric:euclid-metric
          :silhouette-sample-size %silhouette-sample-size
          :key-function %value-key
          :silhouette-sample-count %silhouette-sample-count)))


(defun make-state (data medoids-count distortion-epsilon all)
  (~> (apply #'make 'algorithm-state
             :data data
             :cluster-contents (~> (make-array medoids-count
                                               :adjustable t
                                               :fill-pointer medoids-count)
                                   (map-into #'vect))
             :medoids (make-array medoids-count
                                  :adjustable t
                                  :fill-pointer medoids-count)
             :medoids-count medoids-count
             :distortion-epsilon distortion-epsilon
             all)
      select-initial-medoids))


(defun select-new-medoids (state)
  (bind (((:slots %medoids %value-key %silhouette-sample-size %clusters)
          state))
    (setf %medoids
          (lparallel:pmap
           'vector
           (lambda (cluster medoid)
             (iterate
               (with new-medoid = (make-array (length medoid)
                                              :element-type 'single-float
                                              :initial-element 0.0))
               (for c in-vector cluster)
               (map-into new-medoid #'+ new-medoid c)
               (finally
                (return (map-into new-medoid
                                  (rcurry #'/ (length cluster))
                                  new-medoid)))))
           %clusters
           %medoids)))
  state)
