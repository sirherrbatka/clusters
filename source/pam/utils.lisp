(cl:in-package #:clusters.pam)


(defun clear-cluster-contents (state)
  (setf (fill-pointer (access-cluster-contents state))
        (read-medoids-count state))
  (map nil
       (curry #'(setf fill-pointer) 1)
       (access-cluster-contents state)))


(defun order-medoids (state)
  (setf (access-cluster-contents state)
        (~> state
            access-cluster-contents
            (sort #'< :key #'first-elt))))


(defun medoidp (state index)
  (let* ((cluster-contents (access-cluster-contents state))
         (position (position
                    index
                    cluster-contents
                    :key #'first-elt))
         (cluster-count (length cluster-contents)))
    (and (< position cluster-count)
         (= index (~> cluster-contents
                      (aref position)
                      first-elt)))))


(defun closest-medoid (state index)
  (unless (medoidp state index)
    (iterate
      (declare (type number distance))
      (with result = 0)
      (with distance-matrix = (access-distance-matrix state))
      (with cluster-contents = (access-cluster-contents state))
      (with count = (~> distance-matrix
                        length
                        clusters.utils:half-matrix-size->count))
      (for cluster in-vector cluster-contents)
      (for i from 0)
      (for medoid = (aref cluster 0))
      (for distance = (clusters.utils:mref distance-matrix
                                           index medoid count))
      (minimize distance into mini)
      (when (= mini distance)
        (setf result i))
      (finally (return result)))))


(defun choose-initial-medoids (state)
  (iterate
    (with indexes = (read-indexes state))
    (with cluster-contents = (access-cluster-contents state))
    (with generator = (clusters.utils:lazy-shuffle 0 (length indexes)))
    (for cluster in-vector cluster-contents)
    (for new-value = (funcall generator))
    (assert new-value)
    (setf (aref cluster 0) (aref indexes new-value)))
  (order-medoids state))


(defmethod assign-data-points-to-medoids (state)
  (map nil (curry #'(setf fill-pointer) 1)
       (access-cluster-contents state))
  (iterate
    (with cluster-contents = (access-cluster-contents state))
    (with assignments = (clusters.utils:pmap
                         (clusters:parallelp state)
                         '(vector (or null fixnum))
                         (curry #'closest-medoid state)
                         (read-indexes state)))
    (for i in-vector (read-indexes state))
    (for assignment in-vector assignments)
    (for medoid-p = (null assignment))
    (unless medoid-p
      (vector-push-extend i (aref cluster-contents assignment)))))


(defun build-clusters (state &optional split-merge)
  (bind ((optimal-content nil)
         (clusters-with-optimal-size nil)
         (select-medoids-attempts-count
          (read-select-medoids-attempts-count state))
         (merge-threshold (read-merge-threshold state))
         (split-threshold (read-split-threshold state))
         (split-merge-attempts-count
          (read-split-merge-attempts-count state))
         (cluster-contents (access-cluster-contents state))
         ((:flet split-merge ())
          (when (and split-merge
                     (not (zerop split-merge))
                     split-merge-attempts-count)
            (iterate
              (scan-for-clusters-of-invalid-size state)
              (while (unfinished-clusters-p state))
              (repeat (read-split-merge-attempts-count state))
              (recluster-clusters-of-invalid-size state)
              (for right-size =
                   (/ (count-if (lambda (x) (< merge-threshold x split-threshold))
                                cluster-contents)
                      (length cluster-contents)))
              (ensure clusters-with-optimal-size right-size)
              (minf clusters-with-optimal-size right-size)
              (when (= right-size clusters-with-optimal-size)
                (setf optimal-content (map-into (copy-array cluster-contents)
                                                #'copy-array
                                                cluster-contents)))
              (finally (setf (access-cluster-contents state)
                             optimal-content))))))
    (iterate
      (with attempts = select-medoids-attempts-count)
      (for i from 0)
      (unless (null attempts)
        (unless (< i attempts)
          (leave t)))
      (when (zerop (rem i 3))
        (clear-cluster-contents state)
        (choose-initial-medoids state)
        (assign-data-points-to-medoids state))
      ; TODO
      (clear-unfinished-clusters state)
      (choose-effective-medoids state)
      (always (unfinished-clusters-p state))
      (finally
       (split-merge)
       (clear-unfinished-clusters state)))))
