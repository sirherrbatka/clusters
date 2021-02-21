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
    (with indexes = (access-indexes state))
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
                         (access-indexes state)))
    (for i in-vector (access-indexes state))
    (for assignment in-vector assignments)
    (for medoid-p = (null assignment))
    (unless medoid-p
      (vector-push-extend i (aref cluster-contents assignment)))))


(defun clear-unfinished-clusters (state)
  (let ((cluster-contents (access-cluster-contents state)))
    (setf #1=(access-unfinished-clusters state)
          (adjust-array #1#
                        (length cluster-contents)
                        :fill-pointer (length cluster-contents)))
    (map-into (access-unfinished-clusters state) (constantly nil))))


(-> choose-effective-medoid (pam-algorithm-state (vector t)) boolean)
(defun choose-effective-medoid (state cluster)
  (bind (((:flet swap-medoid (i))
          (declare (type non-negative-fixnum i))
          (rotatef (aref cluster i) (aref cluster 0)))
         (distance-matrix (access-distance-matrix state))
         (matrix-count (~> distance-matrix
                           length
                           clusters.utils:half-matrix-size->count))
         ((:flet total-distance-to-medoid (&optional old-cost))
          (iterate
            (for i from 1 below (length cluster))
            (for distance = (clusters.utils:mref distance-matrix
                                                 (the fixnum (aref cluster 0))
                                                 (the fixnum (aref cluster i))
                                                 matrix-count))
            (assert distance)
            (sum distance into sum)
            (unless (null old-cost)
              (while (<= sum old-cost)))
            (finally (return sum))))
         (improved-something nil))
    (iterate
      (with minimal-distance-to-medoid = (total-distance-to-medoid))
      (for i from 1 below (length cluster))
      (swap-medoid i)
      (for distance = (total-distance-to-medoid
                       minimal-distance-to-medoid))
      (minf minimal-distance-to-medoid distance)
      (for improved = (= distance minimal-distance-to-medoid))
      (unless improved
        (swap-medoid i))
      (setf improved-something
            (not (null (or improved improved-something)))))
    improved-something))


(defun choose-effective-medoids (state)
  (let ((unfinished-clusters (access-unfinished-clusters state))
        (cluster-contents (access-cluster-contents state)))
    (assert (eql (length unfinished-clusters) (length cluster-contents)))
    (clusters.utils:pmap-into unfinished-clusters
                              (curry #'choose-effective-medoid state)
                              cluster-contents)
    (order-medoids state)))


(defun unfinished-clusters-p (state)
  (find t (access-unfinished-clusters state)))


(defun scan-for-clusters-of-invalid-size (state)
  (clear-unfinished-clusters state)
  (let ((merge-threshold (read-merge-threshold state))
        (split-threshold (read-split-threshold state)))
    (map-into (access-unfinished-clusters state)
              (lambda (x)
                (not (< merge-threshold
                        (length x)
                        split-threshold)))
              (access-cluster-contents state))))


(defun fill-reclustering-index-vector (state indexes count-of-eliminated)
  (iterate
    (with cluster-contents = (access-cluster-contents state))
    (with position = 0)
    (for i from (~> cluster-contents length 1-) downto 0)
    (repeat count-of-eliminated)
    (for cluster = (aref cluster-contents i))
    (iterate
      (for value in-vector cluster)
      (setf (aref indexes position) value)
      (incf position)))
  indexes)


(defun prepare-reclustering-index-vector (state)
  (bind ((cluster-contents (access-cluster-contents state))
         (merge-threshold (read-merge-threshold state))
         (split-threshold (read-split-threshold state))
         (count-of-eliminated (clusters.utils:swap-if
                               cluster-contents
                               (lambda (x)
                                 (not (< merge-threshold
                                         x
                                         split-threshold)))
                               :key #'length))
         (count-of-elements (iterate
                              (for i
                                   from (~> cluster-contents length 1-)
                                   downto 0)
                              (repeat count-of-eliminated)
                              (sum (~> cluster-contents
                                       (aref i)
                                       length))))
         ((:dflet expected-cluster-count ())
          (round (/ count-of-elements
                    (/ (+ split-threshold merge-threshold)
                       2)))))
    (iterate
      (while (zerop (expected-cluster-count)))
      (until (eql count-of-eliminated (length cluster-contents)))
      (incf count-of-eliminated)
      (incf count-of-elements (~>> (length cluster-contents)
                                   (- _ count-of-eliminated)
                                   (aref cluster-contents)
                                   length)))
    (values
     (fill-reclustering-index-vector
      state
      (make-array count-of-elements :element-type 'non-negative-fixnum)
      count-of-eliminated)
     count-of-eliminated
     (expected-cluster-count))))


(defun recluster-clusters-of-invalid-size (state)
  (declare (optimize (speed 1) (safety 3)))
  (setf #1=(access-cluster-contents state) (shuffle #1#))
  (bind (((:values indexes count-of-eliminated expected-cluster-count)
          (prepare-reclustering-index-vector state))
         (cluster-contents (access-cluster-contents state))
         (fresh-state (make
                       'algorithm-state
                       :parameters (clusters:parameters state)
                       :indexes indexes
                       :medoids-count expected-cluster-count
                       :distance-matrix (access-distance-matrix state)
                       :data (clusters:data state))))
    (build-clusters fresh-state nil)
    (decf (fill-pointer cluster-contents) count-of-eliminated)
    (map nil
         (rcurry #'vector-push-extend cluster-contents)
         (access-cluster-contents fresh-state))
    (order-medoids state)))


(defun build-clusters (state &optional split-merge)
  (bind ((optimal-content nil)
         (clusters-with-optimal-size nil)
         (select-medoids-attempts-count
          (read-select-medoids-attempts-count state))
         (merge-threshold (read-merge-threshold state))
         (split-threshold (read-split-threshold state))
         (split-merge-attempts-count
          (read-split-merge-attempts-count state))
         ((:flet split-merge
            (&aux (cluster-contents (access-cluster-contents state))))
          (when (and split-merge
                     (not (zerop split-merge))
                     split-merge-attempts-count)
            (iterate
              (scan-for-clusters-of-invalid-size state)
              (while (unfinished-clusters-p state))
              (repeat (read-split-merge-attempts-count state))
              (recluster-clusters-of-invalid-size state)
              (for right-size =
                   (/ (count-if (lambda (x)
                                  (< merge-threshold x split-threshold))
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
      (unless (or (null attempts) (< i attempts))
        (leave t))
      (when (zerop (rem i 3))
        (clear-cluster-contents state)
        (choose-initial-medoids state)
        (assign-data-points-to-medoids state))
      (clear-unfinished-clusters state)
      (choose-effective-medoids state)
      (while (unfinished-clusters-p state))
      ;; TODO
      (finally
       (split-merge)
       (clear-unfinished-clusters state)))))

(defun reset-state (object)
  (declare (optimize (safety 3) (debug 3)))
  (bind (((:accessors (split-merge-attempts-count read-split-merge-attempts-count)
                      (merge-threshold read-merge-threshold)
                      (split-threshold read-split-threshold)
                      (indexes access-indexes)
                      (medoids-count access-medoids-count)
                      (cluster-size access-cluster-size))))
    (if (zerop %split-merge-attempts-count)
        (progn (assert (null %merge-threshold))
               (assert (null %split-threshold)))
        (assert (< 0 %merge-threshold %split-threshold)))
    (macrolet ((slot-initialized-p (slot)
                 `(and (slot-boundp object ',slot)
                       (not (null ,slot)))))
      (unless (slot-initialized-p %indexes)
        (setf %indexes (coerce (~> %input-data length iota)
                               '(vector non-negative-fixnum))))
      (let ((length (length %indexes)))
        (setf %number-of-medoids
              (if (slot-initialized-p %number-of-medoids)
                  (max (min %number-of-medoids length) 1)
                  length))
        (if (slot-initialized-p %cluster-size)
            (assert (< 0 %cluster-size))
            (setf %cluster-size (max 2 (round-to (/ length %number-of-medoids)
                                                 2))))
        (unless (slot-initialized-p %cluster-contents)
          (setf %cluster-contents (make-array %number-of-medoids
                                              :adjustable t
                                              :fill-pointer %number-of-medoids))
          (map-into %cluster-contents
                    (lambda () (make-array %cluster-size :adjustable t
                                                    :fill-pointer 1))))
        (unless (slot-initialized-p %unfinished-clusters)
          (setf %unfinished-clusters
                (make-array %number-of-medoids
                            :element-type 'boolean
                            :adjustable t
                            :fill-pointer %number-of-medoids
                            :initial-element nil)))))))
