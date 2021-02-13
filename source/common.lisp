(cl:in-package #:cl-data-structures.utils.clustering)


(defclass clustering-result ()
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


(cl-ds.utils:define-list-of-slots clustering-result ()
  (%cluster-contents cluster-contents)
  (%distance-function distance-function)
  (%key-function key-function)
  (%silhouette-sample-count silhouette-sample-count)
  (%silhouette-sample-size silhouette-sample-size)
  (%silhouette silhouette))


(defun intra-cluster-distances (distance-matrix cluster)
  (map 'vector
       (lambda (c)
         (iterate
           (with length = (length cluster))
           (for k in-vector cluster)
           (when (eql c k)
             (next-iteration))
           (sum (cl-ds.utils:mref distance-matrix c k)
                into sum)
           (finally (return (if (eql 1 length)
                                nil
                                (coerce (/ sum (1- length))
                                        'single-float))))))
       cluster))


(defun build-distance-matrix (state whole)
  (cl-ds.utils:with-slots-for (state clustering-result)
    (cl-ds.utils:parallel-make-distance-matrix-from-vector
     'single-float
     %distance-function
     whole)))


(defun inter-cluster-distances (distance-matrix cluster sample)
  (map 'vector
       (lambda (k)
         (iterate
           (for other-cluster in-vector sample)
           (when (eq other-cluster cluster)
             (next-iteration))
           (minimize (average-distance-to-element distance-matrix
                                                  k
                                                  other-cluster))))
       cluster))


(defun select-random-cluster-subsets (state distance-matrix-supplied)
  (declare (optimize (debug 3)))
  (cl-ds.utils:with-slots-for (state clustering-result)
    (bind ((sample-size %silhouette-sample-size)
           (total-size (reduce #'+ %cluster-contents
                               :key #'length
                               :initial-value 0))
           (sample-ratio (min 1 (/ sample-size total-size)))
           (sample
            (map 'vector
                 (lambda (cluster)
                   (let* ((size (length cluster))
                          (sample-size (ceiling (* size sample-ratio)))
                          (result (make-array sample-size
                                              :element-type 'fixnum)))
                     (map-into result
                               (compose %key-function
                                        (curry #'aref cluster)
                                        (curry #'random size)))))
                 %cluster-contents))
           ((:values sizes total-size) (cl-ds.utils:scan #'+ sample
                                                         :initial-value 0
                                                         :key #'length))
           (whole-sample (make-array total-size)))
      (iterate
        (for cluster in-vector sample)
        (for offset in (cons 0 sizes))
        (iterate
          (for j from 0 below (length cluster))
          (for i from offset)
          (if distance-matrix-supplied
              (setf (aref whole-sample i) (aref cluster j))
              (shiftf (aref whole-sample i) (aref cluster j) i))))
      (list* sample whole-sample))))


(defun average-distance-to-element (distance-matrix element cluster)
  (iterate
    (for c in-vector cluster)
    (for distance = (cl-ds.utils:mref distance-matrix
                                      (the fixnum c)
                                      (the fixnum element)))
    (sum distance into sum)
    (finally (return (coerce (/ sum (length cluster))
                             'single-float)))))


(defun calculate-silhouette (clustering-result &optional distance-matrix)
  (declare (optimize (speed 1) (safety 1)))
  (cl-ds.utils:with-slots-for (clustering-result clustering-result)
    (bind (((:flet distance-difference (intra inter))
            (cond ((null intra) 0.0)
                  ((null inter) -1.0)
                  ((= intra inter) 0.0)
                  (t (coerce (/ (- inter intra) (max intra inter))
                             'single-float))))
           ((:flet silhouette (sample.whole))
            (iterate
              (with (sample . whole) = sample.whole)
              (with result = (make-array (length sample)
                                         :element-type 'single-float))
              (with distance-matrix = (or distance-matrix
                                          (build-distance-matrix clustering-result
                                                                 whole)))
              (for sub in-vector sample)
              (for i from 0)
              (for inter-distances = (inter-cluster-distances distance-matrix
                                                              sub
                                                              sample))
              (for intra-distances = (intra-cluster-distances distance-matrix
                                                              sub))
              (setf (aref result i)
                    (~> (map '(vector single-float) #'distance-difference
                             intra-distances inter-distances)
                        (reduce #'+ _)
                        (/ (length sub))
                        (coerce 'single-float)))
              (finally (return result)))))
      (~>> (map-into (make-array %silhouette-sample-count)
                     (curry #'select-random-cluster-subsets
                            clustering-result
                            (~> distance-matrix not null)))
           (lparallel:pmap 'list #'silhouette)
           (apply #'map '(vector single-float)
                  (compose (rcurry #'coerce 'single-float)
                           #'+))
           (cl-ds.utils:transform (compose (rcurry #'coerce 'single-float)
                                           (rcurry #'/ %silhouette-sample-count)))))))


(defmethod silhouette :before ((object clustering-result))
  (unless (slot-boundp object '%silhouette)
    (setf (slot-value object '%silhouette) (calculate-silhouette object))))
