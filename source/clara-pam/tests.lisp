(cl:in-package :cl-user)
(defpackage :clara/pam-test-suite (:use :cl :prove))
(cl:in-package :clara/pam-test-suite)


(defun metric (a b)
  (coerce (abs (- a b))
          'single-float))

(plan 4)

(let* ((data (concatenate 'vector
                          (map-into (make-array 100)
                                    (cl-ds.utils:lazy-shuffle 0 100))
                          (map-into (make-array 100)
                                    (cl-ds.utils:lazy-shuffle 500 800))
                          (map-into (make-array 100)
                                    (cl-ds.utils:lazy-shuffle 200 300))))
       (distance-matrix (cl-ds.utils:make-distance-matrix-from-vector
                         'single-float #'metric data))
       (clusters (cl-ds.utils.cluster.clara/pam:partition-around-medoids
                  data distance-matrix 10 :attempts 5 :split 105 :merge 50))
       (clara-clusters (cl-ds.utils.cluster.clara/pam:clara data 5
                                                  #'metric
                                                  150 25
                                                  :attempts 5
                                                  :split 75
                                                  :merge 50))
       (cluster-contents (cl-ds.utils.cluster:cluster-contents clusters))
       (clara-cluster-contents (cl-ds.utils.cluster:cluster-contents
                                clara-clusters))
       (total (apply #'concatenate 'vector (coerce cluster-contents 'list)))
       (clara-total (apply #'concatenate 'vector
                           (coerce clara-cluster-contents 'list))))
  (is (sort total #'<)
      (sort data #'<)
      :test #'serapeum:vector=)
  (is (sort clara-total #'<)
      (sort data #'<)
      :test #'serapeum:vector=))

(let* ((data (concatenate 'vector
                          (map-into (make-array 100)
                                    (cl-ds.utils:lazy-shuffle 0 100))
                          (map-into (make-array 100)
                                    (cl-ds.utils:lazy-shuffle 500 800))
                          (map-into (make-array 100)
                                    (cl-ds.utils:lazy-shuffle 200 300))))
       (distance-matrix (cl-ds.utils:make-distance-matrix-from-vector
                         'single-float #'metric
                         data))
       (clusters (cl-ds.utils.cluster.clara/pam:partition-around-medoids
                  data distance-matrix 10 :attempts 5 :split 105 :merge 50
                                          :minimal-cluster-size 150))
       (clara-clusters (cl-ds.utils.cluster.clara/pam:clara data 5
                                                  #'metric
                                                  150 25
                                                  :attempts 5
                                                  :split 75
                                                  :minimal-cluster-size 150
                                                  :merge 50)))
  (ok (every (lambda (x) (>= (length x) 150))
             (cl-ds.utils.cluster:cluster-contents clara-clusters)))
  (ok (every (lambda (x) (>= (length x) 150))
             (cl-ds.utils.cluster:cluster-contents clusters))))

(finalize)
