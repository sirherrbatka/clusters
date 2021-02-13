(cl:in-package #:cl-user)
(defpackage :bubble-test-suite (:use
                                :cl :alexandria
                                :cl-data-structures.utils.clustering.bubble))
(cl:in-package :bubble-test-suite)


(defun metric (a b)
  (abs (- a b)))

(prove:plan 2)

(let* ((data (coerce (shuffle (iota 1000)) 'vector))
       (bubbles (bubble-grouping
                 data
                 #'metric
                 5
                 10
                 10
                 10
                 10)))
  (prove:ok (every (lambda (bubble)
                     (let* ((clusteroid (bubble-clusteroid bubble))
                            (content (bubble-content bubble))
                            (distance-sum (reduce #'+
                                                  content
                                                  :key (lambda (x)
                                                         (metric x clusteroid))))
                            (radius (sqrt (/ distance-sum (length content)))))
                       (<= radius 10)))
                   bubbles))
  (prove:is (length data) (reduce #'+ bubbles
                                  :key (compose #'length #'bubble-content))))

(prove:finalize)
