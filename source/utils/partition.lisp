(cl:in-package #:clusters.utils)


(defun seed (data indexes medoids y distance-function)
  (bind ((n (length indexes))
         (k (length medoids))
         (d (make-array n
                        :element-type 'double-float
                        :initial-element most-positive-double-float)))
    (setf (aref medoids 0) (aref indexes (random n)))
    (iterate
      (for j from 1 to k)
      (for prev from 0)
      (for medoid  = (aref data (aref medoids prev)))
      (iterate
        (for index in-vector indexes)
        (for datum = (aref data index))
        (for distance = (funcall distance-function
                                 datum
                                 medoid))
        (when (< distance (aref d j))
          (setf (aref d j) distance
                (aref y j) prev)))
      (when (< j k)
        (iterate
          (with cost = 0.0d0)
          (with cutoff = (* (random 1.0d0) (reduce #'+ d)))
          (for index from 0 to n)
          (incf cost (aref d index))
          (when (>= cost cutoff)
            (setf (aref medoids j) (aref indexes index))
            (finish))))
      (finally (return d)))))
