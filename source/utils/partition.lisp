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
        (for i from 0 below n)
        (for index = (aref indexes i))
        (for datum = (aref data index))
        (for distance = (funcall distance-function
                                 datum
                                 medoid))
        (when (< distance (aref d j))
          (setf (aref d i) (coerce distance 'double-float)
                (aref y i) prev)))
      (handler-case
          (when (< j k)
            (iterate
              (with cutoff = (* (random 1.0d0) (reduce #'+ d)))
              (with cost = 0.0d0)
              (for index from 0 to n)
              (incf cost (aref d index))
              (when (>= cost cutoff)
                (setf (aref medoids j) (aref indexes index))
                (finish))))
        (floating-point-overflow (e) (declare (ignore e)) nil))
      (finally (return d)))))
