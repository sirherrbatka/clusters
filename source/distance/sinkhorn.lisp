(cl:in-package #:clusters.distance)


(defun max-condition (u sum-row n epsilon)
  (iterate
    (for i from 0 below n)
    (maximize (abs (- (aref u i) (aref sum-row i)))
              into maximum)
    (thereis (> maximum epsilon))))


(defun sum-row (u transport-matrix n m)
  (iterate
    (for i from 0 below n)
    (setf (aref u i) 0.0d0)
    (iterate
      (for j from 0 below m)
      (incf (aref u i) (aref transport-matrix i j)))
    (finally (return u))))


(-> sinkhorn-optimal-transport-matrix
    ((or double-float (simple-array double-float (* *)))
     (simple-array double-float (*))
     (simple-array double-float (*))
     double-float
     double-float)
    (simple-array double-float (* *)))
(defun sinkhorn-optimal-transport-matrix (cost vector-1 vector-2
                                          regularization-strength epsilon)
  (declare (type (or double-float (simple-array double-float (* *))) cost)
           (type (simple-array double-float (*)) vector-1 vector-2)
           (type double-float regularization-strength epsilon))
  (bind ((n (length vector-1))
         (m (length vector-2))
         (transport-matrix (make-array `(,n ,m)
                                       :initial-element 0.0d0
                                       :element-type 'double-float))
         (u (make-array n :element-type 'double-float
                          :initial-element 0.0d0))
         (sum-row (make-array n :element-type 'double-float
                                :initial-element 0.0d0)))
    (declare (type fixnum n m)
             (type (simple-array double-float (*)) u sum-row)
             (type (simple-array double-float (* *)) transport-matrix))
    (assert (or (not (arrayp cost)) (eql n (array-dimension cost 0))))
    (assert (or (not (arrayp cost)) (eql m (array-dimension cost 1))))
    ;; initialize cost matrix
    (cl-ds.utils:cases ((floatp cost))
      (iterate
        (declare (type double-float sum m value)
                 (type fixnum i total-size))
        (with total-size = (array-total-size transport-matrix))
        (for i from 0 below total-size)
        (for m = (if (floatp cost)
                     cost
                     (row-major-aref cost i)))
        (for value = (exp (- (* regularization-strength m))))
        (setf #1=(row-major-aref transport-matrix i) value)
        (sum value into sum)
        (finally
         (iterate
           (declare (type fixnum i))
           (for i from 0 below total-size)
           (setf #1# (/ #1# sum))))))
    ;; normalize matrix
    (iterate
      (while (max-condition u
                            (sum-row sum-row transport-matrix n m)
                            n
                            epsilon))
      (rotatef u sum-row)
      (iterate
        (for i from 0 below n)
        (iterate
          (for j from 0 below m)
          (setf (aref transport-matrix i j)
                (* (aref transport-matrix i j)
                   (/ (aref vector-1 i)
                      (aref u i))))))
      (iterate
        (for i from 0 below m)
        (for sum = (iterate
                     (for j from 0 below n)
                     (sum (aref transport-matrix j i))))
        (iterate
          (for j from 0 below n)
          (setf (aref transport-matrix j i)
                (* (aref transport-matrix j i)
                   (/ (aref vector-2 i)
                      sum))))))
    transport-matrix))


(-> sinkhorn
    ((or double-float (simple-array double-float (* *)))
     (simple-array double-float (*))
     (simple-array double-float (*))
     double-float
     &optional double-float)
    double-float)
(defun sinkhorn (cost first-vector second-vector
                 regularization-strength
                 &optional (epsilon (coerce single-float-epsilon
                                            'double-float)))
  (iterate
    (declare (type fixnum i)
             (type double-float total))
    (with total = 0.0d0)
    (with transport = (sinkhorn-optimal-transport-matrix
                       cost first-vector
                       second-vector regularization-strength
                       epsilon))
    (for i from 0 below (array-total-size transport))
    (incf total (* (row-major-aref transport i)
                   (if (floatp cost)
                       cost
                       (row-major-aref cost i))))
    (finally (return total))))
