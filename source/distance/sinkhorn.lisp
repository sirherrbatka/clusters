(cl:in-package #:cl-data-structures.utils.distance)


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
           (type double-float regularization-strength epsilon)
           (optimize (speed 3) (space 0) (debug 0) (compilation-speed 0)))
  (bind ((n (length vector-1))
         (m (length vector-2))
         (transport-matrix (make-array `(,n ,m)
                                       :element-type 'double-float))
         (u (make-array n :element-type 'double-float
                          :initial-element 0.0d0))
         (sum-column (make-array m :element-type 'double-float
                                   :initial-element 0.0d0))
         (sum-row (make-array n :element-type 'double-float
                                :initial-element 0.0d0)))
    (declare (type fixnum n m)
             (type (simple-array double-float (*)) u sum-column sum-row)
             (type (simple-array double-float (* *))
                   transport-matrix))
    (assert (or (not (arrayp cost)) (eql n (array-dimension cost 0))))
    (assert (or (not (arrayp cost)) (eql m (array-dimension cost 1))))
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
    (iterate outer
      (declare (type double-float maximum))
      (iterate
        (declare (type fixnum i)
                 (type double-float sum))
        (for i from 0 below n)
        (for sum = 0.0d0)
        (iterate
          (declare (type fixnum k))
          (for k from 0 below m)
          (incf sum (aref transport-matrix i k)))
        (setf (aref sum-row i) sum))
      (for maximum = 0.0d0)
      (iterate
        (declare (type double-float a b value)
                 (type fixnum i))
        (for i from 0 below n)
        (for a = (aref u i))
        (for b = (aref sum-row i))
        (for value = (abs (- a b)))
        (setf maximum (max value maximum))
        (when (> maximum epsilon)
          (leave))
        (finally (in outer (finish))))
      (rotatef sum-row u)
      (iterate
        (declare (type fixnum i)
                 (type double-float r scaler))
        (for i from 0 below n)
        (for r = (aref vector-1 i))
        (for scaler = (/ r (aref u i)))
        (iterate
          (declare (type fixnum j))
          (for j from 0 below m)
          (setf #3=(aref transport-matrix i j)
                (* #3# scaler))))
      (iterate
        (declare (type fixnum i)
                 (type double-float sum))
        (for i from 0 below m)
        (for sum = 0.0d0)
        (iterate
          (declare (type fixnum k))
          (for k from 0 below n)
          (incf sum (aref transport-matrix k i)))
        (setf (aref sum-column i) sum))
      (iterate
        (declare (type fixnum i)
                 (type double-float scaler c))
        (for i from 0 below m)
        (for c = (aref vector-2 i))
        (for scaler = (/ c (aref sum-column i)))
        (iterate
          (declare (type fixnum j))
          (for j from 0 below n)
          (setf #2=(aref transport-matrix j i)
                (* #2# scaler)))))
    transport-matrix))


(-> sinkhorn-distance
    ((or double-float (simple-array double-float (* *)))
     (simple-array double-float (*))
     (simple-array double-float (*))
     double-float
     &optional double-float)
    double-float)
(defun sinkhorn-distance (cost first-vector second-vector
                          regularization-strength
                          &optional (epsilon (coerce single-float-epsilon
                                                     'double-float)))
  (declare (optimize (speed 3)))
  (cl-ds.utils:cases ((floatp cost))
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
      (finally (return total)))))
