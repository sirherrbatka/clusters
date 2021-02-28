(cl:in-package #:clusters.metric)


(defun euclid (a b)
  (check-type a (simple-array single-float (*)))
  (check-type b (simple-array single-float (*)))
  (let ((length-a (length a))
        (length-b (length b)))
    (assert (eql length-a length-b))
    (when (zerop (length a))
      (return-from euclid 0.0))
    (iterate
      (declare (type fixnum i)
               (type non-negative-single-float sum)
               (type single-float ea eb))
      (with sum = 0.0)
      (for i from 0 below length-a)
      (for ea = (aref a i))
      (for eb = (aref b i))
      (incf sum (expt (- ea eb) 2))
      (finally
       (let ((result (sqrt sum)))
         (declare (type non-negative-single-float sum))
         (return result))))))
