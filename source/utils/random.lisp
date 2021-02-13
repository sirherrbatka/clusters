(cl:in-package #:clusters.utils)


(-> draw-random-vector (vector positive-fixnum &optional vector) vector)
(defun draw-random-vector (input size
                           &optional (result (make-array size
                                                         :element-type (array-element-type input))))
  (when (array-has-fill-pointer-p result)
    (setf (fill-pointer result) size))
  (let ((length (length input)))
    (map-into result (lambda () (aref input (random length))))))
