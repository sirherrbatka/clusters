(cl:in-package #:clusters.metric)


(defun hausdorff (fn a b
                  &key
                    (key #'identity)
                    (element-type t)
                    (distance-matrix
                     (make-array (list (length a) (length b))
                                 :element-type element-type)))
  (declare (type vector a) (type vector b))
  (ensure-functionf fn)
  (when (and (emptyp a) (emptyp b))
    (return-from hausdorff 0))
  (iterate
    (for ea in-vector a)
    (for ia from 0)
    (iterate
      (for eb in-vector b)
      (for ib from 0)
      (setf (aref distance-matrix ia ib) (funcall fn
                                                  (funcall key ea)
                                                  (funcall key eb)))))
  (max (iterate
         (for i from 0 below (length a))
         (maximize (iterate
                     (for j from 0 below (length b))
                     (minimize (aref distance-matrix i j)))))
       (iterate
         (for i from 0 below (length b))
         (maximize (iterate
                     (for j from 0 below (length a))
                     (minimize (aref distance-matrix j i)))))))
