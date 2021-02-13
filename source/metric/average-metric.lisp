(cl:in-package #:cl-data-structures.utils.metric)


(defun average-metric (fn test a b &key (key #'identity))
  (declare (type vector a) (type vector b)
           (optimize (speed 3)))
  (ensure-functionf fn)
  (when (and (emptyp a) (emptyp b))
    (return-from average-metric 0))
  (when (or (emptyp a) (emptyp b))
    (return-from average-metric most-positive-fixnum))
  (cl-ds.utils:with-keys ((eea ea key) (eeb eb key))
    (bind ((union (~> (concatenate 'vector a b)
                      (delete-duplicates :test test :key key)))
           (left
            (iterate
              (with val = (/ 1 (* (length union) (length a))))
              (for ea in-vector a)
              (when (null (position ea b :test test :key key))
                (next-iteration))
              (sum (iterate
                     (with ee = eea)
                     (for eb in-vector b)
                     (sum (funcall fn ee eeb)))
                   into result)
              (finally (return (* val result)))))
           (right
            (iterate
              (with val = (/ 1 (* (length union) (length b))))
              (for eb in-vector a)
              (when (null (position eb a :test test :key key))
                (next-iteration))
              (sum (iterate
                     (with ee = eeb)
                     (for ea in-vector a)
                     (sum (funcall fn eea ee)))
                   into result)
              (finally (return (* val result))))))
      (+ left right))))
