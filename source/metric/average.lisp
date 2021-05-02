(cl:in-package #:clusters.metric)


(defun average (fn a b &key (test #'eql) (key #'identity))
  (check-type a list)
  (check-type b list)
  (ensure-functionf fn test key)
  (let ((a-count (length a))
        (b-count (length b)))
    (when (and (zerop a-count) (zerop b-count))
      (return-from average 0))
    (when (or (zerop a-count) (zerop b-count))
      (return-from average most-positive-fixnum)))
  (bind ((union (union a b :key key :test test))
         (union-count (length union))
         ((:flet side (set other-set))
          (let ((sum 0)
                (val (/ 1 (* union-count (length set))))
                (set-difference (set-difference other-set set
                                                :key key
                                                :test test)))
            (iterate
              (for elt1 in set)
              (iterate
                (for elt2 in set-difference)
                (incf sum (funcall fn elt2 elt1)))
              (finally (return (/ sum val))))))
         (left (side a b))
         (right (side b a)))
    (+ left right)))
