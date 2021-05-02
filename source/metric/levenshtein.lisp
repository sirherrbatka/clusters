(cl:in-package #:clusters.metric)


(defun levenshtein (str1 str2)
  (check-type str1 string)
  (check-type str2 string)
  (let ((n (length str1))
        (m (length str2)))
    (cond ((= 0 n) (return-from levenshtein m))
          ((= 0 m) (return-from levenshtein n)))
    (let ((col (make-array (1+ m) :element-type 'fixnum))
          (prev-col (make-array (1+ m) :element-type 'fixnum)))
      (iterate
        (for i from 0 below (1+ m))
        (setf (aref prev-col i) i))
      (iterate
        (for i below n)
        (setf (aref col 0) (1+ i))
        (iterate
          (for j below m)
          (setf (aref col (1+ j))
                (min (1+ (aref col j))
                     (1+ (aref prev-col (1+ j)))
                     (+ (aref prev-col j)
                        (if (char-equal (aref str1 i) (aref str2 j)) 0 1)))))
        (rotatef col prev-col))
      (aref prev-col m))))
