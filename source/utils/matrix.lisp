(cl:in-package #:clusters.utils)


(defun row-index (k n)
  (~> (* -8 k)
      (+ (* 4 n n))
      (- (* 4 n))
      (- 7)
      sqrt
      (-)
      (+ (* 2 n))
      (/ 2)
      (- 1)
      ceiling))


(defun element-in-i-rows (i n)
  (~> (1+ i)
      (* i)
      (truncate 2)
      (+ (* i (- n 1 i)))))


(defun column-index (k i n)
  (~>> (element-in-i-rows (1+ i) n)
       (- n)
       (+ k)))


(declaim (inline half-matrix-index->square-row/column))
(defun half-matrix-index->square-row/column (count index)
  (let* ((i (row-index index count))
         (j (column-index index i count)))
    (cons i j)))


(declaim (inline square-row/column->half-matrix-index))
(defun square-row/column->half-matrix-index (count row column)
  (+ (- (* count row)
        (/ (* row (1+ row))
           2)
        1
        row)
     column))


(declaim (inline half-matrix-size->count))
(defun half-matrix-size->count (size)
  (~> size (* 2) sqrt 1+ floor))


(declaim (inline count->half-matrix-size))
(defun count->half-matrix-size (count)
  (1+ (square-row/column->half-matrix-index
       count (1- count) (1- count))))


(defun make-half-matrix (count
                         &rest all
                         &key element-type initial-element initial-contents)
  (declare (ignore element-type initial-element initial-contents))
  (apply #'make-array (count->half-matrix-size count) all))


(defun mref (half-matrix from to
             &optional (count (~> half-matrix length
                                  half-matrix-size->count)))
  (declare (type fixnum from to)
           (type (array * (*)) half-matrix))
  (cond
    ((or (>= from count) (>= to count) (eql to from))
     (error "No such position in the matrix."))
    (t (aref half-matrix (square-row/column->half-matrix-index count from to)))))


(defun (setf mref) (new-value half-matrix from to
                    &optional (count (~> half-matrix length
                                         half-matrix-size->count)))
  (declare (type fixnum from to)
           (type (array * (*)) half-matrix))
  (cond
    ((or (>= from count) (>= to count) (eql to from))
     (error "No such position in the matrix."))
    (t (setf (aref half-matrix
                   (square-row/column->half-matrix-index count from to))
             new-value))))


(defun map-into-half-matrix (parallel half-matrix function)
  (let ((count (~> half-matrix length half-matrix-size->count)))
    (if parallel
        (lparallel:pmap nil
                        (lambda (i)
                          (iterate
                            (for ii from 0 below i)
                            (setf (mref half-matrix i ii)
                                  (funcall function i ii))))
                        (iota count))
        (iterate
          (for i from 0 below count)
          (iterate
            (for ii from 0 below i)
            (setf (mref half-matrix i ii count)
                  (funcall function i ii)))))
    half-matrix))


(defun distance-matrix (parallel distance-function data)
  (map-into-half-matrix parallel
                        (make-half-matrix (length data))
                        (fork distance-function
                              (curry #'aref data)
                              (curry #'aref data))))
