(cl:in-package #:cl-data-structures.utils.metric)


(defun same-events (grid sequence-a sequence-b to-sum)
  (iterate
    (with collection = nil)
    (for i from 0)
    (for a in-vector sequence-a)
    (iterate
      (for j from 0)
      (for b in-vector sequence-b)
      (for same = (equal a b))
      (if same
          (progn
            (setf (aref grid i j) 1)
            (push (list i j) collection))
          (setf (aref grid i j) 0)))
    (finally
     (let* ((size (length collection))
            (collection (nreverse collection))
            (result (or to-sum
                        (make-array (list size 2)
                                    :element-type 'non-negative-fixnum))))
       (iterate
         (for (row column) in collection)
         (for i from 0)
         (setf (aref result i 0) row
               (aref result i 1) column)
         (finally
          (return-from same-events
            (values result size))))))))


(-> array-sum (non-negative-fixnum
               non-negative-fixnum
               (array non-negative-fixnum (* 2))
               (array non-negative-integer (* *)))
    non-negative-integer)
(defun array-sum (iteration sum-size to-sum grid)
  (declare (optimize (speed 3) (safety 0)))
  (let ((iteration (1- iteration)))
    (iterate
      (for i from 0 below sum-size)
      (for column = (aref to-sum i 0))
      (for row = (aref to-sum i 1))
      (unless (or (> 0 (- row iteration))
                  (> 0 (- column iteration)))
        (sum (aref grid (aref to-sum i 0) (aref to-sum i 1)))))))


(-> update-grid (non-negative-fixnum
                 non-negative-fixnum
                 non-negative-fixnum
                 (simple-array non-negative-fixnum (* 2))
                 (simple-array non-negative-integer (* *))
                 (simple-array non-negative-integer (* *))
                 non-negative-fixnum)
    array)
(defun update-grid (a-length b-length sum-size to-sum grid prev-grid iteration)
  (declare (optimize (safety 0) (speed 3)))
  (flet ((backwards-row-sum ()
           (iterate
             (for row from 0 to (- b-length iteration))
             (iterate
               (for column from (- a-length iteration) downto 0)
               (with sum = 0)
               (symbol-macrolet ((cell (aref prev-grid column row)))
                 (psetf cell sum
                        sum (+ sum cell))))))
         (upwards-column-sum ()
           (iterate
             (for column from 0 to (- a-length iteration))
             (iterate
               (for row from (- b-length iteration) downto 0)
               (with sum = 0)
               (symbol-macrolet ((cell (aref prev-grid column row)))
                 (psetf cell sum
                        sum (+ sum cell)))))))
    (declare (dynamic-extent #'backwards-row-sum #'upwards-column-sum))
    (upwards-column-sum)
    (backwards-row-sum)
    (iterate
      (for column from 0 to (- a-length iteration))
      (iterate
        (for row from 0 to (- b-length iteration))
        (setf (aref grid column row) 0)))
    (let ((iteration (1- iteration)))
      (iterate
        (for i from 0 below sum-size)
        (for column = (aref to-sum i 0))
        (for row = (aref to-sum i 1))
        (until (or (>= row iteration)
                   (>= column iteration)))
        (setf (aref grid row column) (aref prev-grid row column))))
    grid))


(-> count-of-all-possible-subvectors-upto-length (fixnum fixnum &optional (or null (vector fixnum))) fixnum)
(defun count-of-all-possible-subvectors-upto-length (length-of-vector length-of-subvector &optional buffer)
  (cond ((zerop length-of-subvector) 1)
        ((> length-of-subvector length-of-vector) 0)
        ((eql 1 length-of-subvector) length-of-vector)
        (t (1+ (iterate
                 (with vector = (if (or (null buffer)
                                        (< length-of-vector (length buffer)))
                                    (make-array length-of-vector :element-type 'fixnum :initial-element 1)
                                    buffer))
                 (for limit from length-of-vector downto 0)
                 (sum (reduce #'+ vector :end limit))
                 (iterate
                   (for i from (1- limit) downto 0)
                   (with ac = 0)
                   (psetf (aref vector i) ac
                          ac (+ ac (aref vector i))))
                 (repeat length-of-subvector))))))


(defmacro with-grid-mapping-resources (max-length
                                       (&key
                                          (g1 (gensym))
                                          (g2 (gensym))
                                          (g3 (gensym))
                                          (to-sum (gensym)))
                                       &body body)
  (once-only (max-length)
    `(let ((,g1 (make-array (list ,max-length, max-length)
                            :element-type 'non-negative-integer
                            :initial-element 0))
           (,g2 (make-array (list ,max-length ,max-length)
                            :element-type 'non-negative-integer
                            :initial-element 0))
           (,g3 (make-array (list ,max-length ,max-length)
                            :element-type 'non-negative-integer
                            :initial-element 0))
           (,to-sum (make-array (list (expt ,max-length 2) 2)
                                :element-type 'non-negative-fixnum)))
       (flet ((grid-mapping (first-sequence second-sequence)
                (grid-mapping first-sequence second-sequence
                              ,g1 ,g2 ,g3 ,to-sum)))
         (declare (ignorable (function grid-mapping)))
         ,@body))))


(-> grid-mapping (vector vector
                  &optional (or null (array non-negative-integer))
                         (or null (array non-negative-integer))
                         (or null (array non-negative-integer))
                         (or null (array non-negative-fixnum (* 2))))
    single-float)
(defun grid-mapping (first-sequence second-sequence
                     &optional grid first-grid prev-grid to-sum)
  (unless (null grid)
    (assert (and grid prev-grid first-grid))
    (assert (equal (length (array-dimensions grid)) 2))
    (assert (equal (array-dimensions grid) (array-dimensions prev-grid)))
    (assert (equal (array-dimensions grid) (array-dimensions first-grid))))
  (let ((first-length (length first-sequence))
        (second-length (length second-sequence)))
    (when (xor (zerop first-length) (zerop second-length))
      (return-from grid-mapping 1.0))
    (when (and (zerop first-length) (zerop second-length))
      (return-from grid-mapping 0.0))
    (bind ((dims (unless (null grid) (array-dimensions grid)))
           (longer (max first-length second-length))
           (dims-match (and dims (every (rcurry #'>= longer) dims)))
           (actual-dims (if dims-match
                            dims
                            (list longer longer)))
           (grid (if dims-match
                     grid
                     (make-array actual-dims
                                 :element-type 'non-negative-integer
                                 :initial-element 0)))
           (prev-grid (if dims-match
                          prev-grid
                          (make-array actual-dims
                                      :element-type 'non-negative-integer
                                      :initial-element 0)))
           (first-grid (if dims-match
                           first-grid
                           (make-array actual-dims
                                       :element-type 'non-negative-integer
                                       :initial-element 0)))
           ((:flet run-grid-algorithm (first-sequence second-sequence length))
            (bind ((first-length (length first-sequence))
                   (second-length (length second-sequence))
                   ((:values to-sum sum-size) (same-events grid
                                                           first-sequence
                                                           second-sequence
                                                           to-sum)))
              (iterate
                (for i below first-length)
                (iterate
                  (for j below second-length)
                  (setf
                   (aref first-grid i j) (aref grid i j)
                   (aref prev-grid i j) 0)))
              (iterate
                (repeat length)
                (for i from 1)
                (for counts = (array-sum i sum-size to-sum grid))
                (until (zerop counts))
                (rotatef grid prev-grid)
                (update-grid first-length second-length sum-size to-sum grid prev-grid i)
                (sum counts)))))
      (coerce
       (run-grid-algorithm first-sequence
                           second-sequence
                           (min (length second-sequence)
                                (length first-sequence)))
       'single-float))))


(defun svr-metric (a b)
  (declare (type vector a b))
  (let ((larger (if (> (length a) (length b))
                    a
                    b)))
    (with-grid-mapping-resources (length larger) ()
        (- 1.0 (/ (grid-mapping a b)
                  (grid-mapping larger larger))))))
