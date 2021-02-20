(cl:in-package #:clusters.utils)


(defun swap-if (vector test &key
                              (key #'identity)
                              (start 0)
                              (end (length vector)))
  (declare (type fixnum start end)
           (type vector vector))
  (ensure-functionf test key)
  (iterate
    (declare (type fixnum i result))
    (with result = 0)
    (for i from (1- end) downto start)
    (for removal = (funcall test (funcall key (aref vector i))))
    (when removal
      (incf result)
      (rotatef (aref vector i)
               (aref vector (decf end))))
    (finally (return result))))
