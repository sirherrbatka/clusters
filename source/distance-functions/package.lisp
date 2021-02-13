(defpackage cl-data-structures.utils.distance
  (:use #:cl #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.utils.distance)
  (:export
   #:bhattacharyya-distance
   #:sinkhorn-distance
   #:sinkhorn-optimal-transport-matrix))
