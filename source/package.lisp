(defpackage cl-data-structures.utils.clustering
  (:use #:cl #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.utils.cluster)
  (:export
   #:calculate-silhouette
   #:cluster-contents
   #:clustering-result
   #:silhouette))
