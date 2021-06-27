(cl:defpackage #:clusters.metric
  (:use #:cl #:clusters.aux-package)
  (:export
   #:average
   #:earth-mover
   #:group-average
   #:euclid
   #:hausdorff
   #:hellinger
   #:lcs
   #:levenshtein
   #:svr))
