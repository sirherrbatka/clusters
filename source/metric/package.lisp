(cl:defpackage #:clusters.metric
  (:use #:cl #:clusters.aux-package)
  (:export
   #:average
   #:earth-mover
   #:euclid
   #:hausdorff
   #:hellinger
   #:lcs
   #:levenshtein
   #:svr))
