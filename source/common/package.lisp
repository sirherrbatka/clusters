(cl:defpackage #:clusters
  (:use #:cl #:clusters.aux-package)
  (:export
   #:parallelp
   #:distance-function
   #:key-function
   #:silhouette-sample-size
   #:silhouette-sample-count
   #:silhouette
   #:cluster-contents
   #:parameters
   #:algorithm-state
   #:algorithm-state-initialization-list
   #:result-initialization-list
   #:data
   #:make-algorithm-state
   #:state-class
   #:obtain-result
   #:result))
