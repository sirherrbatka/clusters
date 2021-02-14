(cl:defpackage #:clusters.utils
  (:use #:cl #:clusters.aux-package)
  (:export
   #:count->half-matri-size
   #:draw-random-vector
   #:half-matrix-index->square-row/column
   #:half-matrix-size->count
   #:map-into-half-matrix
   #:distance-matrix
   #:pmap
   #:transform
   #:make-half-matrix
   #:mref
   #:square-row/column->half-matrix-index))
