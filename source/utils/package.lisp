(cl:defpackage #:clusters.utils
  (:use #:cl #:clusters.aux-package)
  (:export
   #:copy-into
   #:count->half-matri-size
   #:distance-matrix
   #:draw-random-vector
   #:half-matrix-index->square-row/column
   #:half-matrix-size->count
   #:lazy-shuffle
   #:make-half-matrix
   #:map-into-half-matrix
   #:mref
   #:pmap
   #:pmap-into
   #:seed
   #:square-row/column->half-matrix-index
   #:swap-if
   #:transform))
