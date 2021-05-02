(cl:in-package #:clusters.distance)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function
   group-average
   (:description "Calculates group-average distance between two sets."))

  (function bhattacharyya
    (:description "Calculates Bhattacharyya distance between two distributions represented as frequency vector."
     :returns "Bhattacharyya distance as single-float."
     :arguments ((h1 "Frequency vector representing distribution.")
                 (h2 "Frequency vector representing distribution.")))))
