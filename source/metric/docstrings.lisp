(cl:in-package #:clusters.metric)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function
   euclid
   (:description "Calculates the Euclid distance between two simple-arrays of single-float"
    :returns "Non-negative-single-float distance."))

  (function
   average
   (:description "Calculates average distance between two sets, based around the FN metric function"
    :returns "Average distance between two sets."
    :notes ("Algorithm described in https://doi.org/10.1007/s13160-012-0089-6"
            "KEY function is used in conjuction with the TEST function to construct unions and differences between sets. Not for extracting values for the FN itself. This allows FN to operate on the weight, count or other secondary attribute of the set element."
            "Quadratic time complexity, suitable only for small sets.")))

  (function
   lcs
   (:description "Largest common subsequence metric for vectors."))

  (function levenshtein
    (:description "Calculates the Levenshtein distance."
     :returns "Non-negative fixnum representing the distance."
     :arguments ((str1 "String.")
                 (str2 "String."))))

  (function
   levenshtein
   (:description "Traditional edit distance for strings."))

  (function
   hausdorff
   (:description "Hausdorff distance. Accepts vectors as arguments."))

  (function svr
    (:description "Calculates the subvector representation based metric."
     :returns "Single-float (between 0.0 and 1.0) representing the distance."
     :arguments ((a "Vector.")
                 (b "Vector."))
     :notes ("Content of a and b vectors must be comparable using EQUAL."
             "Algorithm described in https://doi.org/10.1177%2F0049124114540707"
             "Will return 1.0 if one of the vectors is empty and the second is not empty.")))

  (function hellinger
    (:description "Calculates hellinger distance between two distributions, both represented as histograms."
     :arguments ((q "Frequency vector.")
                 (p "Frequency vector."))
     :returns "Hellinger distance."))

  (function earth-mover
    (:description "Calculates earth mover distance between two distributions, both represented as histograms."
     :arguments ((a "Frequency vector.")
                 (b "Frequency vector."))
     :returns "Earth mover distance between two histograms.")))
