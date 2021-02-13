(cl:in-package #:cl-data-structures.utils.clustering.clara/pam)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function partition-around-medoids
    (:description "Clusters data set using partition-around-medoids algorithm. Requires precalculated distance matrix that will contain distance of each pair in the data set."
     :arguments ((input-data "Data to cluster.")
                 (distance-matrix "HALF-MATRIX containing distance for each element pair.")
                 (select-medoids-attempts-count "How many times PAM should attempt to select medoids before accepting suboptimal medoids?")
                 (attempts "How many times clusters should be splitted and merged before accepting suboptimal cluster sizes.")
                 (split "Threshold size of clusters. Clusters above this size will attempt to be splitted.")
                 (merge "Threshold size of clusters. Clusters below this size will attempt to be merged."))
     :notes "Not well suited for clustering large data sets, as construction of full distance matrix will take large ammount of cpu cycles and memory."
     :returns "Vector of vectors (each inner vector represents cluster)."
     :thread-safety "Uses lparallel underneath."))

  (function clara-variable-number-of-medoids
    (:description "Runs clara mutiple-times with different NUMBER-OF-MEDOIDS, selects best number of medoids based on the silhouette and then returns it."
     :arguments ((input-data "Data to cluster.")
                 (sample-size "Size of sample for clara algorithm.")
                 (sample-count "Number of samples drawn for clara.")
                 (from "Smallest number of medoids to try.")
                 (to "Highest number of medoids to try.")
                 (key "Function used to extract value for metric-fn.")
                 (select-medoids-attempts-count "How many times PAM should attempt to select medoids before accepting suboptimal medoids?")
                 (attempts "How many times clusters should be splitted and merged before accepting suboptimal cluster sizes.")
                 (split "Threshold size of clusters. Clusters above this size will attempt to be splitted.")
                 (merge "Threshold size of clusters. Clusters below this size will attempt to be merged."))
     :thread-safety "Uses both lparallel and it's own threads. Will start independent clara in separate threads. This is MOSTLY safe, because actual heavy lifting is delegated to lparallel threads."))

  (function clara
    (:description "Clusters data set using CLARA algorithm. This algorithm attempts to cluster random subset, picking best set of clusters."
     :notes "Useful for clustering large data sets, as required memory is linear to the size of data set and quadratic to the size of the sample."
     :returns ("Vector of vectors (each inner vector represents cluster)."
               "Silhouette of each cluster (as vector of single-floats).")
     :thread-safety "Uses lparallel underneath.")))
