# clusters
Variety of clustering tools for the Common Lisp

## Distances
Package clusters.distance presentes distance functions, intended for use in clustering algorithms. This includes:
* bhattacharyya
* sinkhorn

## Metrics
Package clusters.metric also presents functions intended for use in clustering algorithms, However, these functions also happen to be true metrics.
* average
* group-average
* earth-mover
* euclid
* hausdorff
* hellinger
* lcs
* levenshtein
* svr

## Clustering algorithms
Each clustering algorithm has its own package exporting PARAMETERS symbol. This symbol names the class used to hold the algorithm parameters. To use the algorithm, first construct instance of this class, then call CLUSTER function passing the PARAMETERS instance into it along with the DATA. The returned object contains the clusters, and they can be extracted using the CLUSTER-CONTENTS function. The following example has been taken from the tests.

```
(defun metric (a b)
  (coerce (abs (- a b))
          'single-float))

(let* ((data (~> (concatenate 'vector
                              (iota 100 :start 0)
                              (iota 300 :start 500)
                              (iota 100 :start 200))
                 shuffle))
       (parameters (make-instance 'clusters.clarans:parameters
                                  :parallelp nil
                                  :medoids-count 10
                                  :max-neighbor 200
                                  :distance-function #'metric))
       (clusters (clusters:cluster parameters data))
       (cluster-contents (clusters:cluster-contents clusters)))
    ...)
```

All PARAMETERS classes contain slots intended bootsrapping silhouette values for the clusters. To obtain the silhouette value from the clustering results simply call the CLUSTERS:SILHOUETTE function. This is particulary useful if you are attempting to find the optimal number of clusters.

## PARALLELP
Majority of the algorithms in this system have rudementary parallelization, made with the ever so useful LPARALLEL library. Please notice, that this means that (unless you are certain that NIL has been passed as :PARALLELP value) you must avoid calling CLUSTER on the LPARALLEL worker thread. Otherwise: deadlocks.

## Silhouette
You can use clusters:silhouette to obtain silhouette values for each of the constructed clusters.
