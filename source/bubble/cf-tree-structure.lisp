(cl:in-package #:cl-data-structures.utils.clustering.bubble)


(defclass cf-tree ()
  ((%distance-function :initarg :distance-function
                       :type function
                       :reader read-distance-function
                       :documentation "Callback returning number representing distance between elements.")
   (%sampling-rate :initarg :sampling-rate
                   :type positive-fixnum
                   :reader read-sampling-rate
                   :documentation "Number of inserts in the subtree that can be performed BEFORE triggering resampling.")
   (%subtree-sample-size :initarg :subtree-sample-size
                         :type positive-fixnum
                         :reader read-subtree-sample-size
                         :documentation "Maximal size of the sample in the subtree.")
   (%subtree-maximum-arity :initarg :subtree-maximum-arity
                           :documentation "Maximum number of children possible in the subtree node. Larger subtrees will be splitted."
                           :reader read-subtree-maximum-arity)
   (%leaf-maximum-size :initarg :leaf-maximum-size
                       :reader read-leaf-maximum-size
                       :documentation "Maximum length of the %content vector in the cf-leaf.")
   (%leaf-maximum-radius :initarg :leaf-maximum-radius
                         :reader read-leaf-maximum-radius
                         :documentation "Maximum radius of the leaf allowed. Larger leafs will be splitted.")
   (%parallel-samples-count :initarg :parallel-samples-count
                            :reader read-parallel-samples-count)
   (%parallel-sample-size :initarg :parallel-sample-size
                          :reader read-parallel-sample-size)
   (%parallel-reference-size :initarg :parallel-reference-size
                             :reader read-parallel-reference-size))
  (:default-initargs
   :parallel-samples-count nil))


(defclass fundamental-cf-node ()
  ())


(defclass bubble ()
  ((%content :initarg :content
             :reader read-content
             :type vector)
   (%clusteroid :initarg :clusteroid
                :reader read-clusteroid
                :type vector)))


(defclass cf-leaf (fundamental-cf-node)
  ((%content :initarg :content
             :reader read-content
             :type vector)
   (%row-sums :initarg :row-sums
              :reader read-row-sums
              :type vector)
   (%radius :initarg :radius
            :accessor access-radius))
  (:default-initargs
   :radius nil))


(defclass cf-subtree (fundamental-cf-node)
  ((%children :initarg :children
              :reader read-children
              :type vector)
   (%inserts :initarg :inserts
             :accessor access-inserts
             :type fixnum)
   (%sample :initarg :sample
            :accessor access-sample))
  (:default-initargs
   :sample nil
   :inserts 0))
