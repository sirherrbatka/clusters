(asdf:defsystem clusters
  :name "clusters"
  :version "0.0.0"
  :license "BSD simplified"
  :author "Marek Kochanowicz"
  :maintainer "Marek Kochanowicz"
  :description "Cluster algorithms in CL, for CL."
  :depends-on ( :iterate       :alexandria
                :serapeum      :documentation-utils-extensions
                :metabang-bind :bordeaux-threads
                :lparallel)
  :serial T
  :pathname "source"
  :components ((:file "aux-package")
               (:module "utils"
                :components ((:file "package")
                             (:file "maps")
                             (:file "matrix")
                             (:file "random")
                             (:file "utils")))
               (:module "metric"
                :components ((:file "package")
                             (:file "euclid")
                             (:file "hausdorff")))
               (:module "distance"
                :components ((:file "package")
                             (:file "group-average")))
               (:module "common"
                :components ((:file "package")
                             (:file "generics")
                             (:file "types")
                             (:file "utils")
                             (:file "functions")
                             (:file "silhouette")
                             (:file "methods")))
               (:module "pam"
                :components ((:file "package")
                             (:file "generics")
                             (:file "types")
                             (:file "utils")
                             (:file "methods")))
               (:module "k-means"
                :components ((:file "package")
                             (:file "types")
                             (:file "utils")
                             (:file "methods")))))
