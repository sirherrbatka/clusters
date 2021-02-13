(asdf:defsystem clusters
  :name "clusters"
  :version "0.0.0"
  :license "BSD simplified"
  :author "Marek Kochanowicz"
  :maintainer "Marek Kochanowicz"
  :description "Cluster algorithms in CL, for CL."
  :depends-on ( :iterate       :alexandria
                :serapeum      :documentation-utils-extensions
                :metabang.bind :bordeaux-threads
                :lparallel)
  :serial T
  :pathname "source"
  :components ((:file "aux-package")
               (:module "utils"
                :components ((:file "package")
                             (:file "random")))
               (:module "metric"
                :components ((:file "package")
                             (:file "euclid")))
               (:module "k-means"
                :components ((:file "package")
                             (:file "types")
                             (:file "internal")
                             (:file "external")))))
