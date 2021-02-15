(asdf:defsystem clusters
  :name "clusters"
  :version "0.0.0"
  :license "BSD simplified"
  :author "Marek Kochanowicz"
  :maintainer "Marek Kochanowicz"
  :description "Tests for the clusters system."
  :depends-on (:clusters :prove)
  :defsystem-depends-on (:prove-asdf)
  :serial T
  :pathname "source"
  :components ((:file "aux-package")
               (:module "k-means"
                :components ((:test-file "tests")))))