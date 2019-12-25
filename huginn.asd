(cl:in-package #:cl-user)


(asdf:defsystem huginn
  :name "huginn"
  :version "0.0.0"
  :author "Marek Kochanowicz"
  :depends-on (:iterate :alexandria :prove :prove-asdf
               :flexichain :serapeum :cl-data-structures
               :more-conditions)
  :serial T
  :pathname "src"
  :defsystem-depends-on (:prove-asdf)
  :components ((:file "aux-package")
               (:module "machine"
                :components ((:module "representation"
                              :components ((:file "package")
                                           (:file "conditions")
                                           (:file "tags")
                                           (:file "types")
                                           (:file "functions")))
                             (:module "database"
                              :components ((:file "package")
                                           (:file "conditions")
                                           (:file "protocol")
                                           (:file "clause-range")
                                           (:file "implementation")))
                             (:module "operations"
                              :components ((:file "package")
                                           (:file "macros")
                                           (:file "copying")
                                           (:file "undo")
                                           (:file "unification")
                                           (:file "unfolding")
                                           (:file "tail-call")))))
               (:module "compiler"
                :components ((:file "package")
                             (:file "utils")
                             (:file "types")
                             (:file "conditions")
                             (:file "protocol")
                             (:file "implementation")
                             (:test-file "tests")))
               (:module "api"
                :components ((:file "package")
                             (:file "conditions")
                             (:file "variables")
                             (:file "macros")
                             (:file "internal")
                             (:file "functions")))
               (:module "tests"
                :components ((:file "package")
                             (:test-file "trivial")
                             (:test-file "zebra")))))
