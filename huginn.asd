(cl:in-package #:cl-user)


(asdf:defsystem huginn
  :name "huginn"
  :version "0.0.0"
  :author "Marek Kochanowicz"
  :depends-on (:iterate :alexandria :prove :prove-asdf
               :serapeum :cl-data-structures)
  :serial T
  :pathname "src"
  :defsystem-depends-on (:prove-asdf)
  :components ((:file "aux-package")
               (:module "machine"
                :components ((:module "representation"
                              :components ((:file "package")
                                           (:file "tags")
                                           (:file "types")))
                             (:module "operations"
                              :components ((:file "package")
                                           (:file "copying")
                                           (:file "match-clauses")
                                           (:file "undo")
                                           (:file "unification")
                                           (:file "unfolding")))))
               (:module "compiler"
                :components ((:file "package")
                             (:file "protocol")
                             (:file "implementation")))))
