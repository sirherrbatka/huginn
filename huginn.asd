(cl:in-package #:cl-user)


(asdf:defsystem huginn
  :name "huginn"
  :version "0.0.0"
  :author "Marek Kochanowicz"
  :depends-on (:iterate :alexandria
               :serapeum :cl-data-structures)
  :serial T
  :pathname "src"
  :components ((:file "aux-package")
               (:module "machine"
                :components ((:module "representation"
                              :components ((:file "package")
                                           (:file "utils")
                                           (:file "tokens")
                                           (:file "tags")
                                           (:file "types")
                                           (:file "undo")
                                           (:file "unification")
                                           (:file "unfolding")))
                             (:module "operations"
                              :components ((:file "package")
                                           ))))))
