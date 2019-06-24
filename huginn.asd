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
                                           (:file "tags")
                                           (:file "types")))
                             (:module "operations"
                              :components ((:file "package")
                                           (:file "copying")
                                           (:file "undo")
                                           (:file "unification")
                                           (:file "unfolding")
                                           ))))))
