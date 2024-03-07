(in-package :cl-user)
(defpackage planesnwind-asd
  (:use :cl :asdf))
(in-package :planesnwind-asd)

(defsystem planesnwind
  :version "0.1"
  :author "Jack"
  :license ""
  :entry-point "planesnwind:start-as-worker"
  :depends-on (:cl-ppcre
               :cl-syntax-annot
               :cl-pass
               :cl-json
               :datafly
               :sxql
               :drakma
               :alexandria
               :ningle
               :clack
               :uuid
               :plump
               :lquery
               :cl-postgres)
  :components ((:module "src"
                        :components
                        ((:file "planesnwind" :depends-on ("db" "util" "worker" "flights" "paths"))
                         (:file "flights"  :depends-on ("db" "util"))
                         (:file "winds"  :depends-on ("db" "util"))
                         (:file "worker"  :depends-on ("db" "util"))
                         (:file "db"  :depends-on ("util"))
                         (:file "paths"  :depends-on ("db" "util" "http"))
                         (:file "http" :depends-on ("db" "winds"))
                         (:file "util"))))
  :description ""
  :in-order-to ((test-op (load-op planesnwind-test))))
