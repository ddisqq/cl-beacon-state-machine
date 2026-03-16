(asdf:defsystem #:cl-beacon-state-machine
  :depends-on (#:alexandria #:bordeaux-threads)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "cl-beacon-state-machine" :depends-on ("package"))))))