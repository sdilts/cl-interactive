
(asdf:defsystem #:cl-interactive
  :depends-on (#:closer-mop)
  :serial t
  :components ((:file "api")
               (:module #:src
                :components ((:file "packages")
                             (:file "conditions")
                             (:file "search-tree")
                             (:file "database")
                             (:file "input-methods")
                             (:file "commands")))))

