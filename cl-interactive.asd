
(asdf:defsystem #:cl-interactive
  :depends-on (#:closer-mop)
  :serial t
  :components ((:module #:src
                :components ((:file "packages")
                             (:file "conditions")
                             (:file "search-tree")
                             (:file "database")
                             (:file "input-methods")
                             (:file "commands")))))

(asdf:defsystem #:cl-interactive/test
  :depends-on (#:cl-interactive #:fiveam)
  :serial t
  :components ((:module #:test
                :components ((:file "package")
                             (:file "database")))))

