
(asdf:defsystem #:cl-interactive
  :depends-on (#:closer-mop)
  :serial t
  :in-order-to ((test-op (test-op cl-interactive/test)))
  :components ((:module #:src
                :components ((:file "packages")
                             (:file "input-methods")
                             (:file "conditions")
                             (:file "search-tree")
                             (:file "database")
                             (:file "commands")))))

(asdf:defsystem #:cl-interactive/test
  :depends-on (#:cl-interactive #:fiveam)
  :serial t
  :components ((:module #:test
                :components ((:file "package")
                             (:file "commands")
                             (:file "database"))))
  :perform (test-op :after (op c)
                    (uiop/package:symbol-call "FIVEAM" "RUN-ALL-TESTS")))
