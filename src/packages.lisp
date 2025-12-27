
(uiop:define-package #:cl-interactive/conditions
  (:use :cl)
  (:import-from #:cl-interactive
                #:cl-interactive-error
                #:abort-interactive-command
                #:no-applicable-command-implementation
                #:not-a-command-error
                #:missing-required-arguments-error
                #:no-interactive-function-error
                #:invalid-interactive-function
                #:define-command-invalid-argument-error
                #:unknown-completions-error
                #:unprepared-completions-error))

(uiop:define-package #:cl-interactive/search-tree
  (:use :cl)
  (:export #:search-tree-node
           #:search-tree
           #:search-in-search-tree
           #:add-string-to-tree))

(uiop:define-package #:cl-interactive/database
  (:use :cl)
  (:import-from #:cl-interactive
                #:*database-string-type*
                #:*default-command-database*
                #:database
                #:make-database
                #:map-database
                #:database-strings
                #:search-in-database
                #:add-to-database)
  (:import-from #:cl-interactive/search-tree
                #:search-tree-node
                #:search-tree
                #:search-in-search-tree
                #:add-string-to-tree))

(uiop:define-package #:cl-interactive/input-method
  (:use :cl)
  (:import-from #:cl-interactive
                #:*default-input-method*
                #:*current-input-method*
                #:input-method
                #:prepare-completions-for-input-method
                #:completing-read
                #:read-string
                #:input-method-read
                #:with-input-method-error-handling
                #:interactive-error-handler-for-input-method
                #:cl-interactive-error
                #:abort-interactive-command))

(uiop:define-package #:cl-interactive/command
  (:use :cl)
  (:import-from #:cl-interactive
                #:*interactive*
                #:*current-interactive-command*
                #:*current-interactive-arguments*
                #:command
                #:define-command
                #:no-applicable-command-implementation
                #:interactive-component
                #:compute-interactive-component-value
                #:read-argument-interactively
                #:call-command-interactively

                #:input-method
                #:*default-input-method*
                #:*current-input-method*
                #:completing-read
                #:with-input-method-error-handling
                #:interactive-error-handler-for-input-method

                #:*default-command-database*
                #:add-to-database

                #:unknown-completions-error
                #:unprepared-completions-error
                #:not-a-command-error))



