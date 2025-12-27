
(uiop:define-package #:cl-interactive
  (:use :cl)
  (:export #:*interactive*
           #:*current-interactive-command*
           #:*current-interactive-arguments*
           #:*default-input-method*
           #:*current-input-method*
           #:*current-interactive-argument*
           #:*database-string-type*
           #:*default-command-database*

           #:input-method
           #:prepare-completions-for-input-method
           #:completing-read
           #:read-string
           #:input-method-read
           #:with-input-method-error-handling
           #:interactive-error-handler-for-input-method

           #:command
           #:define-command
           #:no-applicable-command-implementation

           #:interactive-component
           #:compute-interactive-component-value

           #:read-argument-interactively
           #:call-command-interactively

           #:database
           #:make-database
           #:map-database
           #:search-in-database
           #:add-to-database
           #:database-strings

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
