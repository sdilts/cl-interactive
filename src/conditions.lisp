
(in-package #:cl-interactive/conditions)

(define-condition cl-interactive-error (error) ())

(define-condition abort-interactive-command (cl-interactive-error)
  ((command :initarg :command :reader aic-command)
   (arguments :initarg :arguments :reader aic-arguments)))

(define-condition no-applicable-command-implementation (cl-interactive-error)
  ((command :initarg :command :reader naci-command)
   (arguments :initarg :arguments :reader naci-arguments)
   (input-method :initarg :input-method :reader naci-input-method))
  (:report
   (lambda (c s)
     (format s "No applicable command implementation for ~S~
                with arguments ~S"
             (naci-command c)
             (naci-arguments c)))))

(define-condition not-a-command-error (cl-interactive-error)
  ((command :initarg :command :reader nace-command))
  (:report
   (lambda (c s)
     (format s "~A is not a command" (nace-command c)))))

(define-condition missing-required-arguments-error (cl-interactive-error)
  ((command :initarg :command :reader mrae-command)
   (arguments :initarg :missing-arguments :reader mrae-arguments))
  (:report
   (lambda (c s)
     (format s "~A is missing required (positional) arguments:~%~A"
             (mrae-command c)
             (mrae-arguments c)))))

(define-condition no-interactive-function-error (missing-required-arguments-error)
  ()
  (:report
   (lambda (c s)
     (format s "Required (positional) arguments are missing and no interactive
function is present to acquire them for command ~A. The missing arguments are~%~A"
             (mrae-command c)
             (mrae-arguments c)))))

(define-condition invalid-interactive-function (cl-interactive-error)
  ((provided :initarg :provided :reader iif-provided))
  (:report
   (lambda (c s)
     (format s "~A is not a valid function designator"
             (iif-provided c)))))

(define-condition define-command-invalid-argument-error (cl-interactive-error)
  ((argument :initarg :argument :reader dciae-argument)
   (argtype :initarg :type :reader dciae-argument-type))
  (:report
   (lambda (c s)
     (format s "Invalid ~A argument ~A"
             (if (stringp (dciae-argument-type c))
                 (dciae-argument-type c)
                 (case (dciae-argument-type c)
                   (:positional "positional")
                   (:optional "optional")
                   (:rest "rest")
                   (:key "key")
                   (otherwise 'unknown)))
             (dciae-argument c)))))

(define-condition unknown-completions-error (cl-interactive-error)
  ((completions :initarg :completions :reader uce-completions)
   (input-method :initarg :input-method :reader uce-input-method))
  (:report
   (lambda (c s)
     (format s "Unknown completions for input method ~S~%~A"
             (uce-input-method c)
             (uce-completions c)))))

(define-condition unprepared-completions-error (error)
  ((completions :initarg :completions :reader uce-completions)
   (input-method :initarg :input-method :reader uce-input-method))
  (:report
   (lambda (c s)
     (format s "Completions were not prepared for input method ~S"
             (uce-input-method c)))))
