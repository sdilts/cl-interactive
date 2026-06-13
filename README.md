# CL-INTERACTIVE

Interactive functions for Common Lisp.

Interactive functions obtain their arguments interactively by prompting the
user. Input is obtained using an *input method*, such as a rofi input method,
which is then processed using an interactive function or class to obtain a value
for the argument in question. Finally, the function is invoked with the relevant
arguments.

## Quickstart

A trivial example is the rofi input method and the command
`EXECUTE-EXTENDED-COMMAND`.

### Execute Extended Command

This example implements the emacs `M-x` behavior; i.e. it defines a command
which reads another command and calls it interactively.

``` lisp
(uiop:define-package #:cl-interactive/example-command
  (:use :cl)
  (:import-from #:cl-interactive
                #:command
                #:define-command
                #:prepare-completions-for-input-method
                #:completing-read
                #:search-in-database
                #:call-command-interactively
                #:*default-command-database*
                #:*current-input-method*))

(in-package :cl-interactive/example-command)

(defun interactively-read-command (command input-method argument-name
                                   &optional (database *default-command-database*))
  (declare (ignore command argument-name)
           (optimize (debug 3)))
  (let ((cmd (completing-read input-method "Enter a command" :completions comps
                                                             :require-match t)))
    (search-in-database database cmd :from-beginning t :partial nil)))

(define-command execute-extended-command
    ((command (:function interactively-read-command)))
  (:method ((command command))
    (call-command-interactively command *current-input-method*)))
```

### Rofi Input Method

This example defines a trivial input method based on rofi. While this could be
made much more featureful - e.g. by defining additional switches, etc. - this
implements the basic neccessary features.

``` lisp
(uiop:define-package #:cl-interactive/rofi-input-method
  (:use :cl)
  (:import-from #:cl-interactive/input-method
                #:input-method
                #:prepare-completions-for-input-method
                #:input-method-read)
  (:import-from #:cl-interactive/database
                #:database
                #:search-in-database
                #:database-strings))

(in-package #:cl-interactive/rofi-input-method)

(defclass rofi-input-method (input-method) ())

(defmethod prepare-completions-for-input-method ((im rofi-input-method)
                                                 (completions database))
  (database-strings completions))

(defmethod input-method-read ((im rofi-input-method) (prompt string)
                              &key completions require-match initial-input
                                history
                              &allow-other-keys)
  (declare (ignore initial-input history))
  (let ((pset nil))
    (tagbody
     start
       (let ((res (run-simple-rofi prompt completions nil)))
         (cond ((find res completions :test #'string=)
                (return-from input-method-read res))
               (require-match
                (psetf pset t
                       prompt (if pset
                                  prompt
                                  (concatenate 'string "[Invalid entry] "
                                               prompt)))
                (go start))
               (t (return-from input-method-read res)))))))

(defun run-rofi (arguments input)
  (multiple-value-bind (o e s)
      (uiop:run-program (cons "rofi" arguments)
                        :output '(:string :stripped t)
                        :input (make-string-input-stream
                                (typecase input
                                  (string input)
                                  ((or (cons string cons)
                                       (cons string null))
                                   (format nil "~{~A~^~%~}" input))
                                  (null "")
                                  (otherwise
                                   (error "invalid inptu to rofi"))))
                        :ignore-error-status t
                        :force-shell nil)
    (if (= s 0)
        (values o e s)
        (error "rofi exited badly with status ~D" s))))

(defun run-simple-rofi (prompt input &optional lines)
  "Run rofi instead. whoops"
  (multiple-value-bind (output error status)
      (run-rofi (list* "-dmenu" "-p" prompt (when lines (list "-l" "10")))
                input)
    (cond ((= status 0)
           (values output nil 0 nil))
          (t (values nil error status output)))))
```

## Extensibility and Packages

This system is comprised of several subsystems. All symbols are accessible
through the package `CL-INTERACTIVE`. If a symbol is to be exported, it should
be exported from `CL-INTERACTIVE` and imported into the relevant package.

### CL-INTERACTIVE/INPUT-METHOD

This package implements the basic input method functionality. A relevant input
method is obtained via the function `input-method`, which checks for a user
provided input method, the current input method (`*current-input-method*`, and
the default input method (`*default-input-method*`).

User input is obtained via the functions `completing-read` and
`read-string`. Under the hood these functions call the generic functions
`input-method-read` and `prepare-completions-for-input-method`, which are the
two generic functions that must be implemented in order to implement a new input
method.

Input methods MUST descend from the root class `input-method`.

### CL-INTERACTIVE/COMMAND

This package implements command definition and interactive invocation. Commands
are defined by the macro `DEFINE-COMMAND`, which is similar to defgeneric.
Instead of a defgeneric lambda list commands take a command lambda list, which
takes arguments that optionally have an interactive component
specified. `&optional`, `&key`, `&allow-other-keys` have their normal
meaning, but all other symbols in the lambda list can follow this form:

```
COMMAND-ARG : NON-INTERACTIVE | INTERACTIVE-ARG
;; this is just a normal, non interactive argument:
NON-INTERACTIVE : arg-name
;; There's three ways to specify how to get interactive arguments;
;; either by a function or a class:
INTERACTIVE-ARG : (arg-name FUNCTION-GATHERER | CLASS-GATHERER | DEFAULT-GATHERER)

FUNCTION-GATHERER : (:function func-name arg)

CLASS-GATHERER : (:class class-designator arg)

DEFAULT-GATHERER : (:default designator args)
```

You can supply the values for non-interative arguments by passing a datastructure
into the `already-gathered` argument of `call-command-interactively`
and `gather-args-interactively`. This is the the only way to supply
non-interactive arguments, but it can be used to supply interactive
arguments as well.

#### :function specifiers

When a function is in the interactive arg spec, it gets four
positional parameters:
+ `command`: The command that the args are being gathered for
+ `input-method`: The input method that should be used to gather the
  argument value.
+ `arg-symbol`: The symbol of the argument that is being gathered
+ `arg`: The arg specified as the last item in the argument specification.

#### Example

``` lisp
(cl-interactive:define-command example (foo (bar (:function #'func)))
  (format t "Interactive arg: ~S, Non-interactive arg: ~S"
		  bar foo))
```
The above code creates a command called `example`, with an interactive
argument called `bar` and a non-interactive one called `foo`. This is
a normal function, and you can call it like you would any other:
``` lisp
(example "foo" "bar")
;; Prints 'Interactive arg: "bar", Non-interactive arg: "foo"'
```

To invoke this function and gather the arguments interactively, you can
use the `call-interactively` function:

``` lisp
(cl-interactive:with-gathered-args ((bar "bar"))
    args
  (cl-interactive:call-command-interactively #'example
											 :already-gathered args))
```
This will call the `#'func` function in the `define-command` form
above to be called, and the result it returns will be passed in as the
`foo` argument to the `example` command.

Notably, if you use `call-command-interactively` without specifying
non-interactive arguments, you get an error:

``` lisp
;; Raises signal along the lines of
;; "No argument in {(FOO . "value")) matching BAR".
(cl-interactive:call-command-interactively #'example)
```
