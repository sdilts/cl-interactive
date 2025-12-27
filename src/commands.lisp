
(in-package #:cl-interactive/command)

(defvar *interactive* nil
  "Dynamically bound to T within interactive calls.")
(defvar *current-interactive-command* nil
  "The current command being interactively invoked")
(defvar *current-interactive-arguments* nil
  "The arguments being passed to *current-interactive-command*")

(defclass command (c2mop:standard-generic-function)
  ((interactive-components :initarg :interactive-components
                           :accessor interactive-components)
   (interactive-function :initarg :interactive-function
                         :accessor interactive-function)
   (pass-optional-and-key-to-interactive-function :initarg :pass-optional-key
                                                  :accessor pass-optional-key))
  (:metaclass c2mop:funcallable-standard-class))

(defmethod no-applicable-method ((gf command) &rest args)
  (error 'no-applicable-command-implementation
         :command gf
         :arguments args
         :input-method (input-method)))

(defclass interactive-component () ())

(defgeneric read-argument-interactively
    (command argument input-method interactive &key &allow-other-keys)
  (:documentation "Read an argument interactively.

COMMAND - the command object
ARGUMENT - a symbol naming the argument as it appears in DEFINE-COMMAND
INPUT-METHOD - the input method to use
INTERACTIVE - the interactive portion of the command argument"))

(defgeneric compute-interactive-component-value (command
                                                 argument
                                                 interactive-component
                                                 compute-value-from
                                                 &key &allow-other-keys)
  (:documentation
   "After reading an interactive component compute its resultant value, likely
from a string. A method for this generic specializing interactive-component as a
function is predefined, and returns COMPUTE-VALUE-FROM unmodified.

COMMAND will always be the command object

ARGUMENT will always be the symbol naming the argument as it appears in
DEFINE-COMMAND

INTERACTIVE-COMPONENT should specialize on the class of the interactive
component

COMPUTE-VALUE-FROM should specialize on the value returned by reading the
interactive argument using an input method. This will likely be a string, but
may be any object.")
  (:method ((command command) (argument symbol) (interactive-component function)
            compute-value-from &key &allow-other-keys)
    compute-value-from))

(defun call-command-interactively (command &key (input-method (input-method)))
  "Call COMMAND interactively using INPUT-METHOD. This function loops through the
interactive argument list and obtains each argument using that list."
  (declare (optimize (debug 3)))
  (let ((command (if (symbolp command) (symbol-function command) command))
        (*current-input-method* input-method))
    (restart-case
        (handler-bind
            ((error
               (interactive-error-handler-for-input-method (input-method))))
          (unless (typep command 'command)
            (error 'not-a-command-error :command command))
          (flet ((compute-if-positionals-missing (still-needed)
                   (let ((ll (c2mop:generic-function-lambda-list command)))
                     (loop with &ers = '(&optional &rest &key &allow-other-keys)
                           for el in ll
                           until (member el &ers)
                           when (find el still-needed :key #'car)
                             do (return-from compute-if-positionals-missing t))
                     nil))
                 (get-missing-arguments (args)
                   (remove-if-not (lambda (el)
                                    (eql el 'argument-interactive-placeholder))
                                  args
                                  :key #'cdr))
                 (get-the-argument (arg it is ia)
                   (let* ((inst
                            (case it
                              ((:default) (if (find-class is)
                                              (apply #'make-instance is ia)
                                              is))
                              ((:class) (apply #'make-instance is ia))
                              ((:function) (etypecase is
                                             (function is)
                                             (symbol (symbol-function is))))
                              (otherwise (return-from get-the-argument
                                           'argument-interactive-placeholder))))
                          (pre-value
                            (if (typep inst 'interactive-component)
                                (read-argument-interactively command
                                                             arg
                                                             input-method
                                                             inst)
                                (apply inst command input-method arg ia))))
                     (compute-interactive-component-value command
                                                          arg
                                                          inst
                                                          pre-value)))
                 (argument-missing-p (arg)
                   (and (consp arg)
                        (eql (cdr arg) 'argument-interactive-placeholder))))
            (let* ((interactive-function (interactive-function command))
                   (argument-list
                     (loop for (arg it is ia) in (interactive-components command)
                           collect (cons arg (get-the-argument arg it is ia))))
                   (still-needed
                     (remove-if-not #'argument-missing-p argument-list))
                   (obtained (remove-if #'argument-missing-p argument-list))
                   (missing-positionals
                     (compute-if-positionals-missing still-needed)))
              (when (and missing-positionals (null interactive-function))
                (error 'no-interactive-function-error
                       :command command
                       :missing-arguments (get-missing-arguments argument-list)))
              (when interactive-function
                (funcall interactive-function
                         command
                         input-method
                         still-needed
                         obtained))
              (when (compute-if-positionals-missing still-needed)
                (cerror "Use NIL for missing arguments"
                        'missing-required-arguments-error
                        :command command
                        :missing-arguments (get-missing-arguments argument-list)))
              (map nil
                   (lambda (arg)
                     (when (eql (cdr arg) 'argument-interactive-placeholder)
                       (setf (cdr arg) nil)))
                   argument-list)
              (call-command-with-argument-list command argument-list))))
      (abort-command ()
        :report "Abort command"
        (values nil t)))))

(defun call-command-with-argument-list (command arguments-list)
  (declare (optimize (debug 3)))
  (let* ((ll (c2mop:generic-function-lambda-list command))
         (keys (member '&key ll))
         (keys (loop for key in keys until (member key '(&aux &allow-other-keys))
                     collect key)))
    (multiple-value-bind (normals keyargs)
        (loop for arg in arguments-list
              for found = (find (car arg) keys
                                :key (lambda (k) (if (atom k) k (cadar k)))
                                :test #'eql)
              if found
                collect (list found arg) into ks
              else
                collect arg into ns
              finally (return (values ns ks)))
      (let ((*current-interactive-command* command)
            (*current-interactive-arguments*
              (append (mapcar #'cdr normals)
                      (mapcan (lambda (k)
                                (let ((k (car k))
                                      (a (cdr k)))
                                  (list (if (consp k)
                                            (caar k)
                                            (intern (string k) :keyword))
                                        (cdr a))))
                              keyargs)))
            (*interactive* t))
        (apply *current-interactive-command* *current-interactive-arguments*)))))

 ;; Parsers and helpers for define-command
(defun parse-interactive (interactive)
  "Parse a user provided :INTERACTIVE option to define-command"
  (cond ((symbolp interactive)
         (return-from parse-interactive `(quote ,interactive)))
        ((consp interactive)
         (ecase (car interactive)
           ((function lambda)
            (return-from parse-interactive interactive)))))
  (error 'invalid-interactive-function :provided interactive))

(defun parse-arguments-for-define-command (arguments-list)
  "Given an list of arguments as provided to DEFINE-COMMAND, return the
arguments as processed for a defgeneric form and an interactive argument list as
multiple values."
  (let ((current-type '&positional)
        (defgeneric-list nil)
        (interactive-components nil)
        (rest-sentinel nil))
    (dolist (argument arguments-list (values (reverse defgeneric-list)
                                             (reverse interactive-components)))
      (if (member argument '(&optional &rest &key &allow-other-keys))
          (setf current-type argument
                defgeneric-list (cons argument defgeneric-list))
          (ecase current-type
            ((&rest)
             (when (eql current-type '&rest)
               (if rest-sentinel
                   (error "Multiple &rest arguments provided")
                   (setf rest-sentinel t)))
             (assert (symbolp argument) (argument)
                     "&REST arguments cannot have interactive components")
             (push argument defgeneric-list)
             (push (list argument '&rest nil nil) interactive-components))
            ((&positional &optional)
             (multiple-value-bind (argname it is ia)
                 (parse-positional-optional-rest-argument-for-define-command
                  argument)
               (push argname defgeneric-list)
               (push (list argname it is ia) interactive-components)))
            ((&key)
             (multiple-value-bind (argkey argname it is ia)
                 (parse-key-argument-for-define-command argument)
               (push (list (list argkey argname)) defgeneric-list)
               (push (list argname it is ia) interactive-components)))
            ((&allow-other-keys)
             (error "&ALLOW-OTHER-KEYS must be the final symbol in~
                     an interactive lambda list")))))))

(defun parse-positional-optional-rest-argument-for-define-command (argument)
  "Return the argument name, the interactive type, the interactive
symbol, and the interactive arguments as multiple values"
  (cond ((symbolp argument)
         argument)
        ((and (consp argument)
              (symbolp (car argument)))
         (multiple-value-bind (it is ia)
             (parse-interactive-component (cadr argument))
           (values (car argument) it is ia)))
        (t (error 'define-command-invalid-argument-error
                  :argument argument
                  :type "positional/optional/rest"))))

(defun parse-key-argument-for-define-command (argument)
  "Return the argument keyword, argument name, interactive type, interactive
symbol, and interactive arguments as multiple values"
  (cond ((symbolp argument)
         (values (intern (string argument) :keyword)
                 argument))
        ((and (consp argument)
              (symbolp (car argument)))
         (multiple-value-bind (it is ia)
             (parse-interactive-component (cadr argument))
           (values (intern (string (car argument)) :keyword)
                   (car argument)
                   it
                   is
                   ia)))
        ((and (consp argument)
              (consp (car argument))
              (symbolp (caar argument))
              (symbolp (cadar argument)))
         (multiple-value-bind (it is ia)
             (parse-interactive-component (cadr argument))
           (values (caar argument) (cadar argument) it is ia)))
        (t (error 'define-command-invalid-argument-error
                  :type :key
                  :argument argument))))

(defun parse-interactive-component (component)
  "Return the interactive type, the interactive symbol, and the interactive
arguments as multiple values"
  (cond ((null component) nil)
        ((symbolp component)
         (values :default component))
        ((and (consp component)
              (keywordp (car component)))
         (values (car component) (cadr component) (cddr component)))
        ((and (consp component)
              (symbolp (car component)))
         (values :default (car component) (cdr component)))
        (t (error "Invalid interactive component ~S" component))))

(defmacro with-options ((remaining &rest options) options-list &body body)
  (let ((r (gensym)))
    `(let* ((,remaining (copy-list ,options-list))
            ,@(mapcar (lambda (k)
                        (let ((kw (intern (string k) :keyword)))
                          `(,k (let ((,r (find ,kw ,remaining :key #'car)))
                                 (setf ,remaining (remove ,r ,remaining))
                                 ,r))))
                      options))
       ,@body)))

 ;; Define command macro
(defmacro define-command (name command-lambda-list &body body)
  "Define a generic function of class command.

NAME will be the name of the generic function

COMMAND-LAMBDA-LIST is a command lambda list. It contains argument names
optionally with their interactive components. An interactive component is either
a symbol denoting a class or a function, or a list of such a symbol and its
arguments. If the list contains a keyword as its first element that keyword
determines if the interactive component names a function or a class. When
constructing a class make-instance is applied to the symbol and its provided
arguments. When calling a function the function is applied to the command, input
method, argument name, and its provided arguments. The shape of a command lambda
list is as follows:

({A | (A [{symbol | ([keyword] symbol argument*)}])}*
 [&optional {A | (A [{symbol | ([keyword] symbol argument*)}])}*]
 [&rest {A | (A [{symbol | ([keyword] symbol argument*)}])}]
 [&key {A | ({A | (keyword-name A)} [{symbol | ([keyword] symbol argument*)}])}*]
 [&allow-other-keys])

BODY is the set of valid options for defgeneric, with the following additions:

:INTERACTIVE, which if provided must be a valid value to FUNCTION. If provided
and arguments still need to be obtained, this function is called with four
arguments: the command, the input method, two alists an alist mapping argument
names to their values. The first of these alists hold unobtained arguments, the
second holds obtained arguments. Either of these alists may be destructively
modified in order to give arguments their new values. The argument names are the
symbols provided in the command lambda list.

:READ-MISSING-NONPOSITIONALS-INTERACTIVELY, which if T passes any missing
optional or key arguments to the provided interactive function in addition to
missing positional arguments.

If an argument has an interactive component, it will always be prompted for when
calling a command interactively."
  (let ((components (gensym)))
    (multiple-value-bind (defgen-ll interactive-components)
        (parse-arguments-for-define-command command-lambda-list)
      (with-options (remaining interactive
                               read-missing-nonpositionals-interactively
                               database)
                    body
        `(progn
           (defgeneric ,name ,defgen-ll
             (:generic-function-class command)
             ,@remaining)
           (setf (pass-optional-key #',name)
                 ,read-missing-nonpositionals-interactively
                 (interactive-function #',name)
                 ,(parse-interactive interactive))
           (add-to-database ,(if database
                                 `(or ,database *default-command-database*)
                                 '*default-command-database*)
                            #',name
                            nil
                            (string ',name))
           (let ((,components
                   ,(cons 'list
                          (mapcar (lambda (com)
                                    (if (cadr com)
                                        (list 'list
                                              `(quote ,(car com))
                                              (cadr com)
                                              `(quote ,(caddr com))
                                              (cons 'list (cadddr com)))
                                        com))
                                  interactive-components))))
             (setf (interactive-components #',name) ,components))
           #',name)))))

;; (defmacro defunc (name arguments &body body)
;;   (let ((args (remove '&optional arguments)))
;;     `(defun ,name (&optional ,@args)
;;        )))

 ;;; Example
 ;;;; OLD!

;; (uiop:define-package #:commands
;;   (:use :cl))

;; (in-package #:commands)

#|

This is intended for use with mahogany, and reflects that.

|#

;; ;; Interface:
;; (defgeneric read-argument-interactively
;;     (command argument input-method interactive &key &allow-other-keys))
;; (defgeneric obtain-interactive-argument
;;     (command argument input-method interactive &key &allow-other-keys))
;; (defgeneric read-interactive-argument (command input-method interactive
;;                                        &key &allow-other-keys))

;; (defvar *current-sequence*)
;; (defvar *current-seat*)
;; (defvar *current-command*)
;; (defvar *current-input-method*)
;; (defvar *current-needed-interactive-arguments*)

#|

DEFINING COMMANDS AND THEIR IMPLEMENTATIONS

A given interactive system needs to have a way of calling commands invoked by
the user. For this application, Emacs is probably the most familiar. You invoke
commands either programmatically or interactively. Programmatic invocation is
performed as a normal function call. Interactive invocation is performed either
through a gesture (such as a key binding or mouse click) or via M-x. This file
defines one method of creating such commands for a Common Lisp application. This
is a slightly opinionated way of doing this, and should not be taken as
canonical in any way shape or form.

Commands in this file are similar to generic functions; they are defined through
a form analogous to defgeneric, and implemented through a form analogous to
defmethod. The main thing of interest in this file is interactively obtaining
the values of command arguments, as well as defining what implementation of a
command should actually be invoked. Implementations are analogous to methods,
and the effective implementation is analogous to the effective method. The
reason for the introduction of new terms is to make reasoning about commands
separate from reasoning about generic functions, as commands carry additional
information with them.

The analog of defgeneric is DEFINE-COMMAND (abbreviated to DC). The analog of
defmethod is DEFINE-COMMAND-IMPLEMENTATION (abbreviated to DCI).

A given command argument MAY have the following additional information defined
for it:
- A specializer component, for use within a command implementation (defmethod).
  This determines what implementations of a command are applicable to a given
  call.
- An interactive component, for use within a command definition (defgeneric).
  This determines how a commands argument is to be obtained interactively.

NB: WE ARE ABANDONING THE IMPLEMENTATION OF EXTENDED SPECIALIZERS, AS THIS WOULD
REQUIRE MOVING AWAY FROM USING DEFGENERIC AND DEFMETHOD UNDER THE
HOOD. SPECIALIZERS PROVIDED TO DEFINE-COMMAND-IMPLEMENTATION, AND THUS THE
LAMBDA LIST FOR DEFINE-COMMAND-IMPLEMENTATION, MUST CONFORM TO DEFMETHOD AND
METHOD LAMBDA LISTS.

A specializer component is a symbol or a list denoting a type, function, or
class:
{symbol | ([keyword] symbol . arguments)}

An interactive component is a symbol or a list denoting a function or class:
{symbol | ([keyword] symbol . arguments)}

In both of the above the optional keyword may be one of :TYPE, :FUNCTION, or
:CLASS, for cases where a given symbol names multiple valid options. :TYPE may
only be provided to a specializer. If no keyword is provided then classes take
priority over functions take priority over types.

If an interactive type is a function, it is called with the command object as
its first argument, the current input method as its second argument, and any
provided arguments as its remaining arguments. If an interactive type is a
class, then the generic function read-interactive-argument is called with the
command object, the current input method, and the created instance of the class.

Abbreviations:
A   - argument name
S   - specializer component
I   - interactive component
D   - default value
DPP - default value provided

The form DEFINE-COMMAND takes arguments with their interactive specifiers, or
may take an :INTERACTIVE option which either is or defines a function to
interactively obtain the commands arguments. The arguments in the arguments list
must conform to the following shape:

Positional arguments:
{A | (A [I])}

Optional arguments:
{A | (A [I])}

Rest argument:
{A | (A [I])}

Key arguments:
{A | ({A | (keyword-name A)} [I])}

Auxillary arguments are not permitted in DEFINE-COMMAND arguments lists.

The full lambda list:
({A | (A [I])}*
 [&optional {A | (A [I])}*]
 [&rest {A | (A [I])}]
 [&key {A | ({A | (keyword-name A)} [I])}*]
 [&allow-other-keys])

If any of the positional arguments do not have an interactive component, then an
interactive function MUST be provided. If any of the optional or key arguments
do not have an interactive component, then an interactive function MAY be
provided. An interactive function takes three arguments: the command object, the
current input method, and the list of argument names that have not already been
acquired interactively. These argument names will be as provided in the DC forms
lambda list; DCI forms may use different names for these arguments. An
interactive function must return a list whose every element is two elements: the
argument name as it was given, and the value to give that argument.

There is a conflict between key and rest arguments when obtaining them
interactively. This is resolved by first obtaining all KEY arguments according
to their interactive component. If any key arguments do not have an interactive
component and the rest argument does, then after obtaining all key arguments
with interactive components the rest argument will be obtained. If the rest
argument does not have an interactive component, and either key arguments
without interactive components or &allow-other-keys is present, then an
interactive function will be called if one exists. Otherwise, remaining key
arguments or the rest argument will not be obtained.


NB: WE ARE ABANDONING THE IMPLEMENTATION OF EXTENDED SPECIALIZERS, AS THIS WOULD
REQUIRE MOVING AWAY FROM USING DEFGENERIC AND DEFMETHOD UNDER THE
HOOD. SPECIALIZERS PROVIDED TO DEFINE-COMMAND-IMPLEMENTATION, AND THUS THE
LAMBDA LIST FOR DEFINE-COMMAND-IMPLEMENTATION, MUST CONFORM TO DEFMETHOD AND
METHOD LAMBDA LISTS. THIS SECTION CAN BE SAFELY IGNORED. PLEASE SEE THE STANDARD
FOR INFORMATION ON METHOD LAMBDA LISTS.

The form DEFINE-COMMAND-IMPLEMENTATION takes arguments with specializers. These
are different from defgeneric specializers in that they may be any valid
type. Command dispatch *is* slow, because commands are expected to be input
interactively. If repeated programmatic calling of a command is desired, define
regular (generic) functions and call them from the command, and call those
functions programmatically instead. If one wishes to provide a default value to
an optional or key argument, then the specializer T must be provided.

Positional arguments:
{A | (A S)}

Optional arguments:
{A | (A S [D [DPP]])}

Rest argument:
A

Key arguments
{A | ({A | (keyword-name A)} S [D [DPP]])}

Auxillary arguments:
{A | (A D)}

The full lambda list:

({A | (A S)}*
 [&optional {A | (A S [D [DPP]])}*]
 [&rest A]
 [&key {A | ({A | (keyword-name A)} S [D [DPP]])}*]
 [&allow-other-keys]
 [&aux {A | (A D)}])

|#



;; (defun parse-arguments-for-define-command (arguments-list)
;;   "Given an list of arguments as provided to DEFINE-COMMAND, return the
;; arguments as processed for a defgeneric form and an interactive argument list as
;; multiple values."
;;   (let ((current-type '&positional)
;;         (defgeneric-list nil)
;;         (interactive-components nil)
;;         (rest-sentinel nil))
;;     (dolist (argument arguments-list (values (reverse defgeneric-list)
;;                                              (reverse interactive-components)))
;;       (if (member argument '(&optional &rest &key &allow-other-keys))
;;           (setf current-type argument
;;                 defgeneric-list (cons argument defgeneric-list))
;;           (ecase current-type
;;             ((&positional &optional &rest)
;;              (when (eql current-type '&rest)
;;                (if rest-sentinel
;;                    (error "Multiple &rest arguments provided")
;;                    (setf rest-sentinel t)))
;;              (multiple-value-bind (argname it is ia)
;;                  (parse-positional-optional-rest-argument-for-define-command
;;                   argument)
;;                (push argname defgeneric-list)
;;                (push (list argname it is ia) interactive-components)))
;;             ((&key)
;;              (multiple-value-bind (argkey argname it is ia)
;;                  (parse-key-argument-for-define-command argument)
;;                (push (list (list argkey argname)) defgeneric-list)
;;                (push (list argname it is ia) interactive-components)))
;;             ((&allow-other-keys)
;;              (error "&ALLOW-OTHER-KEYS must be the final symbol in~
;;                      an interactive lambda list")))))))

;; (defvar *database* (search-tree::make-database))

;; (define-command execute-extended-command
;;     ((command (:class database-completion
;;                :database '*database*))))

;; (search-tree::add-to-database *database* 'execute-extended-command
;;                               "kjør kommando"
;;                               "execute extended command")

;; (defmethod execute-extended-command ((command symbol))
;;   (call-command-interactively command input-methods::*default-input-method*))

;; (define-command my-command ((foo (:class attempt-parse
;;                                   :try '(parse-integer)))))

;; (search-tree::add-to-database *database* 'my-command
;;                               "skriv integer"
;;                               "write integer")

;; (defmethod my-command ((foo integer))
;;   (print foo))

;; ;; (defmacro define-command (name (sequence-var seat-var &rest other-arguments)
;; ;;                           &body body)
;; ;;   (destructuring-bind (name &key translation translations &allow-other-keys)
;; ;;       (if (listp name) name (list name))
;; ;;     `(progn
;; ;;        (defun ,name (,sequence-var ,seat-var
;; ;;                      ,@(parse-define-command-arguments other-arguments :defun))
;; ;;          ,@body)
;; ;;        (define-simple-command-invoker ,name
;; ;;            ,@(parse-define-command-arguments other-arguments :translator))
;; ;;        (register-canonical-object ,(string name) ',name)
;; ;;        ,@(when translation
;; ;;            `(register-command-translation *user-locale* ,translatin ',name))
;; ;;        ,@(when translations
;; ;;            (mapcar (lambda (s)
;; ;;                      `(register-command-translation ',(car s) ,(cadr s) ',name))
;; ;;                    translations)))))

;; ;; TODO [02:07 19.12.2025]: We need to track history for completing-read... how
;; ;; do we do that? Should we store it in the interactive component? But what if
;; ;; we want to share between interactive components? Ok so we make it class
;; ;; allocated. But what if we want to make it unique to that component on a one
;; ;; off case? ok so... we make a class allocated hash table and map commands to
;; ;; history lists? huh... no... thats not right... Look at emacs, the answer
;; ;; probably lies there.

;; (defclass interactive-component () ())

;; (defclass inter () ())

;; (defclass attempt-parse ()
;;   ((attempt-functions :initarg :try :accessor parsers)))

;; (defclass database-completion ()
;;   ((database :initarg :database :accessor database)))

;; (defmethod read-argument-interactively (cmd arg im inter &key &allow-other-keys)
;;   (let ((string (input-method:completing-read
;;                  im
;;                  (format nil "[~A] Enter argument ~A: "
;;                          (c2mop:generic-function-name cmd)
;;                          arg)
;;                  :completions inter
;;                  :require-match nil
;;                  :initial-input nil
;;                  :history nil)))))

;; (defmethod obtain-interactive-argument (command arg im (inter database-completion)
;;                                         &key &allow-other-keys)
;;   (declare (optimize (debug 3)))
;;   (let ((string
;;           (read-with-completions im
;;                                  (format nil "")))))
;;   (let ((string (read-interactive-argument command im inter
;;                                            :completion-strings
;;                                            (alexandria:hash-table-keys
;;                                             (search-tree::string->object
;;                                              (symbol-value
;;                                               (database inter)))))))
;;     (gethash string (search-tree::string->object (symbol-value
;;                                                   (database inter))))))

;; (defmethod obtain-interactive-argument (command im (inter attempt-parse)
;;                                         &key &allow-other-keys)
;;   (declare (optimize (debug 3)))
;;   (let ((string (read-interactive-argument command im inter)))
;;     (dolist (parser (parsers inter))
;;       (multiple-value-bind (res err)
;;           (ignore-errors (funcall parser string))
;;         (unless (typep err 'condition)
;;           (return-from obtain-interactive-argument res))))))

;; (defun obtain-argument-interactively (command
;;                                       input-method
;;                                       argname
;;                                       interactive-type
;;                                       interactive-symbol
;;                                       interactive-arguments)
;;   "Return the value obtained interactively for this argument. If the argument does
;; not have an iteractive type (i.e. the interactive component was missing) collect
;; a placeholder value. We use c2mop:generic-function-lambda-list to construct the
;; final call to the generic function. We return a list with the argname first so
;; we know how to continue parsing this commands invocation"
;;   (list argname
;;         (case interactive-type
;;           (:default
;;            (cond ((find-class interactive-symbol nil)
;;                   (obtain-interactive-argument command
;;                                                input-method
;;                                                (apply #'make-instance
;;                                                       interactive-symbol
;;                                                       interactive-arguments)))
;;                  ((fboundp interactive-symbol)
;;                   (apply interactive-symbol command input-method
;;                          interactive-arguments))
;;                  (t (error "Symbol ~S denotes neither a class nor a type"
;;                            interactive-symbol))))
;;           (:class
;;            (if (find-class interactive-symbol nil)
;;                (obtain-interactive-argument command
;;                                             input-method
;;                                             (apply #'make-instance
;;                                                    interactive-symbol
;;                                                    interactive-arguments))
;;                (error "No class named ~S" interactive-symbol)))
;;           (:function
;;            (if (fboundp interactive-symbol)
;;                (apply interactive-symbol command input-method
;;                       interactive-arguments)
;;                (error "Symbol ~S is not fbound" interactive-symbol)))
;;           (otherwise 'argument-interactive-placeholder))))

;; (defun call-command-with-argument-list (command arguments-list)
;;   (let* ((ll (c2mop:generic-function-lambda-list command))
;;          (keys (member '&key ll))
;;          (keys (loop for key in keys until (member key '(&aux &allow-other-keys))
;;                      collect key)))
;;     (multiple-value-bind (normals keyargs)
;;         (loop for arg in arguments-list
;;               for found = (find (car arg) keys
;;                                 :key (lambda (k) (if (atom k) k (cadar k)))
;;                                 :test #'eql)
;;               if found
;;                 collect (list found arg) into ks
;;               else
;;                 collect arg into ns
;;               finally (return (values ns ks)))
;;       (apply command
;;              (append (mapcar #'cadr normals)
;;                      (mapcan (lambda (k)
;;                                (let ((k (car k))
;;                                      (a (cadr k)))
;;                                  (list (if (consp k)
;;                                            (caar k)
;;                                            (intern (string k) :keyword))
;;                                        (cadr a))))
;;                              keyargs))))))

;; (defun compute-if-positionals-missing (command still-needed)
;;   (let ((ll (c2mop:generic-function-lambda-list command)))
;;     (loop for el in ll
;;           until (member el '(&optional &rest &key &allow-other-keys))
;;           when (find el still-needed)
;;             do (return-from compute-if-positionals-missing t))
;;     nil))

;; (defun call-command-interactively
;;     (command &optional (input-method input-methods:*default-input-method*))
;;   (let ((command (if (symbolp command) (symbol-function command) command)))
;;     (unless (typep command 'command)
;;       (cerror "Abort interactive call" "~A is not a command" command)
;;       (return-from call-command-interactively nil))
;;     (let* ((interactive-function (interactive-function command))
;;            (argument-list-preprocess
;;              (loop for (argname it is ia) in (interactive-components command)
;;                    collect (obtain-argument-interactively command input-method
;;                                                           argname it is ia)))
;;            (still-needed
;;              (loop for value in argument-list-preprocess
;;                    when (and (consp value)
;;                              (eql (cadr value) 'argument-interactive-placeholder))
;;                      collect (car value)))
;;            (missing-positionals
;;              (compute-if-positionals-missing command still-needed)))
;;       (cond ((and missing-positionals (null interactive-function))
;;              (error "Positional arguments missing and no interactive~
;;                    function given"))
;;             ((and still-needed interactive-function)
;;              (let ((obtained (funcall interactive-function
;;                                       command
;;                                       input-method
;;                                       still-needed)))
;;                (loop for o in obtained
;;                      for argname = (car o)
;;                      for value = (cadr o)
;;                      do (setf (cadr (find argname argument-list-preprocess
;;                                           :key #'car :test #'eql))
;;                               value)))))
;;       (call-command-with-argument-list command argument-list-preprocess))))

