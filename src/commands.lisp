
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
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation "The command class. A subclass of generic functions."))

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
  (multiple-value-bind (func arg-list) (gather-args-interactively command
                                                                  :input-method input-method)
    (when func
      (call-command-with-argument-list func arg-list))))

(defun gather-args-interactively (command &key (input-method (input-method)))
  "Gather the arguments for COMMAND interactively"
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
                                              (etypecase is
                                                (function is)
                                                (symbol (symbol-function is)))))
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
              (values command argument-list))))
      (abort-command ()
        :report "Abort command"
        (values nil)))))

(defun call-command-with-argument-list (command arguments-list)
  "Invoke COMMAND with the arguments specified by ARGUMENTS-LIST. ARGUMENTS-LIST
is a plist with the key being the name of the argument and the value being
the value of the argument. It's best to use gather-args-interactively to
construct it."
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
  "Parse a user provided :INTERACTIVE option to define-command."
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
symbol, and interactive arguments as multiple values. For &KEY arguments only."
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
  "Bind OPTIONS list to their relevant options in an options list (an alist) for
the duration of BODY. Bind remaining options to REMAINING."
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

NAME will be the name of the generic function.

COMMAND-LAMBDA-LIST is a command lambda list. It contains argument names,
optionally with their interactive components. An interactive component is either
a symbol denoting a class or a function, or a list of such a symbol and its
arguments. If the list contains a keyword as its first element that keyword
determines if the interactive component names a function or a class. If no
keyword is provided then classes are preferred to functions when a class
exists. When constructing a class make-instance is applied to the symbol and its
provided arguments. When calling a function the function is applied to the
command, input method, argument name, and its provided arguments. The shape of a
command lambda list is as follows:

({A | (A [{symbol | ([keyword] symbol argument*)}])}*
 [&optional {A | (A [{symbol | ([keyword] symbol argument*)}])}*]
 [&rest {A | (A [{symbol | ([keyword] symbol argument*)}])}]
 [&key {A | ({A | (keyword-name A)} [{symbol | ([keyword] symbol argument*)}])}*]
 [&allow-other-keys])

BODY is the set of valid options for defgeneric, with the following additions:

:INTERACTIVE, which if provided must be a valid value to FUNCTION. If provided
and arguments still need to be obtained, this function is called with four
arguments: the command, the input method, and two alists mapping argument names
to their values. The first of these alists hold unobtained arguments, the second
holds obtained arguments. The entries of these alists - but not the alists
themselves - may be destructively modified to give arguments their values. The
argument names are the symbols provided in the command lambda list.

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
                               database
                               canonical-name
                               no-canonical-name)
                    body
        `(progn
           (defgeneric ,name ,defgen-ll
             (:generic-function-class command)
             ,@remaining)
           (setf (pass-optional-key #',name)
                 ,(cadr read-missing-nonpositionals-interactively)
                 (interactive-function #',name)
                 ,(parse-interactive (cdr interactive)))
           ,@(unless (cadr no-canonical-name)
               `((add-to-database ,(if database
                                       `(or ,(cadr database)
                                            *default-command-database*)
                                       '*default-command-database*)
                                  #',name
                                  nil
                                  (or (and ,(cadr canonical-name)
                                           (string ,(cadr canonical-name)))
                                      (string ',name)))))
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
