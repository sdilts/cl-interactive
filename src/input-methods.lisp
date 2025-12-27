
(in-package #:cl-interactive/input-method)

(defvar *default-input-method* nil
  "The default input method to fall back on")

(defvar *current-input-method* nil
  "The current input-method")

(defclass input-method () ()
  (:documentation "The root input method class"))

(defun input-method (&optional input-method)
  "Return an appropriate input method, or signal an error. If INPUT-METHOD is
non-nil it is used, otherwise if the *CURRENT-INPUT-METHOD* is non-nil it is
used, otherwise if the *DEFAULT-INPUT-METHOD* is non-nil it is used. Otherwise
an error is signalled. "
  (let ((input-method
          (or input-method *current-input-method* *default-input-method*
              (restart-case (error "No input method")
                (make-input-method (sym &rest args)
                  :report "Provide a symbol denoting an input method to use"
                  :interactive (lambda ()
                                 (format *query-io* "Enter input method class")
                                 (force-output *query-io*)
                                 (list (read *query-io*)))
                  (apply #'make-instance sym args))))))
    (check-type input-method input-method)
    input-method))

(defun interactive-error-handler-for-input-method (&optional input-method)
  "Handle errors interactively using an input method. If INPUT-METHOD is a
function it will be called in the handler and must return either NIL or an input
method object."
  (flet ((handler (c)
           (handler-bind
               ((error
                  (interactive-error-handler-for-input-method input-method)))
             (let ((rs (compute-restarts c))
                   (im (input-method (if (functionp input-method)
                                         (funcall input-method c)
                                         input-method))))
               (when (and rs im)
                 (flet ((rn (r) (format nil "~A" r)))
                   (let ((r (completing-read im (format nil "~A" c)
                                             :completions (mapcar #'rn rs))))
                     (when r
                       (let ((r (find r rs :key #'rn :test #'string=)))
                         (when r
                           (invoke-restart-interactively r)))))))))))
    (declare (special *nested*))
    (if (boundp '*nested*)
        #'handler
        (lambda (c)
          (let ((*nested* t))
            (declare (special *nested*))
            (restart-case (handler c)
              (abort-interactive-error-handler ()
                :report "Abort from interactive error handler"
                nil)))))))

(defgeneric prepare-completions-for-input-method (input-method completions)
  (:documentation
   "Given a set of completions, prepare them for the input method.

When implementing a new input method, at least one method for this generic
function should be defined which specializes on the input method and a database
object, so that execute-extended-command can complete for command names.

If there is no applicable method, signals an UNPREPARED-COMPLETIONS-ERROR.")
  (:method (input-method (completions null))
    nil))

(defmethod no-applicable-method ((gf (eql #'prepare-completions-for-input-method))
                                 &rest args)
  (restart-case (error 'unprepared-completions-error
                       :input-method (car args)
                       :completions (cadr args))
    (use-no-completions ()
      :report "Ignore completions for this call"
      nil)
    (use-completions-as-is ()
      :report "Use the completions as-is"
      (cadr args))))

(defun prepare-completions (completions &optional (input-method (input-method)))
  "A simpler interface to PREPARE-COMPLETIONS-FOR-INPUT-METHOD"
  (with-simple-restart (use-completions "Use completions anyway")
    (prepare-completions-for-input-method input-method completions)))

(defgeneric input-method-read (input-method prompt
                               &key completions require-match initial-input
                                 history
                               &allow-other-keys)
  (:documentation "Read something using an input method. Completions will be the
result of calling prepare-completions-for-input-method, and may be nil."))

(defun completing-read (input-method prompt
                        &rest keys
                        &key completions require-match initial-input history
                        &allow-other-keys)
  "Read input from the user with completions. Processes COMPLETIONS using
PREPARE-COMPLETIONS-FOR-INPUT-METHOD, and calls INPUT-METHOD-READ with the
resulting completions and other keys."
  (declare (ignore require-match initial-input history))
  (let ((comp (prepare-completions-for-input-method input-method completions)))
    (remf keys :completions)
    (apply #'input-method-read input-method prompt :completions comp keys)))

(defun read-string (input-method prompt &rest keys &key initial-input history)
  "Read a string from the user. Like COMPLETING-READ but only accepts the keys
INITIAL-INPUT and HISTORY."
  (declare (ignore initial-input history))
  (apply #'input-method-read input-method prompt keys))
