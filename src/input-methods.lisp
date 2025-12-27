
(in-package #:cl-interactive/input-method)

(defvar *default-input-method* nil
  "The default input method to fall back on")

(defvar *current-input-method* nil
  "The current input-method")

(defclass input-method () ())

(defun input-method (&optional input-method)
  "Return the relevant input method."
  (let ((input-method
          (or input-method *current-input-method* *default-input-method*)))
    (check-type input-method input-method)
    input-method))

(defun interactive-error-handler-for-input-method (&optional input-method)
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
  "Read input from the user with completions"
  (declare (ignore require-match initial-input history))
  (let ((comp (prepare-completions-for-input-method input-method completions)))
    (remf keys :completions)
    (apply #'input-method-read input-method prompt :completions comp keys)))

(defun read-string (input-method prompt &rest keys &key initial-input history)
  "Read a string from the user"
  (declare (ignore initial-input history))
  (apply #'input-method-read input-method prompt keys))

(defgeneric read-with-completions (input-method prompt completions
                                   &key require-match
                                     initial-input history
                                   &allow-other-keys)
  (:documentation "Read input from the user using INPUT-METHOD."))

(defun call-with-input-method-error-handling (handle-it input-method fn)
  (case handle-it
    ((:interactive)
     (handler-bind ((cl-interactive-error
                      (interactive-error-handler-for-input-method
                       (input-method input-method))))
       (funcall fn)))
    ((:errors t)
     (handler-bind ((error
                      (interactive-error-handler-for-input-method
                       (input-method input-method))))
       (funcall fn)))
    (otherwise (funcall fn))))

(defmacro with-input-method-error-handling ((key input-method) &body body)
  (let ((fn (gensym)))
    `(flet ((,fn () ,@body))
       (call-with-input-method-error-handling ,key ,input-method #',fn))))
