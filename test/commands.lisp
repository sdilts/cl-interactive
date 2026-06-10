(in-package :cl-interactive/test)

(fiveam:def-suite* commands)

;; Ya, we are testing internal functions here, but it makes
;; things so much easier:
(fiveam:test build-arg-list-with-keywords
  (let ((args '((TST . "Foo")))
        (ll '(&KEY ((:TST TST)))))
    (fiveam:is
     (equalp (cl-interactive/command::%build-arg-list
              ll args)
             '(:TST "Foo")))))

(fiveam:test build-arg-list-with-positionals
  (let ((args '((pos1 . "Foo") (pos2 . "bar")))
        (ll '(pos1 pos2)))
    (fiveam:is
     (equalp (cl-interactive/command::%build-arg-list
              ll args)
             '("Foo" "bar")))))

(fiveam:test build-arg-list-with-optionals
  (let ((args '((opt1 . "Foo")))
        (ll '(&optional opt1)))
    (fiveam:is
     (equalp (cl-interactive/command::%build-arg-list
              ll args)
             '("Foo")))))

(fiveam:test build-arg-list-with-all-types
  (let ((args '((pos1 . "pos1") (pos2 . "pos2") (opt1 . "Foo") (key1 . "key")))
        (ll '(pos1 pos2 &optional opt1 &key key1)))
    (fiveam:is
     (equalp (cl-interactive/command::%build-arg-list
              ll args)
             '("pos1" "pos2" "foo" :key1 "key")))))

(cl-interactive:define-command no-interactive-positionals (one two)
  (:method (one two)
    (format t "one: ~S two: ~S" one two)))

(defclass dummy-input-method (cl-interactive:input-method) ()
  (:documentation "Input method that uses foot and fzf for interactive input"))

(defmacro with-input-method (class-name &body body)
  `(let ((cl-interactive:*default-input-method* (make-instance ,class-name)))
    ,@body))

(fiveam:test call-command-interactively-missing-non-interactive-1
  (with-input-method 'dummy-input-method
    (fiveam:signals cl-interactive:missing-required-arguments-error
      (cl-interactive:call-command-interactively #'no-interactive-positionals
                                                 :already-gathered `((one . "one"))))))

(fiveam:test call-command-interactively-missing-non-interactive-2
  (with-input-method 'dummy-input-method
    (fiveam:signals cl-interactive:missing-required-arguments-error
      (cl-interactive:call-command-interactively #'no-interactive-positionals))))

(fiveam:test gather-args-interactively-missing-non-interactive
  (with-input-method 'dummy-input-method
    (fiveam:signals cl-interactive:missing-required-arguments-error
      (cl-interactive:gather-args-interactively #'no-interactive-positionals))))

(fiveam:test call-command-interactively-provided-non-interactive
  (with-input-method 'dummy-input-method
    (let ((gathered `((one . "one") (two ."two"))))
      (fiveam:signals cl-interactive:missing-required-arguments-error
        (cl-interactive:call-command-interactively #'no-interactive-positionals
                                                   :already-gathered gathered)))))
