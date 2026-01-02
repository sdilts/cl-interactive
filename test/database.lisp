
(in-package :cl-interactive/test)

(fiveam:def-suite* database)

(defvar *database*)

(fiveam:test make-database
  (setf *database* (cl-interactive:make-database))
  (fiveam:is (typep *database* 'cl-interactive:database)))

(fiveam:test add-to-database
  (fiveam:is
   (eql 'hello
        (cl-interactive:add-to-database *database* 'hello "hola" "hello")))
  (fiveam:is
   (eql 'goodbye
        (cl-interactive:add-to-database *database* 'goodbye "adios" "goodbye"))))

(fiveam:test search-database
  (fiveam:is (member 'hello
                     (cl-interactive:search-in-database *database* "hello")))
  (fiveam:is (member 'hello
                     (cl-interactive:search-in-database *database* "hel"
                                                        :string-type :canonical
                                                        :partial t
                                                        :from-beginning t)))
  (fiveam:is (member 'hello
                     (cl-interactive:search-in-database *database* "hol"
                                                        :string-type :local
                                                        :partial t
                                                        :from-beginning t)))
  (fiveam:is (member 'hello
                     (cl-interactive:search-in-database *database* "ol"
                                                        :string-type :local
                                                        :partial t
                                                        :from-beginning nil)))
  (fiveam:is (member 'hello
                     (cl-interactive:search-in-database *database* "l"
                                                        :partial t
                                                        :from-beginning nil
                                                        :prefer :local)))
  (let ((s (cl-interactive:search-in-database *database* "o"
                                              :partial t
                                              :from-beginning nil
                                              :prefer :local)))
    (fiveam:is (member 'hello s))
    (fiveam:is (member 'goodbye s))))

;; (fiveam:test database-cleanup
;;   (print 'unbinding-database)
;;   (makunbound '*database*))
