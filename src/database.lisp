
(in-package #:cl-interactive/database)

(defvar *database-string-type* nil
  "Must be either T, NIL, :LOCAL or :CANONCIAL")

(defclass database ()
  ((tree :initarg :search-tree :accessor search-tree
         :documentation "A search tree mapping local strings to objects")
   (local&canonical->object
    :initarg :strings->object :accessor string->object
    :documentation "All strings (local and canonical) map to an object. Quicker
than searching in a search tree if you know the whole string")))

(defun make-database ()
  (make-instance 'database
                 :search-tree (make-instance 'search-tree)
                 :strings->object (make-hash-table :test #'equalp)))

(defvar *default-command-database* (make-database))

(defgeneric map-database (function database &key which stringtype collect)
  (:documentation "Map FUNCTION over DATABASE, selecting the database component
to map over with WHICH, the string type to restrict to with STRINGTYPE, and
whether or not to collect the output of FUNCTION with COLLECT. FUNCTION should
return two values, the first being the return value and the second being whether
or not to collect it."))

(defmethod map-database ((function function) (database database)
                         &key (which :hash) (stringtype :both) (collect t))
  "Map over a base DATABASE object. WHICH must be either :hash or :tree.
STRINGTYPE must be either :local, :canonical, or :both. FUNCTION must take two
arguments if WHICH is :hash (the key and the value), and must take four
arguments if WHICH is :TREE (the character of that tree node, the list of
elements that begin with the current tree as traversed so far, the list of
elements that contain the current tree as traversed so far, and the subnodes of
the tree)."
  (check-type which (member :hash :tree))
  (check-type stringtype (member :both :local :canonical))
  (let* ((%collection (cons nil nil))
         (collection %collection))
    (labels ((doit-hash (k v)
               (multiple-value-bind (res push)
                   (funcall function k (car v))
                 (when push
                   (setf (cdr collection) (cons res nil))
                   (setf collection (cdr collection)))))
             (doit-tree (el)
               (multiple-value-bind (res push)
                   (funcall function (car el) (caadr el) (cdadr el) (cddr el))
                 (when push
                   (setf (cdr collection) (cons res nil))
                   (setf collection (cdr collection)))))
             (hash-collector (k v)
               (flet ()
                 (case stringtype
                   (:both (doit-hash k v))
                   ((:local)
                    (when (eql (cdr v) 'local) (doit-hash k v)))
                   ((:canonical)
                    (when (eql (cdr v) 'canonical) (doit-hash k v))))))
             (tree-collector (el)
               (doit-tree el)
               (map nil #'tree-collector (cddr el)))
             (tree-walker (el)
               (funcall function (car el) (caadr el) (cdadr el) (cddr el))
               (map nil #'tree-walker (cddr el))))
      (if (eql which :hash)
          (maphash (if collect #'hash-collector function)
                   (string->object database))
          (map nil
               (if collect #'tree-collector #'tree-walker)
               (cl-interactive/search-tree::roots-list (search-tree database)))))
    (cdr %collection)))

(defgeneric database-strings (database &optional type)
  (:documentation "Return all strings from DATABASE that are of type TYPE
(either :LOCAL, :CANONICAL, :BOTH, T, or NIL. :BOTH, T and NIL return all
strings)."))

(defmethod database-strings ((database database)
                             &optional (type *database-string-type*))
  (map-database (lambda (k v) (declare (ignore v)) (values k t))
                database
                :which :hash
                :stringtype (if (or (null type) (eql type 't)) :both type)
                :collect t))

(defgeneric search-in-database (database string
                                &key partial from-beginning default
                                &allow-other-keys)
  (:documentation "Search DATABASE for STRING. STRING may be a canonical string or
a local string. Return two values, the object or objects found, and T if any
objects were found.

If PARTIAL is T, then STRING is a partial string, and will be searched for in
the search tree of DATABASE. A list will be returned if PARTIAL is T. If PARTIAL
is NIL then STRING denotes the entire string and is searched for in the hash
table of the database.

If FROM-BEGINNING is T then STRING will be searched for from the beginning. Only
relevant if PARTIAL is T.

Return DEFAULT if no objects were found to match STRING."))

(defgeneric add-to-database (database object local-string canonical-string)
  (:documentation "Add OBJECT to DATABASE, interned under LOCAL-STRING and
CANONICAL-STRING"))

(defmethod search-in-database ((database database) (string string)
                               &key partial from-beginning default
                               &allow-other-keys)
  (if partial
      (search-in-search-tree (search-tree database) string
                             :from-beginning from-beginning
                             :default default)
      (multiple-value-bind (o lc)
          (gethash string (string->object database) default)
        (declare (optimize (debug 3)))
        (if lc
            (values (car o) (cdr o))
            (values o lc)))))

(defmethod add-to-database ((db database) object local-string canonical-string)
  (when local-string
    (add-string-to-tree (search-tree db) local-string object)
    (setf (gethash local-string (string->object db)) (cons object 'local)))
  (when canonical-string
    (add-string-to-tree (search-tree db) canonical-string object)
    (setf (gethash canonical-string (string->object db))
          (cons object 'canonical)))
  object)
