
(in-package #:cl-interactive/database)

(defvar *database-string-type* nil
  "Must be either T, NIL, :LOCAL, :CANONCIAL, or (:PREFER {:LOCAL | :CANONICAL})")

(defclass database ()
  ((ltree :initarg :local-search-tree :accessor local-search-tree
          :documentation "A search tree mapping local strings to objects")
   (ctree :initarg :canonical-search-tree :accessor canonical-search-tree
          :documentation "A search tree mapping canonical strings to objects")
   (lhash :initarg :local-hash-table :accessor local-hash-table
          :documentation "A hash table mapping local strings to objects")
   (chash :initarg :canonical-hash-table :accessor canonical-hash-table
          :documentation "A hash table mapping canonical strings to objects")))

(defun make-database ()
  (make-instance 'database
                 :local-search-tree (make-instance 'search-tree)
                 :canonical-search-tree (make-instance 'search-tree)
                 :local-hash-table (make-hash-table :test #'equalp)
                 :canonical-hash-table (make-hash-table :test #'equalp)))

(defvar *default-command-database* (make-database))

(defgeneric map-database (function database &key collect &allow-other-keys)
  (:documentation "Map FUNCTION over DATABASE. Key arguments are specific to the
type of database being mapped over. If COLLECT is T then FUNCTION must return
two values, the value to collect, and a boolean indicating whether or not to
collect it."))

(defgeneric database-strings (database &optional type)
  (:documentation "Return all strings from DATABASE that are of type TYPE
(either :LOCAL, :CANONICAL, :BOTH, T, or NIL. :BOTH, T and NIL return all
strings)."))

(defgeneric search-in-database (database string
                                &key partial from-beginning default
                                &allow-other-keys)
  (:documentation "Search DATABASE for STRING.



 STRING may be a canonical string or
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

(defmethod map-database ((function function) (database database)
                         &key (which :hash) (string-type :both) (prefer :local)
                           (collect t) (remove-duplicates nil))
  "Map FUNCTION over DATABASE.

WHICH - one of :hash or :tree. When it is :hash FUNCTION must take two
        arguments, the key and the value. When it is :tree FUNCTION must take
        four arguments, the current character, the list of objects whose search
        string begins with the current path through the tree, the list of
        objects whose search string contains the current path through the tree,
        and a list of the subnodes of the current tree node.
STRING-TYPE - one of :both, :local, or :canonical. Controls which trees or hash
              tables to map over.
PREFER - one of :local or :canonical. Controls whether to prefer results coming
         from local or canonical strings. Only applicable when STRING-TYPE is
         :both.
COLLECT - when T, FUNCTION must return two values: the value to collect, and a
          boolean indicating whether or not to collect it.

REMOVE-DUPLICATES - must be either NIL or a function. When NIL, duplicates are
                    not removed from the resultant list. When provided, must be
                    a function that is a suitable test function for
                    remove-duplicates. Only applicable when COLLECT is T."
  (check-type which (member :hash :tree))
  (check-type string-type (or (member :both :local :canonical)))
  (check-type prefer (member :local :canonical))
  (let* ((%collection (cons nil nil))
         (collection %collection))
    (labels ((compute-tables ()
               (case string-type
                 (:both (case prefer
                          (:local (list (local-hash-table database)
                                        (canonical-hash-table database)))
                          (:canonical (list (canonical-hash-table database)
                                            (local-hash-table database)))))
                 (:local (list (local-hash-table database)))
                 (:canonical (list (canonical-hash-table database)))))
             (compute-trees ()
               (case string-type
                 (:both (case prefer
                          (:local (list (local-search-tree database)
                                        (canonical-search-tree database)))
                          (:canonical (list (canonical-search-tree database)
                                            (local-search-tree database)))))
                 (:local (list (local-search-tree database)))
                 (:canonical (list (canonical-search-tree database)))))
             (hash-collect (k v)
               (multiple-value-bind (r p)
                   (funcall function k v)
                 (when p
                   (setf (cdr collection) (cons r nil))
                   (setf collection (cdr collection)))))
             (tree-collect (el)
               (multiple-value-bind (r p)
                   (funcall function (car el) (caadr el) (cdadr el) (cddr el))
                 (when p
                   (setf (cdr collection) (cons r nil))
                   (setf collection (cdr collection)))))
             (tree-call (el)
               (funcall function (car el) (caadr el) (cdadr el) (cddr el)))
             (for-hash (tables)
               (loop for table in tables
                     do (maphash (if collect #'hash-collect function) table)))
             (for-tree (trees)
               (loop for tree in trees
                     do (map nil (if collect #'tree-collect #'tree-call) tree))))
      (case which
        (:hash (for-hash (compute-tables)))
        (:tree (for-tree (compute-trees)))))
    (if remove-duplicates
        (remove-duplicates (cdr %collection) :test remove-duplicates)
        (cdr %collection))))

(defmethod database-strings ((database database)
                             &optional (type *database-string-type*))
  (map-database (lambda (k v) (declare (ignore v)) (values k t))
                database
                :which :hash
                :string-type (if (or (null type) (eql type 't)) :both type)
                :collect t))

(defmethod search-in-database ((database database) (string string)
                               &key partial from-beginning default
                                 (string-type :both) (prefer :local)
                               &allow-other-keys)
  "Search DATABASE for STRING. STRING-TYPE controls whether to search for local
or canonical strings, or both. PREFER controls which to prefer when STRING-TYPE
is :both. If PARTIAL is T then search trees are searched, otherwise hash tables
are searched. If FROM-BEGINNING controls whether to search a search tree for
objects whose search strings begin with STRING or for objects whose search
string contains STRING. DEFAULT is the value to use if no object is
found. Returns a list of all matching objects."
  (remove-duplicates
   (if partial
       (case string-type
         (:both
          (case prefer
            (:local
             (append
              (search-in-search-tree (local-search-tree database) string
                                     :from-beginning from-beginning
                                     :default default)
              (search-in-search-tree (canonical-search-tree database) string
                                     :from-beginning from-beginning
                                     :default default)))
            (:canonical
             (append
              (search-in-search-tree (canonical-search-tree database) string
                                     :from-beginning from-beginning
                                     :default default)
              (search-in-search-tree (local-search-tree database) string
                                     :from-beginning from-beginning
                                     :default default)))))
         (:local
          (search-in-search-tree (local-search-tree database) string
                                 :from-beginning from-beginning
                                 :default default))
         (:canonical
          (search-in-search-tree (canonical-search-tree database) string
                                 :from-beginning from-beginning
                                 :default default)))
       (case string-type
         (:both
          (case prefer
            (:local
             (list (gethash string (local-hash-table database) default)
                   (gethash string (canonical-hash-table database) default)))
            (:canonical
             (list (gethash string (canonical-hash-table database) default)
                   (gethash string (local-hash-table database) default)))))
         (:local
          (list (gethash string (local-hash-table database) default)))
         (:canonical
          (list (gethash string (canonical-hash-table database) default)))))))

(defmethod add-to-database ((db database) object local-string canonical-string)
  (when local-string
    (add-string-to-tree (local-search-tree db) local-string object)
    (setf (gethash local-string (local-hash-table db)) object))
  (when canonical-string
    (add-string-to-tree (canonical-search-tree db) canonical-string object)
    (setf (gethash canonical-string (canonical-hash-table db)) object))
  object)


;; (defclass database ()
;;   ((tree :initarg :search-tree :accessor search-tree
;;          :documentation "A search tree mapping local strings to objects")
;;    (local&canonical->object
;;     :initarg :strings->object :accessor string->object
;;     :documentation "All strings (local and canonical) map to an object. Quicker
;; than searching in a search tree if you know the whole string")))

;; (defun make-database ()
;;   (make-instance 'database
;;                  :search-tree (make-instance 'search-tree)
;;                  :strings->object (make-hash-table :test #'equalp)))

;; (defvar *default-command-database* (make-database))

;; (defgeneric map-database (function database &key which stringtype collect)
;;   (:documentation "Map FUNCTION over DATABASE, selecting the database component
;; to map over with WHICH, the string type to restrict to with STRINGTYPE, and
;; whether or not to collect the output of FUNCTION with COLLECT. FUNCTION should
;; return two values, the first being the return value and the second being whether
;; or not to collect it."))

;; (defmethod map-database ((function function) (database database)
;;                          &key (which :hash) (stringtype :both) (collect t))
;;   "Map over a base DATABASE object. WHICH must be either :hash or :tree.
;; STRINGTYPE must be either :local, :canonical, or :both. FUNCTION must take two
;; arguments if WHICH is :hash (the key and the value), and must take four
;; arguments if WHICH is :TREE (the character of that tree node, the list of
;; elements that begin with the current tree as traversed so far, the list of
;; elements that contain the current tree as traversed so far, and the subnodes of
;; the tree)."
;;   (check-type which (member :hash :tree))
;;   (check-type stringtype (or (member :both :local :canonical)
;;                              (cons (eql :prefer) (cons (or (eql :canonical)
;;                                                            (eql :local))))))
;;   (let* ((%collection (cons nil nil))
;;          (collection %collection)
;;          (prefer (consp stringtype))
;;          (stringtype (if prefer (cadr stringtype) stringtype)))
;;     (declare (ignore prefer))
;;     (labels ((doit-hash (k v)
;;                (multiple-value-bind (res push)
;;                    (funcall function k (car v))
;;                  (when push
;;                    (setf (cdr collection) (cons res nil))
;;                    (setf collection (cdr collection)))))
;;              (doit-tree (el)
;;                (multiple-value-bind (res push)
;;                    (funcall function (car el) (caadr el) (cdadr el) (cddr el))
;;                  (when push
;;                    (setf (cdr collection) (cons res nil))
;;                    (setf collection (cdr collection)))))
;;              (hash-collector (k v)
;;                (flet ()
;;                  (case stringtype
;;                    (:both (doit-hash k v))
;;                    ((:local)
;;                     (when (eql (cdr v) 'local) (doit-hash k v)))
;;                    ((:canonical)
;;                     (when (eql (cdr v) 'canonical) (doit-hash k v))))))
;;              (tree-collector (el)
;;                (doit-tree el)
;;                (map nil #'tree-collector (cddr el)))
;;              (tree-walker (el)
;;                (funcall function (car el) (caadr el) (cdadr el) (cddr el))
;;                (map nil #'tree-walker (cddr el))))
;;       (if (eql which :hash)
;;           (maphash (if collect #'hash-collector function)
;;                    (string->object database))
;;           (map nil
;;                (if collect #'tree-collector #'tree-walker)
;;                (cl-interactive/search-tree::roots-list (search-tree database)))))
;;     (cdr %collection)))

;; (defgeneric database-strings (database &optional type)
;;   (:documentation "Return all strings from DATABASE that are of type TYPE
;; (either :LOCAL, :CANONICAL, :BOTH, T, or NIL. :BOTH, T and NIL return all
;; strings)."))

;; (defmethod database-strings ((database database)
;;                              &optional (type *database-string-type*))
;;   (map-database (lambda (k v) (declare (ignore v)) (values k t))
;;                 database
;;                 :which :hash
;;                 :stringtype (if (or (null type) (eql type 't)) :both type)
;;                 :collect t))

;; (defgeneric search-in-database (database string
;;                                 &key partial from-beginning default
;;                                 &allow-other-keys)
;;   (:documentation "Search DATABASE for STRING. STRING may be a canonical string or
;; a local string. Return two values, the object or objects found, and T if any
;; objects were found.

;; If PARTIAL is T, then STRING is a partial string, and will be searched for in
;; the search tree of DATABASE. A list will be returned if PARTIAL is T. If PARTIAL
;; is NIL then STRING denotes the entire string and is searched for in the hash
;; table of the database.

;; If FROM-BEGINNING is T then STRING will be searched for from the beginning. Only
;; relevant if PARTIAL is T.

;; Return DEFAULT if no objects were found to match STRING."))

;; (defgeneric add-to-database (database object local-string canonical-string)
;;   (:documentation "Add OBJECT to DATABASE, interned under LOCAL-STRING and
;; CANONICAL-STRING"))

;; (defmethod search-in-database ((database database) (string string)
;;                                &key partial from-beginning default
;;                                &allow-other-keys)
;;   (if partial
;;       (search-in-search-tree (search-tree database) string
;;                              :from-beginning from-beginning
;;                              :default default)
;;       (multiple-value-bind (o lc)
;;           (gethash string (string->object database) default)
;;         (declare (optimize (debug 3)))
;;         (if lc
;;             (values (car o) (cdr o))
;;             (values o lc)))))

;; (defmethod add-to-database ((db database) object local-string canonical-string)
;;   (when local-string
;;     (add-string-to-tree (search-tree db) local-string object)
;;     (setf (gethash local-string (string->object db)) (cons object 'local)))
;;   (when canonical-string
;;     (add-string-to-tree (search-tree db) canonical-string object)
;;     (setf (gethash canonical-string (string->object db))
;;           (cons object 'canonical)))
;;   object)
