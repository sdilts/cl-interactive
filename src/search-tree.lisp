
(in-package #:cl-interactive/search-tree)

(deftype search-tree-node ()
  "A tree node has the shape
(char (begins . contains) &rest subtrees)"
  `(cons character      ; character at this node
         (cons (cons t  ; begins-with
                     t) ; contains-with
               t)))     ; subnodes

(defmacro with-search-tree-node-slots ((char begin contain subnodes) node
                                       &body body)
  "Bind CHAR BEGIN CONTAIN and SUBNODES to their relevant parts of the list NODE
using symbol-macrolet for the duration of BODY."
  (let ((n (gensym "NODE")))
    `(let ((,n ,node))
       (declare (ignorable ,n))
       (symbol-macrolet ((,char (car ,n))
                         (,begin (caadr ,n))
                         (,contain (cdadr ,n))
                         (,subnodes (cddr ,n)))
         ,@body))))

(defun make-search-tree-node (char value &key beginning? (validate t))
  (let ((node (list char
                    (cons (when beginning? (list value))
                          (unless beginning? (list value))))))
    (when validate
      (check-type node search-tree-node))
    node))

(defun make-search-tree-subtree-from-remaining (chars value &key beginning?)
  (let ((node (make-search-tree-node (car chars) value :beginning? beginning?)))
    (when (cdr chars)
      (with-search-tree-node-slots (ch b c s) node
        (setf s (list (make-search-tree-subtree-from-remaining
                       (cdr chars) value :beginning? beginning?)))))
    node))

(defun add-value-to-search-tree-node (node value &key beginning?)
  (with-search-tree-node-slots (char b c sub) node
    (if beginning?
        (push value b)
        (push value c))))

(defun recurse-and-add-to-search-tree (node charbag value &key beginning?)
  (with-search-tree-node-slots (ch b c s) node
    (if beginning?
        (pushnew value b)
        (pushnew value c))
    (let ((found (when charbag
                   (find (car charbag) s :test #'char-equal :key #'car))))
      (cond (found
             (recurse-and-add-to-search-tree found (cdr charbag) value
                                             :beginning? beginning?))
            ((not (null charbag))
             (push (make-search-tree-subtree-from-remaining
                    charbag value :beginning? beginning?)
                   s))))))

(defclass search-tree ()
  ((roots-list :initform nil :accessor roots-list)))

(defun add-string-to-tree (tree string store-value)
  "Add STORE-VALUE to TREE interned under STRING"
  (add-character-list-to-tree tree (coerce string 'list) store-value))

(defun add-character-list-to-tree (tree chars store-value)
  ;; Intern every char in charlist at the top level, as well as
  ;; recursively. This allows partial searches
  (loop for c on chars
        for beg = t then nil
        for found = (find (car c) (roots-list tree)
                          :test #'char-equal
                          :key #'car)
        do (cond ((and (null (cdr c)) (null found))
                  ;; If theres no more characters in string and the char is not
                  ;; found in the root, make a new node and push it.
                  (push (make-search-tree-node (car c) store-value
                                               :beginning? beg)
                        (roots-list tree)))
                 ((null (cdr c))
                  ;; when theres no more characters but we do have a root node,
                  ;; add to that node.
                  (add-value-to-search-tree-node found store-value
                                                 :beginning? beg))
                 (found
                  (recurse-and-add-to-search-tree found (cdr c) store-value
                                                  :beginning? beg))
                 (t
                  (push (make-search-tree-subtree-from-remaining c store-value
                                                                 :beginning? beg)
                        (roots-list tree))))))

(defun search-in-search-tree (tree string &key from-beginning default)
  (search-for-charlist-in-search-tree tree (coerce string 'list)
                                      :from-beginning from-beginning
                                      :default default))

(defun search-for-charlist-in-search-tree (tree chars &key from-beginning
                                                        default)
  (let ((found (find (car chars) (roots-list tree) :test #'char-equal
                                                   :key #'car)))
    (if found
        (search-nodes-recursively found (cdr chars) from-beginning default)
        default)))

(defun search-nodes-recursively (node chars from-beginning default)
  (with-search-tree-node-slots (ch b c s) node
    (if chars
        (let ((n (find (car chars) s :test #'char-equal
                                     :key #'car)))
          (if n
              (search-nodes-recursively n (cdr chars) from-beginning default)
              default))
        (values (if from-beginning
                    b
                    (append b c))
                t))))
