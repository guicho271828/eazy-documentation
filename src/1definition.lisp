(in-package :eazy-documentation)

(deftype name ()
  `(or symbol
       (cons (eql setf) (cons symbol null))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass def ()
    ((doctype   :accessor   doctype :initarg :doctype   :type symbol)
     (name      :accessor      name :initarg :name      :type name)
     (args      :accessor      args :initarg :args      :type list)
     (docstring :accessor docstring :initarg :docstring :type string)
     (file      :accessor      file :initarg :file      :type pathname
                :initform (or *compile-file-pathname* *load-pathname*)))
    (:documentation "Instances represententing a documentation entry."))

  (defun safe-name (def)
    (match (name def)
      ((list 'setf name)
       name)
      (name
       name))))

(defmethod print-object ((def def) s)
  (print-unreadable-object (def s)
    (ematch def
      ((def :doctype d1 :name n1 :args (place a1))
       (format s "~@{~s ~a~^ ~}" :doctype d1 :name n1)
       (when (slot-boundp def 'args)
         (format s " ~@{~s~^ ~}" :args a1))))))

;; WARNING: def= must imply def~, and def~ must imply def~doc .

(defun def= (a b)
  "Compare the name and the doctype. Returns true when they are both EQ."
  (ematch* (a b)
    (((def :doctype d1 :safe-name n1 :args (place a1))
      (def :doctype d2 :safe-name n2 :args (place a2)))
     (and (eq d1 d2)
          (eq n1 n2)
          (equal (ignore-errors a1) (ignore-errors a2))))))

(defun def~ (a b)
  "Compare the name (and its package), doctype, file, args, docstring.
 Returns true when they look same according to a heuristic rule."
  (ematch* (a b)
    (((def :doctype d1 :safe-name n1 :file f1 :docstring (place s1) :args (place a1))
      (def :doctype d2 :safe-name n2 :file f2 :docstring (place s2) :args (place a2)))
     (let ((name    (eq n1 n2))
           (doctype (eq d1 d2))
           (file    (equal f1 f2))
           (a1 (slot-boundp a 'args))
           (a2 (slot-boundp b 'args))   ;note: this is not let*
           (args (equal (ignore-errors a1) (ignore-errors a2)))
           (string (equal (ignore-errors s1) (ignore-errors s2))))
       (when (and file string)
         ;; (break "~@{~a ~}" a b)
         (cond
           ;; different name, but same docstring, args, package
           ((and doctype
                 (eq (symbol-package n1) ; same package
                     (symbol-package n2))
                 (or (and a1 a2 args)
                     (and (not a1) (not a2))))
            (values t :same-doctype))
           ;; different doctype, but same docstring, args
           ((and name
                 (or (and a1 a2 args)
                     (and (not a1) (not a2))))
            (values t :same-name))))))))

(defun def~doc (a b)
  "Compare the file and docstring.
 Returns true when they are both missing or EQUAL."
  (ematch* (a b)
    (((def :docstring (place s1) :file f1)
      (def :docstring (place s2) :file f2))
     (let ((file (equal f1 f2))
           (s1 (slot-boundp a 'docstring))
           (s2 (slot-boundp b 'docstring))
           (string (equal (ignore-errors s1) (ignore-errors s2))))
       (when file
         (cond
           ;; different name, but same docstring, args, package
           ((and s1 s2 string)
            (values t :same-docstring))
           ((and (not s1) (not s2))
            (values t :missing-docstring))))))))

(defun left (a b) (declare (ignore b)) a)

(defun merge-slot (from to slot &optional (fn #'left))
  (when (slot-boundp from slot)
    (if (slot-boundp to slot)
        (progn
          (note "~&Overwriting ~a for (~a ~a ...) :~:_ ~a~:_ ->~:_ ~a" slot (doctype to) (name to)
                (ignore-errors (slot-value to slot))
                (ignore-errors (slot-value from slot)))
          (setf (slot-value to slot) (funcall fn (slot-value to slot) (slot-value from slot))))
        (setf (slot-value to slot) (slot-value from slot)))))


