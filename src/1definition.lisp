(in-package :eazy-documentation)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass def ()
    ((doctype   :accessor   doctype :initarg :doctype   :type symbol)
     (name      :accessor      name :initarg :name      :type symbol)
     (args      :accessor      args :initarg :args      :type list)
     (docstring :accessor docstring :initarg :docstring :type string)
     (file      :accessor      file :initarg :file      :type pathname
                :initform (or *compile-file-pathname* *load-pathname*)))
    (:documentation "Instances represententing a documentation entry."))
  )

(defmethod print-object ((def def) s)
  (print-unreadable-object (def s)
    (ematch def
      ((def :doctype d1 :name n1 :args (place a1))
       (format s "~@{~s ~a~^ ~}" :doctype d1 :name n1)
       (when (slot-boundp def 'args)
         (format s "~@{~s~^ ~}" :args a1))))))

(defun def= (a b)
  "Compare the name and the doctype. Returns true when they are all EQ."
  (ematch* (a b)
    (((def :doctype d1 :name n1)
      (def :doctype d2 :name n2))
     (and (eq d1 d2)
          (eq n1 n2)))))

(defun def~ (a b)
  "Compare the name, doctype, docstring by EQ.
 Returns true when they look same according to a heuristic rule."
  (ematch* (a b)
    (((def :doctype d1 :name n1 :file f1 :docstring (place s1) :args (place a1))
      (def :doctype d2 :name n2 :file f2 :docstring (place s2) :args (place a2)))
     (let ((name    (eq n1 n2))
           (doctype (eq d1 d2))
           (file    (equal f1 f2))
           (a1 (slot-boundp a 'args))
           (a2 (slot-boundp b 'args))   ;note: this is not let*
           (args (equal (ignore-errors a1) (ignore-errors a2)))
           (s1 (slot-boundp a 'docstring))
           (s2 (slot-boundp b 'docstring))
           (string (equal (ignore-errors s1) (ignore-errors s2))))
       (when file
         ;; (break "~@{~a ~}" a b)
         (cond
           ;; different name, but same docstring, args, package
           ((and doctype
                 (eq (symbol-package n1) ; same package
                     (symbol-package n2))
                 (or (and s1 s2 string)
                     (and (not s1) (not s2)))
                 (or (and a1 a2 args)
                     (and (not a1) (not a2))))
            (values t :same-doctype))
           ;; different doctype, but same docstring, args
           ((and name
                 (or (and s1 s2 string)
                     (and (not s1) (not s2)))
                 (or (and a1 a2 args)
                     (and (not a1) (not a2))))
            (values t :same-name))))))))

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


