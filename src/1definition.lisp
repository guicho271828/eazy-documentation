(in-package :eazy-documentation)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass definition ()
  ((doctype :accessor doctype :initarg :doctype)
   (name :accessor name :initarg :name)
   (args :accessor args :initarg :args)
   (docstring :accessor docstring :initarg :docstring))
  (:documentation "Instances represententing a documentation entry."))
)

(defun definition= (a b)
  "Compare the name, doctype, docstring by EQ. Returns true when they look same.

Docstring are compared because the same function could be recognized multiple
times when a macro expands into another macro (e.g. defrule -> defun).
In such a case, the docstring objects are most likely the same object (by EQ)."
  (match* (a b)
    (((definition :doctype (place d1) :name (place n1) :docstring (place s1))
      (definition :doctype (place d2) :name (place n2) :docstring (place s2)))
     #+(or)
     (<= 2 (count t (vector (eq (ignore-errors n1)
                                (ignore-errors n2))
                            (eq (ignore-errors s1)
                                (ignore-errors s2))
                            (eq (ignore-errors d1)
                                (ignore-errors d2)))))
     (and (eq (ignore-errors n1)
              (ignore-errors n2))
          (or 
           (eq (ignore-errors s1)
               (ignore-errors s2))
           (eq (ignore-errors d1)
               (ignore-errors d2)))))))

(defun left (a b) (declare (ignore b)) a)

(defun merge-slot (from to slot &optional (fn #'left))
  (when (slot-boundp from slot)
    (if (slot-boundp to slot)
        (progn
          (simple-style-warning "overwriting ~a for ~a/~a" slot (doctype to) (name to))
          (setf (slot-value to slot) (funcall fn (slot-value to slot) (slot-value from slot))))
        (setf (slot-value to slot) (slot-value from slot)))))


