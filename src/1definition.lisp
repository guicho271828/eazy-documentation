(in-package :eazy-documentation)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defclass definition ()
  ((doctype   :accessor   doctype :initarg :doctype   :type symbol)
   (name      :accessor      name :initarg :name      :type symbol)
   (args      :accessor      args :initarg :args      :type list)
   (docstring :accessor docstring :initarg :docstring :type string)
   (file      :accessor      file :initarg :file      :type pathname
              :initform (or *compile-file-pathname* *load-pathname*)))
  (:documentation "Instances represententing a documentation entry."))
)

(defun definition= (a b)
  "Compare the name, doctype, docstring by EQ. Returns true when they look same.

Docstring are compared because the same function could be recognized multiple
times when a macro expands into another macro (e.g. defrule -> defun).
In such a case, the docstring objects are most likely the same object (by EQ)."
  (match* (a b)
    (((definition :doctype (place d1) :name (place n1) :docstring (place s1) :file f1)
      (definition :doctype (place d2) :name (place n2) :docstring (place s2) :file f2))
     (let* ((name    (eq n1 n2))
            (doctype (eq d1 d2))
            (file    (equal f1 f2))
            (s1 (ignore-errors s1))
            (s2 (ignore-errors s2))
            (string (equal s1 s2)))
       (and file
            (or (and name (or (not s1) (not s2) string))
                (and doctype s1 s2 string)
                (and name doctype string)))

       ;; wrong attempts
       #+(or)
       (<= 2 (count t (vector 
                       (set-equal (ignore-errors s1)
                                  (ignore-errors s2))
                       (equal (ignore-errors d1)
                              (ignore-errors d2)))))
       #+(or)
       (and (eq (ignore-errors n1)
                (ignore-errors n2))
            (or 
             (eq (ignore-errors s1)
                 (ignore-errors s2))
             (eq (ignore-errors d1)
                 (ignore-errors d2))))))))

(defun left (a b) (declare (ignore b)) a)

(defun merge-slot (from to slot &optional (fn #'left))
  (when (slot-boundp from slot)
    (if (slot-boundp to slot)
        (progn
          (fresh-line *error-output*)
          (let ((*print-right-margin* 100)
                (*print-pretty* t)
                (*print-length* 100))
            (pprint-logical-block (*error-output* nil :per-line-prefix "; ")
              (format *error-output*
                      "~&Overwriting ~a for (~a ~a ...) :~:_ ~a~:_ ->~:_ ~a" slot (doctype to) (name to)
                      (ignore-errors (slot-value to slot))
                      (ignore-errors (slot-value from slot)))))
          (setf (slot-value to slot) (funcall fn (slot-value to slot) (slot-value from slot))))
        (setf (slot-value to slot) (slot-value from slot)))))


