(in-package :eazy-documentation)

(defclass definition ()
  ((doctype :accessor doctype :initarg :doctype)
   (name :accessor name :initarg :name)
   (args :accessor args :initarg :args)
   (docstring :accessor docstring :initarg :docstring))
  (:documentation "Instances represententing a documentation entry."))

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

(defvar *definitions*)

(defun add-definition (&rest initargs &key doctype name args docstring)
  (declare (ignore doctype name args docstring))
  (let ((obj (apply #'make-instance 'definition initargs)))
    (if-let ((it (find obj *definitions* :test #'definition=)))
      (progn
        (merge-slot obj it 'args)
        (merge-slot obj it 'docstring))
      (vector-push-extend obj *definitions* (max 1 (length *definitions*))))))

;;;;

(defvar *old-macroexpand-hook*)
(defun call-with-extracting-document (fn)
  (let* ((*definitions* (make-array 128 :adjustable t :fill-pointer 0))
         (*deferred-tasks* nil)
         (*old-macroexpand-hook* *macroexpand-hook*)
         (*macroexpand-hook* 'expand-extracting-document))
    (funcall fn)))

(defun expand-extracting-document (expander form env)
  (handler-case (extract-document form)
    (error ()
      (let ((*print-length* 4))
        (format *error-output* "~%; failed parsing ~a" form))))
  (funcall *old-macroexpand-hook* expander form env))

(defun extract-document (form)
  (match form
    ((list* 'setf args)
     (parse-setf args))
    ((list* macro _)
     (when (macro-function macro)
       (when (eql 0 (search "DEF" (symbol-name macro)))
         (parse-def form))))))

(defun parse-def (form &aux acc)
  (match form
    ((list* macro rest)
     (setf (getf acc :doctype) macro)
     
     (match (sb-kernel:%fun-lambda-list (macro-function macro))
       ((list _ (type list) '&body _)
        ;; defun, defmacro, deftype and others
        
        (match rest
          ((list* name-and-options args body)
           
           (ematch (ensure-list name-and-options)
             ((list 'setf (type symbol))
              (setf (getf acc :name) name-and-options))
             ((list* (and name (type symbol)) options)
              (setf (getf acc :name) name)))

           (setf (getf acc :args) args)

           (multiple-value-bind (body decl docstring) (parse-body maybe-body :documentation t)
             (declare (ignore body decl))
             (when (stringp docstring)
               (setf (getf acc :docstring) docstring))))))

       (_
        
        (match rest
          ((list* name-and-options args body)
           
           (ematch (ensure-list name-and-options)
             ((list 'setf (type symbol))
              (setf (getf acc :name) name-and-options))
             ((list* (and name (type symbol)) options)
              (setf (getf acc :name) name)))))

        (when-let ((it (find-if #'stringp rest)))
          (setf (getf acc :docstring) it))

        ;; defgeneric / defclass / defpackage-style documentation
        (match (member :documentation (flatten form))
          ((list* :documentation (and docstring (type string)) _)
           (setf (getf acc :docstring) docstring)))))
     
     (apply #'add-definition acc))))

(defvar *deferred-tasks*)

(defun parse-setf (args)
  (match args
    (nil
     nil)
    ((list* (list 'documentation nameform typeform) _ rest)
     (when (and (constantp nameform) (constantp typeform))
       (let ((name (eval nameform))
             (type (eval typeform)))
         (push (lambda ()
                 (add-definition :name name
                                 :doctype (case type
                                            (function 'defun)
                                            (variable
                                             (if (constantp name)
                                                 'defconstant
                                                 'defvar))
                                            (structure 'defstruct)
                                            (type 'deftype)
                                            (method-combination 'define-method-combination)
                                            (setf 'defsetf)
                                            (compiler-macro 'define-compiler-macro))
                                 :docstring (documentation name type)))
               *deferred-tasks*)))
     (parse-setf rest))))



(defun extract-document-from-file (file)
  (uiop:with-temporary-file (:pathname p)
    (call-with-extracting-document
     (lambda ()
       (compile-file file :output-file p)
       (when *deferred-tasks*
         (load p)
         (mapc #'funcall *deferred-tasks*))
       *definitions*))))
