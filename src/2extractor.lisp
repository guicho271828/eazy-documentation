(in-package :eazy-documentation)

(defvar *definitions*)
(defvar *old-macroexpand-hook*)
(defvar *deferred-tasks*)

(defun add-definition (&rest initargs &key doctype name args docstring)
  (declare (ignore doctype name args docstring))
  (let ((obj (apply #'make-instance 'definition initargs)))
    (if-let ((it (find obj *definitions* :test #'definition=)))
      (progn
        (merge-slot obj it 'args)
        (merge-slot obj it 'docstring))
      (vector-push-extend obj *definitions* (max 1 (length *definitions*))))))

(defun call-with-extracting-document (fn)
  (let* ((*definitions* (make-array 128 :adjustable t :fill-pointer 0))
         (*deferred-tasks* nil)
         (*old-macroexpand-hook* *macroexpand-hook*)
         (*macroexpand-hook* 'expand-extracting-document))
    (funcall fn)))

(defun expand-extracting-document (expander form env)
  (handler-case (extract-document form)
    (error (c)
      (fresh-line *error-output*)
      (let ((*print-right-margin* 100)
            (*print-length* 4)
            (*print-pretty* t))
        (pprint-logical-block (*error-output* nil :per-line-prefix "; ")
          (format *error-output*
                  "~&Failed parsing ~a due to ~a" form (type-of c))))))
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

     (let ((lambda-list (sb-kernel:%fun-lambda-list (macro-function macro))))
       (labels ((rec (list)
                  (match list
                    ((list* '&whole _ rest)
                     (rec rest))
                    ((list* '&environment _ rest)
                     (rec rest))
                    ((cons else rest)
                     (cons else (rec rest))))))
         (setf lambda-list
               (rec lambda-list)))
       
       
       (match lambda-list
         ((list* _ (type list) '&body _ _)
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
             (setf (getf acc :docstring) docstring))))))
     
     (apply #'add-definition acc))))

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

(defun extract-document-from-system (system)
  (uiop:with-temporary-file (:pathname p)
    (call-with-extracting-document
     (lambda ()
       (with-compilation-unit ()
         (let ((*compile-print* nil)
               (*compile-verbose* nil))
           (let ((*package* (find-package :asdf)))
             (compile-file (asdf:system-source-file system) :output-file p))
           (asdf:clear-system system)
           (asdf:compile-system system :verbose nil :force t)))
       (when *deferred-tasks*
         (asdf:load-system system)
         (mapc #'funcall *deferred-tasks*))
       *definitions*))))
