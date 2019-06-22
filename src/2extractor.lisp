(in-package :eazy-documentation)

(defvar *defs*)
(defvar *old-macroexpand-hook*)
(defvar *deferred-tasks*)

(defun add-def (&rest initargs &key doctype name args docstring file)
  (declare (ignore doctype name args docstring file))
  (let ((obj (apply #'make-instance 'def initargs)))
    (if-let ((it (find obj *defs* :test #'def=)))
      (progn
        (merge-slot obj it 'args)
        (merge-slot obj it 'docstring))
      (vector-push-extend obj *defs* (max 1 (length *defs*))))))

(defun call-with-extracting-definitions (fn)
  (let* ((*defs* (make-array 128 :adjustable t :fill-pointer 0))
         (*deferred-tasks* nil)
         (*old-macroexpand-hook* *macroexpand-hook*)
         (*macroexpand-hook* 'expand-extracting-definitions))
    (funcall fn)))

(defun expand-extracting-definitions (expander form env)
  (handler-case (extract-definitions form)
    (error (c)
      (note "~&Failed parsing ~a due to ~a" form (type-of c))))
  (funcall *old-macroexpand-hook* expander form env))

(defun extract-definitions (form)
  (match form
    ((list* 'setf args)
     (parse-setf args))
    ((list* macro _)
     (when (macro-function macro)
       (when (eql 0 (search "DEF" (symbol-name macro)))
         (parse-def form))))))

(defun natural-language-string-p (string)
  "Heuristic decision to prune non-docstring"
  (and (stringp string)
       (let* ((words (ppcre:split " +" string))
              (count (iter (for word in words)
                           (counting
                            (find word
                                  ;; from http://www.cs.cmu.edu/~cburch/words/top.html
                                  ;; 483726.84 / 1000000 of words consume this
                                  `("the" "of" "and" "to" "a" "in" "is" "that" "was" "it" "for" "on" "with" "he" "be" "I"
                                          "by" "as" "at" "you" "are" "his" "had" "not" "this" "have" "from" "but" "which" "she"
                                          "they" "or" "an" "her" "were" "there" "we" "their" "been" "has" "will" "one" "all"
                                          "would" "can" "if" "who" "more" "when" "said" "do" "what" "about" "its" "so" "up"
                                          "into" "no" "him" "some" "could" "them" "only" "time" "out" "my" "two" "other"
                                          "then" "may" "over" "also" "new" "like" "these" "me" "after" "first" "your" "did"
                                          "now" "any" "people" "than" "should" "very" "most" "see" "where" "just" "made"
                                          "between" "back" "way" "many" "years" "being" "our" "how" "work"
                                          ;; lisp-specific keywords
                                          ,@(iter (for s in-package :cl external-only t)
                                                  (collecting (symbol-name s))))
                                  :test #'string-equal))))
              (result (and (< 2 (length words))
                           (<= 0.1
                               (/ count
                                  (max 1 (length words)))))))
         
         (unless result
           (note "String ~s does not look like a docstring." string))
         result)))


(defun remove-macro-lambda-list-keywords (lambda-list)
  "remove &whole and &env from a lambda list"
  (labels ((rec (list)
             (match list
               ((list* '&whole _ rest)
                (rec rest))
               ((list* '&environment _ rest)
                (rec rest))
               ((cons else rest)
                (cons else (rec rest))))))
    (rec lambda-list)))

(defun parse-def (form &aux acc)
  (match form
    ((list* macro rest)
     (setf (getf acc :doctype) macro)

     (let ((lambda-list
            (remove-macro-lambda-list-keywords
             (sb-kernel:%fun-lambda-list (macro-function macro)))))
       
       (match lambda-list
         ((list* _ _ '&body _ _)
          ;; defun, defmacro, deftype and others
          
          (match rest
            ((list* name-and-options args body)
             
             (ematch (ensure-list name-and-options)
               ((list 'setf (type symbol))
                (setf (getf acc :name) name-and-options))
               ((list* (and name (type symbol)) _)
                (setf (getf acc :name) name))
               ((list* (and name (type string)) _)
                (setf (getf acc :name) (intern (string-upcase name) :keyword))))

             (when (listp args)
               (setf (getf acc :args)
                     (remove-macro-lambda-list-keywords args)))

             (when-let ((it (find-if #'natural-language-string-p body)))
               (setf (getf acc :docstring) it))

             (multiple-value-bind (body decl docstring) (parse-body body :documentation t)
               (declare (ignore body decl))
               (when (natural-language-string-p docstring)
                 (setf (getf acc :docstring) docstring)))
             
             ;; defgeneric / defclass / defpackage-style documentation
             ;; defsystem
             (labels ((rec (list)
                        (match list
                          ((list* (or :description :documentation) docstring rest)
                           (when (natural-language-string-p docstring)
                             (setf (getf acc :docstring) docstring))
                           (rec rest))
                          ((list* _ rest)
                           (rec rest)))))
               (rec (flatten form))))))

         (_
          
          (match rest
            ((list* name-and-options rest2)
             
             (ematch (ensure-list name-and-options)
               ((list 'setf (type symbol))
                (setf (getf acc :name) name-and-options))
               ((list* (and name (type symbol)) _)
                (setf (getf acc :name) name))
               ((list* (and name (type string)) _)
                (setf (getf acc :name) (intern (string-upcase name) :keyword))))

             (when-let ((it (find-if #'natural-language-string-p rest2)))
               (setf (getf acc :docstring) it))))

          ;; defgeneric / defclass / defpackage-style documentation
          ;; defsystem
          (labels ((rec (list)
                     (match list
                       ((list* (or :description :documentation) docstring rest)
                        (when (natural-language-string-p docstring)
                          (setf (getf acc :docstring) docstring))
                        (rec rest))
                       ((list* _ rest)
                        (rec rest)))))
            (rec (flatten form))))))
     
     (apply #'add-def acc))))

(defun parse-setf (args)
  (match args
    (nil
     nil)
    ((list* (list 'documentation nameform typeform) _ rest)
     (when (and (constantp nameform) (constantp typeform))
       (let ((name (eval nameform))
             (type (eval typeform)))
         (push (lambda ()
                 (add-def :name name
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

