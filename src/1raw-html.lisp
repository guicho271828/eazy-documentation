
(in-package :common-html.emitter)

(common-doc:define-node raw-text-node (text-node) ())

;; Overall, common-html is a huge pile of hack and its API is not thoroughly
;; thought out, leading to the need for this kind of junk code
(defmethod common-html.emitter::emit ((node raw-text-node))
  (let ((text (text node)))             ; originally called plump's HTML escape code
    (if (metadata node)
        (with-tag ("span" node) (write-string text *output-stream*))
        (write-string text *output-stream*))))

(export 'raw-text-node)


(in-package :eazy-documentation)

(defun convert-file-to-html-string (pin)
  (uiop:with-temporary-file (:pathname pout :type "html")
    (handler-case
        (progn
          (uiop:run-program
           (format nil "pandoc -o ~a ~a" pout pin)
           :output t
           :error-output t)
          (read-file-into-string pout))
      (uiop:subprocess-error ()
        (read-file-into-string pin)))))

(defun convert-string-to-html-string (string markup)
  (uiop:with-temporary-file (:pathname pin)
    (write-string-into-file string pin :if-exists :supersede)
    (uiop:with-temporary-file (:pathname pout :type "html")
      (handler-case
          (progn
            (uiop:run-program
             (format nil "pandoc -f ~a -o ~a ~a" markup pout pin)
             :output t
             :error-output t)
            (read-file-into-string pout))
        (uiop:subprocess-error ()
          string)))))

(defun convert-file-to-ascii-string (pin)
  (uiop:with-temporary-file (:pathname pout :type "txt")
    (handler-case
        (progn
          (uiop:run-program
           (format nil "pandoc -o ~a ~a" pout pin)
           :output t
           :error-output t)
          (read-file-into-string pout))
      (uiop:subprocess-error ()
        (read-file-into-string pin)))))

(defun convert-string-to-ascii-string (string markup)
  (uiop:with-temporary-file (:pathname pin)
    (write-string-into-file string pin :if-exists :supersede)
    (uiop:with-temporary-file (:pathname pout :type "txt")
      (handler-case
          (progn
            (uiop:run-program
             (format nil "pandoc -f ~a -o ~a ~a" markup pout pin)
             :output t
             :error-output t)
            (read-file-into-string pout))
        (uiop:subprocess-error ()
          string)))))

