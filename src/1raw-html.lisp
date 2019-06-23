
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

(defun convert-file-to-html-string (file)
  (uiop:with-temporary-file (:pathname p :type "html")
    (uiop:run-program
     (format nil "pandoc -o ~a ~a" p file))
    (read-file-into-string p)))

(defun convert-string-to-html-string (string markup)
  (uiop:with-temporary-file (:pathname p :type markup)
    (write-string-into-file string p :if-exists :supersede)
    (convert-file-to-html-string p)))

(defun convert-file-to-ascii-string (file)
  (uiop:with-temporary-file (:pathname p :type "txt")
    (uiop:run-program
     (format nil "pandoc -o ~a ~a" p file))
    (read-file-into-string p)))

(defun convert-string-to-ascii-string (string markup)
  (uiop:with-temporary-file (:pathname p :type markup)
    (write-string-into-file string p :if-exists :supersede)
    (convert-file-to-ascii-string p)))
