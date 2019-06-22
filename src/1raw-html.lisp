
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
