
(in-package :eazy-documentation)

(defclass eazy-template (common-html.template:template)
  ((css-list :type list :initarg :css-list :initform *default-css*)
   (js-list  :type list :initarg :js-list  :initform *default-js*)
   (header :type string :initarg :header :initform "")
   (footer :type string :initarg :footer :initform "")))

(defmethod common-html.template:render ((template eazy-template)
                                        (document document)
                                        content-string)
  "The simplest template."
  ;; while we could create a complex directory structure, we don't.
  (with-slots (css-list js-list header footer) template
    (cl-who:with-html-output-to-string (s)
      ;; <!DOCTYPE html>
      (:html
       (:head
        (dolist (css css-list)
          (cl-who:htm
           (:link :rel "stylesheet" :type "text/css" :href css)))
        (dolist (js js-list)
          (cl-who:htm
           (:script :type "text/javascript" :src js)))
        (:title
         (cl-who:str (plump:encode-entities (title document)))))
       (:body
        (cl-who:str header)
        (cl-who:str content-string)
        (cl-who:str footer))))))

(defmethod common-html.template:render-section ((template eazy-template) (document document) (section section)
                           content-string)
  "The simplest section template."
  (with-slots (css-list js-list header footer) template
    (cl-who:with-html-output-to-string (s)
      ;; <!DOCTYPE html>
      (:html
       (:head
        (dolist (css css-list)
          (cl-who:htm
           (:link :rel "stylesheet" :type "text/css" :href css)))
        (dolist (js js-list)
          (cl-who:htm
           (:script :type "text/javascript" :src js)))
        (:title
         (cl-who:str
          (plump:encode-entities
           (common-doc.ops:collect-all-text (title section))))))
       (:body
        (cl-who:str header)
        (cl-who:str content-string)
        (cl-who:str footer))))))


