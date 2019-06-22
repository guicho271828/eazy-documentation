
(in-package :eazy-documentation)

(defclass eazy-template (common-html.template:template)
  ((css-list :type list :initarg :css-list :initform *default-css*)
   (font-list :type list :initarg :font-list :initform *default-fonts*)
   (js-list  :type list :initarg :js-list  :initform *default-js*)))

(defmethod common-html.template:render ((template eazy-template)
                                        (document document)
                                        content-string)
  "The simplest template."
  ;; while we could create a complex directory structure, we don't.
  (with-slots (css-list js-list font-list header footer) template
    (cl-who:with-html-output-to-string (s)
      ;; <!DOCTYPE html>
      (:html
       (:head
        (:link :rel "stylesheet"
               :href (format nil "https://fonts.googleapis.com/css?family=~{~a~^|~}" font-list))
        (dolist (css css-list)
          (cl-who:htm
           (:link :rel "stylesheet" :type "text/css" :href css)))
        (dolist (js js-list)
          (cl-who:htm
           (:script :type "text/javascript" :src js)))
        (:script :type "text/javascript"
                 "window.onload = function(){HighlightLisp.highlight_auto({className: 'lisp'});};")
        (:title
         (cl-who:str (plump:encode-entities (title document)))))
       (:body
        (cl-who:str content-string))))))

(defmethod common-html.template:render-section ((template eazy-template) (document document) (section section)
                           content-string)
  "The simplest section template."
  (with-slots (css-list js-list font-list header footer) template
    (cl-who:with-html-output-to-string (s)
      ;; <!DOCTYPE html>
      (:html
       (:head
        (:link :rel "stylesheet"
               :href (format nil "https://fonts.googleapis.com/css?family=~{~a~^|~}" font-list))
        (dolist (css css-list)
          (cl-who:htm
           (:link :rel "stylesheet" :type "text/css" :href css)))
        (dolist (js js-list)
          (cl-who:htm
           (:script :type "text/javascript" :src js)))
        (:script :type "text/javascript"
                 "window.onload = function(){HighlightLisp.highlight_auto({className: 'lisp'});};")
        (:title
         (cl-who:str (plump:encode-entities (title document)))))
       (:body
        (cl-who:str content-string))))))


