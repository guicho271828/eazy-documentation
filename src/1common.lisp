(in-package :eazy-documentation)

(defparameter +keywords+ '((title "(no title)" title-supplied-p)
                           (header "")
                           (footer "")
                           (whitelist nil)
                           (blacklist '())
                           (external-only t)
                           (toc t)
                           (max-depth 2)
                           (template-class 'eazy-template)
                           (css-list *default-css*)
                           (js-list  *default-js*)
                           (font-list  *default-fonts*)
                           (clean nil)
                           (remote-root nil)
                           (local-root nil)
                           (static-files nil)
                           (markup "org")
                           &allow-other-keys)
  "The list of keyword argument list shared by several functions.")

(defparameter +doc+ "
Options:

| keyword           | description                                                           |
|-------------------+-----------------------------------------------------------------------|
| =:title=          | Documentation title                                                   |
| =:header=         | The header inserted after the title                                   |
| =:footer=         | The footer inserted at the bottom                                     |
| =:markup=         | Markup langage used in the docstring, should be supported by pandoc.  |
|-------------------+-----------------------------------------------------------------------|
| =:whitelist=      | Whitelist of the package designators for the symbols being documented |
| =:blacklist=      | Blacklist of the package designators for the symbols being documented |
| =:external-only=  | Generate entries for external symbols only                            |
|-------------------+-----------------------------------------------------------------------|
| =:toc=            | Generate a table of contents (toc)                                    |
| =:max-depth=      | The maximum depth of a toc                                            |
|-------------------+-----------------------------------------------------------------------|
| =:template-class= | COMMON-HTML template class, no need to be chanded.                    |
| =:css-list=       | List of CSS scripts to be added to the template.                      |
| =:js-list=        | List of Javascripts to be added to the template.                      |
| =:font-list=      | List of Google fonts to be added to the template.                     |
| =:clean=          | Overwrite CSS/JS in the target directory                              |
|-------------------+-----------------------------------------------------------------------|
| =:remote-root=    | Used to generate a weblink. Example: https://github.com/<name>/<proj> |
| =:local-root=     | Used to generate a weblink. Example: /home/<user>/lisp/<proj>         |
| =:static-files=   | List of static README files etc.                                      |
|-------------------+-----------------------------------------------------------------------|
" )

(defparameter +ignore+
  `(declare (ignorable ,@(mapcar #'first (butlast +keywords+))
                       ,@(remove nil (mapcar #'third (butlast +keywords+))))
            (special local-root remote-root))
  "Declare statement that says ignorable for the keyword arguments in +keywords+.")


(defun basename (pathname)
  "path/to/dir/file -> file"
  (make-pathname :name (pathname-name pathname)
                 :type (pathname-type pathname)
                 :directory nil))

(defun dirname (pathname)
  "path/to/dir/file -> path/to/dir

use uiop:pathname-directory-pathanme when you need path/to/dir/
"
  (let ((dir (pathname-directory pathname)))
    (make-pathname
     :name (lastcar dir)
     :type nil
     :directory (butlast dir))))

(defun copy-destination (src dir)
  (make-pathname :name (pathname-name src)
                 :type (pathname-type src)
                 :directory (pathname-directory dir)))

(defun copy-to-dir (src dir &optional force)
  (let ((dst (copy-destination src dir)))
    (ignore-errors
      (copy-file src dst :if-to-exists (if force :supersede :error)))))

(defun local-enough-namestring (file)
  (declare (special local-root))
  (enough-namestring file local-root))

(defun remote-enough-namestring (file)
  (declare (special remote-root))
  (format nil "~a~a" remote-root (local-enough-namestring file)))


