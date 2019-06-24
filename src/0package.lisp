#|

This file is a part of NUMCL project.
Copyright (c) 2019 IBM Corporation
SPDX-License-Identifier: LGPL-3.0-or-later

NUMCL is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any
later version.

NUMCL is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
NUMCL.  If not, see <http://www.gnu.org/licenses/>.

|#

(defpackage :eazy-documentation
  (:use :cl :alexandria :trivia :iterate :common-doc)
  (:documentation
   "
This package provides an easy-pezy way to extract documentations from
a lisp source code.

Advantages over the existing libraries:

* EAZY-DOCUMENTATION can generate documents from a single file or an ASDF system.
  Run `(eazy-documentation:generate-html-from-system <system-name> \"index.html\")` or
  `(eazy-documentation:generate-html-from-file #p\"script.lisp\" \"index.html\")`.
  No additional quirks are necessary.

* Extraction is heuristic and robust. It is easily automated and can handle
  unknown user-defined macro whose name begins with DEF.

* As per Quickdocs, it respects the file and directory structure. Well-written, maintainable
  libraries have a great readability in the source code. This should be respected
  and the documentation generator should respect this structure.

* It minimizes the clutter in the auto-generateded output by detecting and
  grouping the similar documentation entries.
  For example, these entries are grouped together:
  
  * Functions with the same lambda-list and docstrings.
  * Variables with the same docstrings.
  * Any DEF-something macro entries with the same name and missing documentations.

* Typesetting and syntax highlighting of docstring is done by
  [Pandoc](https://pandoc.org/) which supports a wide range of language and
  format support.

  Run `curl -sSL https://get.haskellstack.org/ | sh && stack install pandoc`
  to install the latest Pandoc.

* The output is a common-doc object, thus easily pluggable to the existing emitter
  for them.

")
  (:export
   #:extract-definitions-from-file
   #:extract-definitions-from-system
   #:generate-commondoc-from-file
   #:generate-commondoc-from-system
   #:generate-html-from-file
   #:generate-html-from-system
   #:+doc+
   #:convert-file-to-html-string
   #:convert-string-to-html-string
   #:convert-file-to-ascii-string
   #:convert-string-to-ascii-string))

(in-package :eazy-documentation)

(defun list-all-define-macros (&aux acc)
  (do-all-symbols (s)
    (when (and (fboundp s)
               (macro-function s)
               (eql 0 (search "DEF" (symbol-name s))))
      (push s acc)))
  (let ((len (reduce #'max acc :key (alexandria:compose #'length #'symbol-name))))
    (dolist (s acc)
      (format t "~%~(~v@a : ~a~)" len s (princ-to-string (sb-kernel:%fun-lambda-list (macro-function s)))))))

(defun note (format-string &rest args)
  (fresh-line *error-output*)
  (let ((*print-right-margin* 100)
        (*print-pretty* t)
        (*print-length* 100))
    (pprint-logical-block (*error-output* nil :per-line-prefix "; ")
      (apply #'format
             *error-output*
             format-string args))))
