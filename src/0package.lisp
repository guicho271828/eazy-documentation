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
  (:use :cl :alexandria :trivia)
  (:documentation
   "
This package provides an easy-pezy way to extract documentations from
a lisp source code.

Advantages over the existing libraries:

+ It does not depend on ASDF. --- It can be combined with ASDF, but it is
  not mandatory.

+ Extraction is heuristic and robust. --- It is easily automated and can handle
  unknown user-defined macro.

+ It respects the file and directory structure. Well-written, maintainable
  libraries have a great readability in the source code. This should be respected
  and the documentation generator should respect this structure.
  This decision directly comes from quickdocs.

+ The output is a common-doc object. Easily pluggable to the existing emitter
  supportd for common-doc objects.

"))

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
