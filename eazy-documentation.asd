;;;; Autogenerated ASD file for system "EAZY-DOCUMENTATION"
;;;; In order to regenerate it, run update-asdf
;;;; from shell (see https://github.com/phoe-krk/asd-generator)
;;;; For those who do not have update-asdf,
;;;; run `ros install asd-generator` (if you have roswell installed)
;;;; There are also an interface available from lisp:
;;;; (asd-generator:regen &key im-sure)
(defsystem eazy-documentation
 :version "0.1"
 :author "Masataro Asai"
 :mailto "guicho2.71828@gmail.com"
 :license "LGPL"
 :depends-on (:trivia
              :alexandria
              :iterate
              :cl-ppcre
              :trivia.ppcre
              :common-doc
              :common-html
              :common-doc-split-paragraphs)
 :pathname "src/"
 :serial t
 :components ((:file "0package")
              (:file "1definition")
              (:file "2extractor")
              (:file "3emitter")
              (:file "4user"))
 :description "Heuristic documentation extraction library")
