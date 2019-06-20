
(in-package :eazy-documentation)

(defun main (&rest argv)
  (declare (ignorable argv))
  (handler-case
      (uiop:run-program '("sh" "-c" "
                         which pandoc   || (echo 'Error: pandoc   not found! It should be a relatively new version supporting gfm.' >&2 ; exit 1)
                         which texi2any || (echo 'Error: texi2any not found!' >&2 ; exit 1)")
                        :output t
                        :error-output t)
    (error ()
      (roswell:quit 1)))
  (match argv
    ((list* file-or-system target rest)
     #+sbcl
     (proclaim '(sb-ext:muffle-conditions sb-ext:compiler-note style-warning))
     (apply #'generate-html file-or-system target (mapcar #'read-from-string rest))
     (finish-output *error-output*)
     (finish-output *trace-output*)
     (finish-output *standard-output*)
     (format t "~%Done!~%"))
    (nil
     (format *error-output*
             "~&Usage: eazy-documentation file-or-system output [OPTIONS]~%~
             ~%~
             ~% The output is a single file when the output has a html extension;~
             ~% otherwise it will be considered as a directory name.~
             ~%~
             ~% Options are specified as follows:~
             ~%~
             ~a" #.(convert-string-to-ascii-string eazy-documentation::+doc+ "gfm")))))
