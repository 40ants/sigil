(in-package #:sigil-cli)


(defmacro while (test &body body)
  `(loop
      (when (not ,test)
        (return))
      ,@body))


(defun add-include-path (dir)
  (push (probe-file dir) sigil::*include-paths*))


(defun eval-lisp (code)
  (format t "/* --eval ~A~% */" (read-from-string code))
  (in-package :ps)
  (eval (read-from-string code)))


(defun eval-ps (code)
  (format t "/* --pseval ~A~% */" (read-from-string code))
  (ps:ps* (read-from-string code)))


(defun process-file (arg)
  ;; invert paths to make them come in user specified order
  (setf *include-paths* (reverse sigil::*include-paths*))
  (handler-bind
      ((error
        (lambda (e)
          (format *error-output* "~A~%" e)
          (sb-ext:exit :code 1))))
    (sigil:compile-ps-file arg)))


(defun main (argv)
  
  (push (probe-file ".") sigil::*include-paths*)
  (if argv
      (while argv
        (let ((arg (pop argv)))
          (cond 
            ((string= arg "-I") (add-include-path (pop argv)))
            ((string= arg "-i") (sigil-repl:repl))
            ((string= arg "--eval") (eval-lisp (pop argv)))
            ((string= arg "--pseval") (eval-ps (pop argv)))
            (t (process-file arg)))))
      (sigil-repl:repl)))
