(in-package #:sigil-cli)


(defmacro while (test &body body)
  `(loop
      (when (not ,test)
        (return))
      ,@body))


(defun main (argv)
  (push (probe-file ".") sigil::*include-paths*)
  (if (cdr argv)
      (progn
        (pop argv)
        (while argv
          (let ((arg (pop argv)))
            (cond 
              ((string= arg "-I")
               (let ((dir (pop argv)))
                 (push (probe-file dir) sigil::*include-paths*)))
              ((string= arg "-i") (sigil-repl:repl))
              ((string= arg "--eval")
               (let ((code (pop argv)))
                 (format t "/* --eval ~A~% */" (read-from-string code))
                 (in-package :ps)
                 (eval (read-from-string code))))
              ((string= arg "--pseval")
               (let ((code (pop argv)))
                 (format t "/* --pseval ~A~% */" (read-from-string code))
                 (ps:ps* (read-from-string code))))
              (t
               (let ((probe-results (probe-file arg)))
                 (when probe-results
                   ;; Add current file directory to include paths so they can relative load properly
                   (push (directory-namestring probe-results) sigil::*include-paths*)
                   
                   (setf *include-paths* (reverse sigil::*include-paths*))
                   (with-open-file (f arg)
                     (handler-bind
                         ((error
                           (lambda (e) 
                             (format *error-output* "~A~%" e)
                             (sb-ext:exit :code 1))))
                       (ps2js f))))))))))
      (sigil-repl:repl)))
