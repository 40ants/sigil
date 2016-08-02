(in-package #:sigil-repl)


(defun repl ()
  (let* ((node (sb-ext:run-program "node" '("-i") :search t :input :stream :output :stream :wait nil))
         (node-input (sb-ext:process-input node))
         (node-output (sb-ext:process-output node)))
    (loop
       (format *error-output* "> ")
       (force-output *error-output*)
       (read-char node-output) ; eat initial prompt
       (handler-case
           (let ((form (read)))
             (format node-input "~A~%" (ps:ps* form))
             (force-output node-input)
             (loop
                (let ((c (read-char node-output)))
                  (when (and (char= #\Newline c)
                             (char= #\> (peek-char nil node-output)))
                    (read-char node-output)
                    (fresh-line)
                    (return))
                  (princ c)
                  (force-output))))
         (sb-sys:interactive-interrupt () (sb-ext:exit))
         (end-of-file () (sb-ext:exit))
         ))))
