(in-package #:sigil)

(defvar *include-paths* ())
(defvar *verbose* nil)


(defun log-message (message &rest args)
  (when *verbose*
    (apply #'format (append (list *error-output* message) args))))


(defun get-system-part (filename)
  "For filename like \"some.foo:bar.lisp\" returns \"some.foo\" string"
  (let ((pos (position #\: filename)))
    (when pos
      (subseq filename 0 pos))))


(defun get-file-part (filename)
  "For filename like \"some.foo:bar.lisp\" returns \"bar.lisp\" string.
If there is no system part, then just return filename."
  (let ((pos (position #\: filename)))
    (if pos
        (subseq filename (1+ pos))
        filename)))


(defun find-system-path (system-name)
  "Returns path to system's files or throws error"

  (let ((path (ql:where-is-system system-name)))
    (if path
        path
        ;; try to quickload the system and search path again
        (if (ql:quickload system-name :silent t)
            (find-system-path system-name)
            ;; or return error is system wasn't found
            (error (format nil "Unable to find path for system \"~A\"."
                           system-name))))))


(defun find-file (file)
  (let ((system-name (get-system-part file)))
    (if system-name
        ;; find file inside specified system
        (let ((*include-paths*
               (cons (find-system-path system-name)
                     *include-paths*)))
          (find-real-file (get-file-part file)))

        ;; find file on filesystem
        (find-real-file file))))


(defun find-real-file (file)
  "Searches file with given name in *include-paths* directories"
  (if (probe-file file)
      file
      (block found
        (dolist (include-path *include-paths*)
          (let* ((search-path (directory-namestring include-path))
                 (path (concatenate 'string search-path file)))
          
            (log-message "Checking include path: ~A~%" search-path)
          
            (when (probe-file path)
              (log-message "File ~A readed successfuly.~%" file)
              (return-from found path))))
      
        (error (format nil "Cannot find file \"~A\"~%" file)))))


(defun compile-ps-file (filename)
  "Opens filename and outputs JS to stdout"

  (let* ((filename (find-file (namestring filename))))
    (log-message "Compiling ~A~%" filename)
    (let ((*include-paths* (cons (directory-namestring filename)
                                 *include-paths*)))
      (with-open-file (f filename)
        (ps2js f)))))


(defun simple-js-form (form)
  "Outputs usual form"
  (format t  "/* ~A */~%" form)
  (format t "~A~%" (ps:ps* form)))


(defun output-import-statement (js-library obj)
  "Compiles:
       (import \"react\" \"React\")
   into:
       import React from 'react';

   If third argument is list, like that:
       (import \"react-router\" '(\"Router\" \"browserHistory\"))
   then compiles into:
       import { Router, browserHistory } from 'react-router';
   "
  (format t
          (if (listp obj)
              "import { ~{~a~^, ~} } from '~A';~%"
              "import ~A from '~A';~%")
          obj
          js-library))


(defun load-lisp-system (name)
  (with-output-to-string (*standard-output*)
    (ql:quickload name :silent t)))


(defun form2js (form)
  "Outputs to stdout JS representation of the form"
;;  (format t "Processing ~A~%" (car form))
  (let* ((first-item (car form))
         (item-symbol-name (when (symbolp first-item)
                             (symbol-name first-item))))
    
    (macrolet ((is (name) `(equal item-symbol-name ,name)))
      (cond
        ((is "QUICKLOAD")
         (load-lisp-system (second form)))
        ((is "IMPORT")
         (apply #'output-import-statement (cdr form)))
        ((is "LOAD")
         (compile-ps-file (second form)))
        (t
         (simple-js-form form))))))


(defun ps2js (f)
  "Reads forms from stream f and outputs to stdout"
  (in-package :ps)
  (do
   ((form (read f nil) (read f nil)))
   ((not form))
    (form2js form)))


