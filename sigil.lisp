(in-package #:sigil)

(defvar *include-paths* ())


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
        (error (format nil "Unable to find path for system \"~A\"."
                       system-name)))))


(defun find-file (file)
  (let ((system-name (get-system-part file)))
    (if system-name
        (let ((*include-paths*
              (cons (find-system-path system-name)
                    *include-paths*)))
          (find-real-file (get-file-part file)))
        (find-real-file file))))


(defun find-real-file (file)
  "Searches file with given name in *include-paths* directories"
  (if (probe-file file)
      file
      (block found
        (dolist (include-path *include-paths*)
          (let* ((search-path (directory-namestring include-path))
                 (path (concatenate 'string search-path file)))
          
            (format *error-output* "Checking include path: ~A~%" search-path)
          
            (when (probe-file path)
              (format *error-output* "File ~A readed successfuly.~%" file)
              (return-from found path))))
      
        (error (format nil "Cannot find file \"~A\"~%" file)))))


(defun compile-ps-file (filename)
  "Opens filename and outputs JS to stdout"

  (let* ((filename (find-file (namestring filename))))
    (format t "/* ~A */~%" filename)
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
              "import { ~{~a~^, ~} } from '~A';"
              "import ~A from '~A';")
          obj
          js-library))


(defun form2js (form)
  (case (car form)
    ('import (funcall #'output-import-statement (tail form)))
    ('load (compile-ps-file (second form)))
    (otherwise (simple-js-form form))))


(defun ps2js (f)
  "Reads forms from stream f and outputs to stdout"
  (in-package :ps)
  (do
   ((form (read f nil) (read f nil)))
   ((not form))
    (form2js form)))


