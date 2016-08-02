(defpackage sigil
  (:use #:common-lisp)
  (:export compile-file))


(defpackage sigil-cli
  (:use #:common-lisp
        #:sigil)
  (:export main))


(defpackage sigil-repl
  (:use #:common-lisp
        #:sigil)
  (:export repl))


(defpackage sigil-playground
  (:use #:common-lisp
        #:sigil)
  (:export compile-file))
