(in-package #:sigil-playground)

(ps:ps* `(load "app/some.lisp"))

(with-output-to-string (*standard-output*)
  (with-input-from-string (f "(load \"app/some.lisp\")")
    (sigil::ps2js f)))


(with-output-to-string (*standard-output*)
  (with-input-from-string (f "(load \"app:some.lisp\")")
    (sigil::ps2js f)))


