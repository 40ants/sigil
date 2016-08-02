(defsystem sigil
  :name "sigil"
  :author "Alexander Artemenko"
  :version "0.1.0"
  :description "A Parenscript to Javascript builder."
  
  :depends-on (#:parenscript)
  :serial t
  :components ((:file "packages")
               (:file "sigil")
               (:file "repl")
               (:file "cli")))
