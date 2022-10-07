(asdf:defsystem #:control-ui
  :description "Describe ps-react-example here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on
  (#:asdf
   #:quicklisp
   #+hunchentoot #:hunchentoot
   #+woo #:woo
   #:clack
   #:optima
   #:alexandria
   #:uiop
   #:clog)
  :components
  ((:module "secrets"
    :components
    ((:file "secrets")))
   (:module "src"
    :components
    ((:file "control-loop")
     (:file "control-ui")))))


