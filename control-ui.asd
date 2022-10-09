(asdf:defsystem #:control-ui
  :description "Describe ps-react-example here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on
  (#:uiop
   #:quicklisp
   #+hunchentoot #:hunchentoot
   #+woo #:woo
   #:clack
   #:optima
   #:alexandria
   #:dexador
   #:yason
   #:clog)
  :components
  ((:module "secrets"
    :components
    ((:file "secrets")))
   (:module "src"
    :components
    ((:file "control-ui")))))
