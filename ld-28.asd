;;;; ld-28.asd

(asdf:defsystem #:ld-28
  :serial t
  :description "Describe ld-28 here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:lispbuilder-sdl
               #:lispbuilder-sdl-ttf
               #:l-math)
  :components ((:file "package")
               (:file "ld-28")))

