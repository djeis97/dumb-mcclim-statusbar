
(asdf:defsystem #:site.djei.bar
  :description "Simple X11 Status Bar Playground"
  :author "Elijah Malaby <emalaby@usf.edu>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               ;; #:atomics
               #:bordeaux-threads
               #:classowary
               #:cl-gobject-introspection
               #:clim-debugger
               #:local-time
               #:mcclim
               #:serapeum)
  :components ((:file "package")
               (:file "bar")))
