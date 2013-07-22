(asdf:defsystem #:fs-watcher
  :serial t
  :description "Filesystem watcher for changes"
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :depends-on ("com.gigamonkeys.pathnames" "alexandria")
  :components ((:file "src/package")
               (:file "src/fs-watcher")))

