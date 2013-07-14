(asdf:defsystem #:fs-watcher
  :serial t
  :description "Filesystem watcher for changes"
  :author "Florian Margaine <florian@margaine.com>"
  :license "MIT License"
  :depends-on ("com.gigamonkeys.pathnames")
  :components ((:file "src/package")
               (:file "src/watcher")
               (:file "src/dir-watcher")
               (:file "src/file-watcher")
               (:file "src/fs-watcher")))

