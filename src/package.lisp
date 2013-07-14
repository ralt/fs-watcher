(defpackage #:fs-watcher
  (:use #:cl #:com.gigamonkeys.pathnames)
  (:export :watch
           :dir-watcher
           :file-watcher
           :*delay*))
