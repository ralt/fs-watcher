(defpackage #:fs-watcher
  (:use #:cl #:com.gigamonkeys.pathnames)
  (:import-from #:alexandria :flatten)
  (:export :watch))
