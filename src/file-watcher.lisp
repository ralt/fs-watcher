(in-package #:fs-watcher)

(defclass file-watcher (watcher) ())

(defmethod initialize-instance :after ((w watcher) &key)
  "Sets up the file watcher"
  (set-mtime w))

(defmethod fire-change ((w watcher))
  "Fires the listener"
  (funcall *onchange*))
