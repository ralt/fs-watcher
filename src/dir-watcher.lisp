(in-package #:fs-watcher)

(defclass dir-watcher (watcher)
  ((files
     :accessor files)))

(defmethod initialize-instance :after ((w watcher) &key)
  "Sets up the directory watcher"
  (set-mtime w)
  (with-slots ((path path) (files files)) w
    (let ((pathnames (list-directory path)))
      (setf files (remove-if-not #'file-p pathnames))
      (new-watchers pathnames))))

(defmethod fire-change ((w watcher))
  "Finds out the new change and fires the listener"
  ;; Reload everything to get the new files
  (vector-push-extend (make-instance 'dir-watcher
                                     :path (slot-value w 'path))
                      *watchers*)
  (funcall *onchange*)
  (delete w *watchers*))
