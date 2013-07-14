(in-package #:fs-watcher)

(defclass watcher ()
  ((path
     :initarg :path
     :reader path)
   (mtime
     :accessor mtime)))

(defmethod check ((w watcher))
  "Watches a file"
  (if (probe-file (slot-value w 'path))
    (when (has-changed w)
      (set-mtime w)
      (fire-change w))
    (delete w *watchers*)))

(defmethod set-mtime ((w watcher))
  "Sets the mtime of a watcher"
  (with-slots ((path path) (mtime mtime)) w
    (setf mtime (get-mtime w))))

(defmethod get-mtime ((w watcher))
  "Gets the mtime of a watcher"
  (with-slots ((path path)) w
    (file-write-date path)))

(defmethod has-changed ((w watcher))
  "Checks if the mtime has changed"
  (/= (slot-value w 'mtime) (get-mtime w)))

(defun new-watchers (pathnames)
  "Creates new instances of watchers"
  (loop for path across pathnames do (new-watcher path)))

(defun new-watcher (path)
  "Creates a new instance of a watcher"
  (if (file-p path)
    (vector-push-extend (make-instance 'file-watcher :path path) *watchers*)
    (vector-push-extend (make-instance 'dir-watcher :path path) *watchers*)))

(defgeneric fire-change (watcher)
  (:documentation "Finds out the change to make and fires it"))
