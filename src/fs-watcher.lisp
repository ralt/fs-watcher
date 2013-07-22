(in-package #:fs-watcher)

(defun watch (pathnames callback &key (delay 1))
  "Watches a list of pathnames"
  (unless (listp pathnames)
    (setf pathnames (list pathnames)))
  (setf pathnames (flatten (dir-contents pathnames)))
  (let ((mtimes (make-hash-table)))
    (map nil
         #'(lambda (pathname)
             (setf (gethash pathname mtimes) (mtime pathname)))
         pathnames)
    (run-loop pathnames mtimes callback delay)))

(defun dir-contents (pathnames)
  "Returns a list of all the contents in a directory"
  (mapcar #'(lambda (pathname)
              (when (directory-p pathname)
                (walk-directory pathname
                                #'(lambda (pathname)
                                    (push pathname pathnames))
                                :directories t)))
          pathnames))

(defun run-loop (pathnames mtimes callback delay)
  "The main loop constantly polling the filesystem"
  (loop
    (sleep delay)
    (map nil
         #'(lambda (pathname)
             (let ((mtime (mtime pathname)))
               (unless (eq mtime
                           (gethash pathname mtimes))
                 (funcall callback pathname)
                 (if mtime
                   (setf (gethash pathname mtimes) mtime)
                   (remhash pathname mtimes)))))
         pathnames)))

(defun mtime (pathname)
  "Returns the mtime of a pathname"
  (when (probe-file pathname)
    (file-write-date pathname)))
