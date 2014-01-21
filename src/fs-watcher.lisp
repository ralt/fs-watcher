(in-package #:fs-watcher)

(defun watch (pathnames on-change &key on-delete (delay 1))
  "Watches a list of pathnames"
  (unless (listp pathnames)
    (setf pathnames (list pathnames)))
  (setf pathnames (flatten (dir-contents pathnames)))
  (let ((mtimes (make-hash-table)))
    (map nil
         (lambda (pathname)
           (setf (gethash pathname mtimes) (mtime pathname)))
         pathnames)
    (run-loop pathnames mtimes on-change on-delete delay)))

(defun dir-contents (pathnames)
  "Returns a list of all the contents in a directory"
  (let ((files (list)))
    (dolist (pathname pathnames)
      (if (directory-p pathname)
          (walk-directory pathname
                          (lambda (file) (push file files))
                          :directories t)
          (when (file-p pathname)
            (push pathname files))))
    files))

(defun run-loop (pathnames mtimes on-change on-delete delay)
  "The main loop constantly polling the filesystem"
  (loop
     (sleep delay)
     (dolist (pathname pathnames)
       (let ((mtime (mtime pathname)))
         (if mtime
             (unless (= mtime
                        (gethash pathname mtimes))
               (funcall on-change pathname)
               (setf (gethash pathname mtimes) mtime))
             (progn
               (setf pathnames (remove pathname pathnames))
               (remhash pathname mtimes)
               (when on-delete
                 (funcall on-delete pathname))))))))

(defun mtime (pathname)
  "Returns the mtime of a pathname"
  (when (probe-file pathname)
    (file-write-date pathname)))
