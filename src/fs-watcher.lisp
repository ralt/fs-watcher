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
  "For each directory, collect the file pathnames recursively. Results are appended and returned as a single list."
  (let (acc)
    (dolist (pathname pathnames acc)
      (if (directory-p pathname)
          (walk-directory pathname
                          #'(lambda (pathname)
                              (push pathname acc)))
          (push pathname acc)))))

(defun run-loop (pathnames mtimes callback delay)
  "The main loop constantly polling the filesystem"
  (loop
    (sleep delay)
    (map nil
         #'(lambda (pathname)
             (let ((mtime (mtime pathname)))
               (unless (eql mtime
                            ;; universal time or nil, so = is ng.
                            ;; eq between integers is impl-dependent behavior
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
