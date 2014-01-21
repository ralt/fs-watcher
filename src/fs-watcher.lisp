(in-package #:fs-watcher)

(defun watch (pathnames on-change &key on-new on-delete (delay 1))
  "Watches a list of pathnames"
  (if (listp pathnames)
      (setf pathnames (mapcar #'truename pathnames))
      (setf pathnames (list (truename pathnames))))
  (let ((mtimes (make-hash-table :test 'equal))
        (all (list)))
    (walk-paths pathnames
                (lambda (pathname)
                  (setf (gethash pathname mtimes) (mtime pathname))
                  (push pathname all)))
    (run-loop pathnames all mtimes on-change on-new on-delete delay)))

(defun walk-paths (paths file-action)
  "Returns a list of all the contents in a directory"
  (dolist (path paths)
    (if (directory-p path)
        (walk-directory path
                        (lambda (file) (funcall file-action file) file)
                        :directories t)
        (when (file-p path)
          (funcall path file-action)))))

(defun run-loop (original all mtimes on-change on-new on-delete delay)
  "The main loop constantly polling the filesystem"
  (loop
     (sleep delay)
     (let ((found (list)))
       (walk-paths original
                   (lambda (pathname)
                     (push pathname found)
                     (let ((mtime (mtime pathname)))
                       (if mtime
                           (if (gethash pathname mtimes)
                               (unless (= mtime
                                          (gethash pathname mtimes))
                                 (funcall on-change pathname)
                                 (setf (gethash pathname mtimes) mtime))
                               (progn (when on-new
                                        (funcall on-new pathname))
                                      (push pathname all)
                                      (setf (gethash pathname mtimes) mtime)))))))
       (dolist (deleted (set-difference all found :test #'equal))
         (when on-delete
           (funcall on-delete deleted))
         (setf all (remove deleted all :test #'equal))
         (remhash deleted mtimes)))))

(defun mtime (pathname)
  "Returns the mtime of a pathname"
  (when (probe-file pathname)
    (file-write-date pathname)))
