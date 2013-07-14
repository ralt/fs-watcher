(in-package #:fs-watcher)

(defvar *delay* 1)

(defvar *watchers* (make-array 0
                               :adjustable t
                               :fill-pointer 0))

(defvar *onchange*)

(defun watch (pathnames listener)
  "Watches a list of pathnames"
  (setf *onchange* listener)
  (new-watchers pathnames)
  (loop
    (progn
      (sleep *delay*)
      (loop for watcher across *watchers* do (check watcher)))))
