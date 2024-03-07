(in-package :cl-user)
(defpackage planesnwind.worker
  (:use :cl
        :planesnwind.util))
(in-package :planesnwind.worker)
(cl-syntax:use-syntax :annot)

@export
(defparameter +my-name+ "unset")

@export
(defun update-worker-state (state-str txt-str)
  (handler-case
      (planesnwind.db:update-db-worker-state +my-name+ state-str txt-str)
    (error (c)
      (format t "!!! err updating worker state in db: ~a~%" c)
      nil)))

@export
(defun worker-log (tag-str txt-str &optional val-float)
  (handler-case
      (planesnwind.db:worker-log-to-db +my-name+ tag-str txt-str val-float)
    (error (c)
      (format t "!!! err inserting worker log: ~a~%" c)
      nil)))

@export
(defun get-worker-conf (k)
  (handler-case
      (planesnwind.db:worker-get-db-conf-key k)
    (error (c)
      (format t "!!! err fetching db conf for key ~a: ~a~%"
              k c))))
