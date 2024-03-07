;;

(ql:quickload 'planesnwind)

(format t "loaded ok, continuing, going to fork and write.~%")

(defparameter core-fn "out.bin")



(defun tl ()
  (format t "--- hi from the tl!~%")
  ;;(format t "build stuff ~a ~a ~a ~a~%"
  ;;        build-hn build-user build-date build-ver)

  (format t "build host: '~a'~%" planesnwind:+build-host+)

  (planesnwind:start-as-worker))

#|
(let ((fork-result (sb-posix:fork)))
  (case fork-result
    (-1 (error "fork failed"))
    (0
     (format t "fork succeeded, writing...~%")
     (sb-ext:save-lisp-and-die
        core-fn
        :toplevel #'tl :executable t))
    (otherwise (sb-posix:wait)))
  (format t "stand-alone core ~a saved" core-fn))
|#

(defun run-n-return-str (progpath &optional args)
  (remove #\newline
          (with-output-to-string (strm)
            (sb-ext:run-program progpath args :output strm))))

(let ((build-hn (run-n-return-str "/bin/hostname"))
      (build-user (run-n-return-str "/usr/bin/whoami"))
      (build-date (run-n-return-str "/bin/date"))
      (iso-date (run-n-return-str "/bin/date" '("-Iminutes")))
      (build-ver (format nil "~a-~a"
                         (lisp-implementation-type)
                         (lisp-implementation-version))))
  (setf planesnwind:+build-host+ build-hn)
  (setf planesnwind:+build-user+ build-user)
  (setf planesnwind:+build-date+ build-date)
  (setf planesnwind:+build-sbcl-ver+ build-ver)
  (setf planesnwind:+my-vertail+
        (planesnwind.db::replace-all
         (planesnwind.db::replace-all
          (subseq iso-date 0 16)
          "-" "")
         ":" ""))

  (sb-ext:save-lisp-and-die
   core-fn
   :toplevel #'tl :executable t))

;;(defun main (argv)
;;  (format t " ?? hello? ~%"))

;;(asdf:load-system :cffi-grovel)
;;(asdf:operate :static-program-op :planesnwind)

(format t "done.~%")
