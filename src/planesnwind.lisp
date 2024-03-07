(in-package :cl-user)
(defpackage planesnwind
  (:use :cl
        :planesnwind.worker))
(in-package planesnwind)
(cl-syntax:use-syntax :annot)

;; simple distributed worker pool thx to postgres

@export
(defparameter +build-host+ "none")

@export
(defparameter +build-user+ "none")

@export
(defparameter +build-date+ "none")

@export
(defparameter +build-sbcl-ver+ "none")

@export
(defparameter +my-vertail+ "none")

@export
(defparameter +my-verstr+ "pnw-0.3")

@export
(defparameter +my-fullverstr+ "none")

(defparameter +my-sname+ "unset")
(defparameter +my-lname+ "unset")
(defparameter +my-pid+ nil)
(defparameter +my-dbport+ 3306)

@export
(defun find-and-process-a-flight (&optional take-this-fid)
  (let ((fid (or take-this-fid (planesnwind.db:new-select-flight-to-run))))
    (cond
      ((not fid)
       (format t "--! db didnt give us back an fid to run.~%")
       (worker-log "proc-fids-none-found" "")
       nil)
      (t
       (format t "-------- picked up fid ~a, starting it.~%" fid)

       (worker-log "proc-fids-start" (format nil "starting fid ~d" fid) (float fid))

       (handler-case (planesnwind.paths:check-then-optimize-and-save-flight fid)
         (error (c)
           (format t "!!!!! condition during fid ~d: ~a~%" fid c)
           (worker-log "proc-fids-err" (format nil "caught err during fid ~d, setting bad runstate: ~a" fid c))

           (planesnwind.db:update-runstate-for-fid fid -30)
           (planesnwind.db:hstore-upsert :flights
                                         (list (cons "failed" "true")
                                               (cons "condition"
                                                     (format nil "~a" c)))
                                         fid)))
       (worker-log "proc-fids-finish" (format nil "finishing fid ~d" fid) (float fid))
       fid))))

@export
(defun fetch-a-few-flights-from-fa (&optional num)
  (format t "--- asking db for flight ident suggestions to fetch...~%")

  (let ((fl-idents (planesnwind.db:new-find-flights-to-maybe-fetch (or num 10))))
    (cond
      ((not fl-idents)
       (format t "!! weird, asked the db for flight idents to fetch, got nil.~%")
       nil)
      ((= 0 (length fl-idents))
       (format t "!! db gave us back zero flight idents to fetch.~%")
       nil)
      (t
       (format t "------- cool, got ~d flights to try to fetch.~%" (length fl-idents))

       (loop for flname in fl-idents
            collecting
              (let ((sts (/ (float (get-internal-real-time)) internal-time-units-per-second))
                    (failed-p nil))
                (handler-case (planesnwind.flights:fetch-and-add-scr-path flname)
                  (error (c)
                    (format t "!!!!! condition caught while fetching path for flight ~a: ~a~%" flname c)
                    (setf failed-p t)
                    (worker-log "fa-pg-scr-condition"
                                (format nil "condition while fetching flight ~a: ~a"
                                        flname c))))
                (unless failed-p
                  (worker-log "fa-pg-scr-success"
                              flname
                              (- (/ (float (get-internal-real-time)) internal-time-units-per-second) sts))
                  (format t "-- sweet, fetched and inserted ~a successfully in ~3$ s.~%"
                          flname (- (/ (float (get-internal-real-time)) internal-time-units-per-second) sts)))))))))

@export
(defun simple-fetch ()
  (planesnwind.db:connect-db)

  (loop while t do
       (let ((flname (planesnwind.db:nab-rand-kf))
             (sts (/ (float (get-internal-real-time)) internal-time-units-per-second))
             (failed-p nil))
         (handler-case (planesnwind.flights:fetch-and-add-scr-path flname)
           (error (c)
             (format t "!!!!! condition caught while fetching path for flight ~a: ~a~%" flname c)
             (setf failed-p t)
             (worker-log "fa-pg-scr-condition"
                         (format nil "condition while fetching flight ~a: ~a"
                                 flname c))))
         (unless failed-p
           (worker-log "fa-pg-scr-success"
                       flname
                       (- (/ (float (get-internal-real-time)) internal-time-units-per-second) sts))
           (format t "-- sweet, fetched and inserted ~a successfully in ~3$ s.~%"
                   flname (- (/ (float (get-internal-real-time)) internal-time-units-per-second) sts)))
         (sleep (+ 20 (random 40))))))

(defun die (&optional ret-code quiet-p)
  (unless quiet-p
    (format t "-! dying...~%"))
  (sb-ext:exit :code (or ret-code 1)))

@export
(defun start-as-worker ()
  (setf +my-fullverstr+
        (format nil "~a-~a" +my-verstr+ +my-vertail+))

  (format t "-- hey, starting up, im ~a~%" +my-fullverstr+)
  (format t "built ~a by ~a@~a using ~a~%"
          +build-date+ +build-user+ +build-host+ +build-sbcl-ver+)

  (let* ((env-sname (sb-ext:posix-getenv "HN"))
         (env-lname (sb-ext:posix-getenv "LN"))
         (env-dbh (sb-ext:posix-getenv "DB")))
    (when (and env-sname
               (stringp env-sname)
               (> (length env-sname) 2))
      (setf +my-sname+ env-sname))
    (when (and env-lname
               (stringp env-lname)
               (> (length env-lname) 2))
      (setf +my-lname+ env-lname)
      (if (or (not +my-sname+)
              (string= "unset" +my-sname+))
          (setf +my-sname+ +my-lname+)))
    (when (and env-dbh
               (stringp env-dbh)
               (> (length env-dbh) 2))
      (setf +my-dbport+
            (parse-integer
             (second (cl-ppcre:split ":" env-dbh))))
      (setf planesnwind.db:+db-port+ +my-dbport+))
    (setf +my-pid+ (sb-posix:getpid))

    (format t "starting with sname ~a, lname ~a, pid ~a, db port ~d.~%"
            +my-sname+ +my-lname+ +my-pid+ +my-dbport+)

    (setf planesnwind.worker:+my-name+
          (format nil "~a/~d"
                  (or +my-sname+ +my-lname+ "unknown") +my-pid+))

    (planesnwind.db:connect-db)

    (format t "db looks good.~%")

    (planesnwind.worker:worker-log
     "worker-startup"
     (format nil "hi, im ~a, built ~a by ~a@~a using ~a. my sname is ~a, lname ~a, dbh ~a, db port ~d, pid ~d."
             +my-fullverstr+ +build-date+ +build-user+ +build-host+ +build-sbcl-ver+
             +my-sname+ +my-lname+ env-dbh planesnwind.db:+db-port+ +my-pid+))

    (let ((r (planesnwind.worker:get-worker-conf :curr-version))
          (env-anyver (sb-ext:posix-getenv "ANYVER")))
      (if env-anyver
          (format t "--- env says not to care about ver, but just a heads up were ~a and db current is ~a.~%"
                  +my-fullverstr+ r)
          (cond
            ((or (not r)
                 (not (stringp r)))
             (format t "! failed to get current version from db, cant continue.~%")
             (planesnwind.worker:worker-log "ver-check-fail" +my-fullverstr+)
             (sb-ext:exit :code 1))

            ((not (string= r +my-fullverstr+))
             (format t "! were running a different version than db suggests: ~a vs db ~a, cant continue.~%"
                     +my-fullverstr+ r)
             (planesnwind.worker:worker-log "ver-incorrect"
                                            (format nil "us ~a, db ~a"
                                                    +my-fullverstr+ r))
             (sb-ext:exit :code 1))

            (t
             (format t "our ver matches db, cool. ~%")))))

    ;;(work)
    (simple-fetch)))

@export
(defun work ()
  (format nil "hello from work!~%")
  ;;(sb-ext:exit :code 0)

  ;;(find-and-process-a-flight 4852)
  ;;(die)


  (let ((killme nil))
    (loop while (not killme)
       do
         (let ((fetch-ena (get-worker-conf :fetching-enabled))
               (proc-ena nil))
           (when (and fetch-ena
                      (string= fetch-ena "true"))
             (sleep (+ 30.0 (random 30.0)))
             (fetch-a-few-flights-from-fa 8))

           (setf proc-ena (get-worker-conf :processing-enabled))
           (when (and proc-ena
                      (string= proc-ena "true"))
             (let ((n 0))
               (loop while (and (numberp (find-and-process-a-flight)) (< n 4))))
             (format t "--- looks like were out of flights to process.~%")
             (worker-log "no-more-fids-to-proc" ""))

           (setf killme (get-worker-conf :killall))

           (if (not (or fetch-ena proc-ena))
               (sleep 10))))))

@export
(defun save-core (core-fn)
  (progn
    #+sbcl
    (let ((fork-result (sb-posix:fork)))
      (case fork-result
        (-1 (error "fork failed"))
        (0 (sb-ext:save-lisp-and-die
            core-fn
            :toplevel #'start-as-worker :executable t))
        (otherwise (sb-posix:wait)))
      (format t "stand-alone core ~a saved" core-fn))
    #-sbcl
    (error "not available on this lisp")
    (values)))


#|
(defun initial-setup (&key debug)
  ;; get the airports csv, parse and insert

  (planesnwind.util::fetch-us-airports)
  )
|#
