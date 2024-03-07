(in-package :cl-user)
(defpackage planesnwind.http
  (:use :cl
        :datafly
        :sxql
        :alexandria
        :cl-ppcre))
(in-package :planesnwind.http)
(cl-syntax:use-syntax :annot)


(defparameter +dbconn+ nil)

(defun connect-db-eek ()
  (format t "--- connecting to db.~%")

  (setf +dbconn+
        (datafly:connect-cached :postgres
                                :database-name planesnwind.db:+db-name+
                                :username planesnwind.db:+db-user+
                                :password planesnwind.db:+db-pass+
                                :host "127.0.0.1")))

(defmacro with-connection (conn &body body)
  `(let ((datafly:*connection* ,conn))
     ,@body))

(defun curr-db-or-connect ()
  (if +dbconn+
      +dbconn+
      (setf +dbconn+ (connect-db))))

(defun parse-float (str)
  (with-input-from-string (s str)
    (read s)))

#|
(defun fetch-latest-thing (vehicle thing &optional since-id)
  (with-connection (curr-db-or-connect)
    (datafly:retrieve-one

     (let ((req
            (concatenate 'string
                         "select id,extract(epoch from create_ts) as create_ts, "
                         "rawvals from rtdata "
                         (format nil "where vehicle = '~a' " vehicle)
                         (format nil "and dtype = '~a' " thing)
                         (when since-id
                           ;;(format nil "and create_ts > to_timestamp(~f) " since-ts))
                           (format nil "and id > ~a " since-id))
                         "order by create_ts desc limit 1")))
       ;;(format t "req: ~a~%" req)
       req))))
|#

(defvar *app* (make-instance 'ningle:<app>))

(defun set-api-headers ()
  (setf (lack.response:response-headers ningle:*response*)
        (append (lack.response:response-headers ningle:*response*)
                (list :content-type "application/json")))
  (setf (lack.response:response-headers ningle:*response*)
        (append (lack.response:response-headers ningle:*response*)
                (list :access-control-allow-origin "*"))))

(setf (ningle:route *app* "/")
      "hey man")

(defun fetch-airports ()
  (mapcar
   #'alexandria:plist-alist
   (retrieve-all ;; todo:
    "select ident,state,posn[0] as lon,posn[1] as lat from airports where contig_us=1 and posn[0] > -125.0 and posn[0] < -70.0 and posn[1] > 25.0 and posn[1] < 50.0")))

(defun fmt-airports (apt-list)
  (let ((min-xlon (getf planesnwind.util:+model-spatial-range+ :min-xlon))
        (max-xlon (getf planesnwind.util:+model-spatial-range+ :max-xlon))
        (min-ylat (getf planesnwind.util:+model-spatial-range+ :min-ylat))
        (max-ylat (getf planesnwind.util:+model-spatial-range+ :max-ylat)))
    `((:min--xlon . ,min-xlon)
      (:max--xlon . ,max-xlon)
      (:min--ylat . ,min-ylat)
      (:max--ylat . ,max-ylat)
      (:airports . ,apt-list))))

(defvar *apts-cache* nil)
(defun fetch-airports-resp-maybe-from-cache ()
  (or *apts-cache*
      (setf *apts-cache*
            (cl-json:encode-json-alist-to-string
             (fmt-airports
              (fetch-airports))))))

(defun fetch-flights-and-paths (&optional max-flights)
  (declare (ignore max-flights))

  (mapcar
   #'(lambda (f)
       (let ((fp (planesnwind.db:fetch-flightpath-by-flightid
                  (getf f :flight-id)))
             (pretty-alist (alexandria:plist-alist f)))
         (if fp
             (append pretty-alist
                     `((:posns . ,fp)))
             pretty-alist)))
   (retrieve-all
    ;; todo lol
    "select f.id as flight_id,f.ident,f.origin,f.dest,extract(epoch from f.dept_ts) as dept_ts,extract(epoch from f.arrive_ts) as arrive_ts,f.acft_type from flights f, airports a where a.contig_us = 1 and a.ident = f.origin")))

@export
(defun write-flights-js-file (&optional (fpath "/home/jackc/Projects/planesnwind/static/flights.js"))
  (with-open-file (out fpath
                       :direction :output
                       :if-exists :supersede)
    (format out "var Flights=~a;~%"
            (cl-json:encode-json-to-string
             (fetch-flights-and-paths))))
  (format t "-- wrote ~a successfully.~%" fpath))

(setf (ningle:route *app* "/flights.json")
      #'(lambda (params)
          (declare (ignore params))
          (set-api-headers)
          (cl-json:encode-json-to-string
           (fetch-flights-and-paths))))

@export
(defun fetch-flights-with-analysis ()
  (with-connection (curr-db-or-connect)
    (mapcar
     #'alexandria:plist-alist
     (retrieve-all
      "select id,ident,origin,dest,extract(epoch from dept_ts) as dept_ts,extract(epoch from arrive_ts) as arrive_ts,acft_type,hstore_to_json(meta) as meta from flights where meta != ''"
      ))))

@export
(defun fetch-spartan-fp (fid &optional (track-type 0))
  (with-connection (curr-db-or-connect)
    (mapcar
     #'(lambda (o) (list
                    (getf o :lon)
                    (getf o :lat)))
     (retrieve-all
      (format nil
              "select posn[0] as lon,posn[1] as lat from flight_tracks where flight_id = ~d and tracktype = ~d order by ts"
              fid track-type)

      ))))

(defun adjust-flight-alist (fl)
  (let ((meta-str (rest (assoc :meta fl)))
        (rest-of-flight-str
         (cl-json:encode-json-alist-to-string
          (remove :meta fl :key #'first))))
    (concatenate 'string
                 (subseq rest-of-flight-str 0 (- (length rest-of-flight-str) 1))
                 ",\"meta\":"
                 meta-str
                 "}")))

@export
(defun write-fpaths-js-file (&optional (fpath "/home/jackc/Projects/planesnwind/static/flightpaths.js"))
  (with-open-file (out fpath
                       :direction :output
                       :if-exists :supersede)
    (format out "var FlightAnalysis=[~%")
    (let* ((n 0)
          (m 0)
          (ids
           (loop for fl in (fetch-flights-with-analysis)
              collecting
                (progn
                  (format out "~a~a" (if (> (incf n) 1) "," "")
                          (adjust-flight-alist fl))
                  (rest (assoc :id fl))))))
      (format out "];~%")

      (format out "var FlightPaths={~%")
      (loop for fid in ids
         do
           (format out "~a\"~d\":[~a,~%~a,~%~a]"
                   (if (> (incf m) 1) "," "")
                   fid
                   (cl-json:encode-json-to-string
                    (fetch-spartan-fp fid))
                   (cl-json:encode-json-to-string
                    (fetch-spartan-fp fid 1))
                   (cl-json:encode-json-to-string
                    (fetch-spartan-fp fid 2))))
      (format out "};~%")))

  (format t "-- wrote ~a successfully.~%" fpath))

(defvar *orig-encoded-flightpath* "[]")
(defvar *current-encoded-flightpath* "[]")

@export
(defun set-orig-flightpath (posn-list)
  (setf *orig-encoded-flightpath*
        (cl-json:encode-json-to-string
         posn-list))
  (length posn-list))

@export
(defun set-current-flightpath (posn-list)
  (setf *current-encoded-flightpath*
        (cl-json:encode-json-to-string
         posn-list))
  (length posn-list))

(setf (ningle:route *app* "/flights/:fid")
      #'(lambda (params)
          (set-api-headers)

          (let ((fid (rest (assoc :fid params))))
            (cond
              ((string= fid "current")
               *current-encoded-flightpath*)
              ((string= fid "orig")
               *orig-encoded-flightpath*)
              (t
               (cl-json:encode-json-to-string
                (fetch-flightpath-by-flightid
                 (parse-integer (rest (assoc :fid params))))))))))

@export
(defun write-airports-js-file (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (format out "var Airports=~a;~%"
            (fetch-airports-resp-maybe-from-cache)))
  (format t "-- wrote ~a successfully.~%" filename))

(setf (ningle:route *app* "/airports.json")
      #'(lambda (params)
          (declare (ignore params))
          (set-api-headers)
          (fetch-airports-resp-maybe-from-cache)))

(defvar *winds-cache* nil)
(defun fetch-winds-resp-maybe-from-cache ()
  (or *winds-cache*
      (setf *winds-cache*
            (cl-json:encode-json-to-string
             (planesnwind.winds:dump-whole-wind-model)))))

@export
(defun write-winds-js-file (&optional (fpath "/home/jackc/Projects/planesnwind/static/winds.js"))
  (with-open-file (out fpath
                       :direction :output
                       :if-exists :supersede)
    (format out "var Winds=~a;~%"
            (fetch-winds-resp-maybe-from-cache)))
  (format t "-- wrote ~a successfully.~%" fpath))

(setf (ningle:route *app* "/flights.json")
      #'(lambda (params)
          (declare (ignore params))
          (set-api-headers)
          (cl-json:encode-json-to-string
           (fetch-flights-and-paths))))

#|
(setf (ningle:route *app* "/data/:vehicle")
      #'(lambda (params)
          (cl-json:encode-json-alist-to-string
           (fetch-full-update
            (rest (assoc :vehicle params))))))

(setf (ningle:route *app* "/data/:vehicle/:lastid")
      #'(lambda (params)
          (cl-json:encode-json-alist-to-string
           (fetch-full-update
            (rest (assoc :vehicle params))
            (rest (assoc :lastid params))))))
|#

(defparameter +clack-h+ nil)

@export
(defun start-http (&key (port 8802))
  (connect-db)
  (setf +clack-h+
        (clack:clackup (lack:builder
                        (:static :path "/public/"
                                 :root #P"/static-files/")
                        *app*)
                       :server :toot
                       :port port)))

@export
(defun stop-http ()
  (clack:stop +clack-h+))
