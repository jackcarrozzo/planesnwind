(in-package :cl-user)
(defpackage planesnwind.db
  (:use :cl
        :datafly
        :sxql
        :alexandria
        :planesnwind.util))
(in-package :planesnwind.db)
(cl-syntax:use-syntax :annot)

@export
(defparameter +db-name+ "planesnwind")

@export
(defparameter +db-user+ "pnw")

@export
(defparameter +db-pass+ "yaymath")

@export
(defparameter +db-port+ 5432)

(defparameter +dbconn+ nil)

@export
(defun connect-db (&key (wait-coef 10))
  (format t "--- connecting to db.~%")
  (let ((n 10))
    (loop while (not +dbconn+)
       do
         (handler-case
             (setf +dbconn+
                   (datafly:connect-cached :postgres
                                           :database-name +db-name+
                                           :username +db-user+
                                           :password +db-pass+
                                           :host "127.0.0.1"
                                           :port +db-port+))
           (error (c)
             (format t "!!! issue connecting to db, ~d tries left. sleeping: ~a~%"
                     (decf n) c)
             (if (= 0 n) (error "out of tries connecting to db.~%"))
             (sleep (* wait-coef (random 7.0)))
             (setf +dbconn+ nil))))))

(defmacro with-connection (conn &body body)
  `(let ((datafly:*connection* ,conn)
         (reconnected-times 0))
     (handler-bind
         ((cl-postgres:database-connection-error
           (lambda (err)
             (cond
               ((< reconnected-times 10)
                (format t "!!!! attempting db reconnect number ~d: ~a~%"
                        reconnected-times err)
                (sleep (* 6 reconnected-times))
                (incf reconnected-times)
                (invoke-restart :reconnect))
               (t
                (format t "!!!! tried too many times to reconnect, punting.~%")
                (setf +dbconn+ nil)
                (sleep 60)
                (error "db failed reconnect loop"))))))
       ,@body)))

(defun curr-db-or-connect ()
  (if +dbconn+
      +dbconn+
      (setf +dbconn+ (connect-db))))



(defvar *local-tz-offset-from-zulu* nil)


@export
(defun get-local-tz-offset ()
  (or
   *local-tz-offset-from-zulu*
   (setf *local-tz-offset-from-zulu*
         (find-offset-secs-for-tz planesnwind.util:+db-tz+))))

@export
(defun get-zulu-time ()
  (- (planesnwind.util:get-unix-time) (get-local-tz-offset)))



;;

;;; ------ hstore ------

;; table names can be "this" 'that or :theother, and
;;   will be lowercased
;; key names can be "this" 'that or :theother, and
;;   will be UPPERCASED

;; select slice(stuff,array['g']) from elk;
;; select stuff->'b' from elk;
;; select stuff->array['b', 'n'] from elk;

(defvar *debug* t)

;; (hstore-fetch :users :elk "id=2")
@export
(defun hstore-fetch (tablename key where-val &key (column "meta") (where-col "id"))
  (second
   (datafly:retrieve-one
    (format nil "SELECT ~a->'~a' FROM ~a WHERE ~a"
            column (string-upcase (string key))
            (string-downcase (string tablename))
            (format nil "~a=~a" where-col where-val)))))

;; (hstore-upsert :flights '((:moose . "11")(:elk-ya . 8.43598)) 34)

@export
(defun hstore-upsert (tablename kv-pairs where-val &key (column "meta") (where-col "id"))
  (with-connection (curr-db-or-connect)
    (datafly:execute
     (format nil "UPDATE ~a SET ~a=~a||~a WHERE ~a"
             (string-downcase (string tablename)) column column
             (format-hstore-pairs kv-pairs)
             (format nil "~a=~a" where-col where-val)))))

;; take dotted pairs of k.v - can be symbol, keyword sym, or str
;; return str like '"k1"=>"elk","k2"=>"v2",' (including singles)
(defun format-hstore-pairs (pairs)
  (concatenate 'string
               "'"
          (reduce
           (lambda (cur new) (concatenate 'string cur new))
           (map 'list
                (lambda (p)
                  ;; postgres doesnt mind trailing commas in kv pairs
                  (format nil "\"~a\"=>~a,"
                          (string-upcase (format nil "~a" (first p)))
                          (let ((vi (rest p)))
                            (cond
                              ((stringp vi)
                               (sql-sanitize (format nil "\"~a\"" vi)))
                              ((numberp vi)
                               (if (integerp vi)
                                   (format nil "~d" vi)
                                   (format nil "~6$" vi))) ;; todo:
                              ((not vi)
                               "\"false\"")
                              (t
                               (format t "!! fyi tried to fmt weird thing for hstore: ~a~%"
                                       (type-of vi))
                               (sql-sanitize (format nil "\"~a\"" vi)))))))
                pairs))
          "'"))

@export
(defun connect-db-2 (&key (debug))
  ;; (datafly:disconnect-toplevel)
  (when (nth-value
         1
         (ignore-errors
           (datafly:connect-toplevel :postgres
                                     :database-name +db-name+
                                     :username +db-user+
                                     :password +db-pass+
                                     :host "127.0.0.1")))
    (when (or *debug* debug)
      (format t "--- db already connected, continuing"))))

@export
(defun find-flight-to-optimize ()
  (with-connection (curr-db-or-connect)
    (getf
     (retrieve-one
      "select distinct t.flight_id as id from flight_tracks t, flights f, airports a where t.tracktype = 0 and not f.meta?'CRUISE-ALT' and t.flight_id = f.id and f.origin = a.ident and a.contig_us = 1 limit 1"
      ;;"select id from flights where (select count(id) from flight_tracks t where
      ;;flight_id = flights.id and tracktype = 0 limit 1) > 0 and  (select count(id)
      ;;from flight_tracks t where flight_id = flights.id and tracktype = 1 limit 1) = 0 limit 1")
      )
     :id)))

(defun k ()
  (with-connection (curr-db-or-connect)
    (retrieve-one
     "select id from flights where flights.meta?'CRUISE-ALT' order by random() limit 1")))

;;; ------------------------ airport helpers

(defvar *airport-posn-cache* (make-hash-table :test 'equal))

@export
(defun clear-apt-posn-cache ()
  (setf *airport-posn-cache*
        (make-hash-table :test 'equal)))

@export
(defun fetch-airport-posn (ident-str &key override-cache-p)
  (let* ((ident-up (string-upcase ident-str))
         (cache-r (gethash ident-up *airport-posn-cache*)))
    (cond
      (override-cache-p
       (setf (gethash ident-up *airport-posn-cache*)
             (fetch-airport-posn-from-db ident-up)))
      (cache-r
       cache-r)
      (t
       (setf (gethash ident-up *airport-posn-cache*)
             (fetch-airport-posn-from-db ident-up))))))

(defun fetch-airport-posn-from-db (ident-str)
  ;; select posn[0] as lon, posn[1] as lat from airports where ident = 'KABI';

  (or
   (with-connection (curr-db-or-connect)
     (retrieve-one
      (select
          ((:as '(:raw "posn[0]") :lon)
           (:as '(:raw "posn[1]") :lat))
        (from :airports)
        (where (:= :ident ident-str)) ;; todo: wouldnt hurt to sanitize this guy
        (limit 1))))
   (progn
     (format t "!!!! couldnt get posn for airport: '~a'~%" ident-str)
     nil)))

(defun airport-in-list-p (ident apt-list)
  ;; todo: make this nicer
  (let ((r nil))
    (loop for apt in apt-list
       do
         (when (string= ident (rest (assoc :ident apt)))
           (setf r t)
           (return t)))
    r))

;; todo:
@export
(defun g ()
  (let ((fid)
        (n 0))
    (loop while (and (not fid) (< n 100))
       do
         (multiple-value-bind (id ident)
             (select-rand-fid)
           (when
               (and id
                    (airport-in-contig-p ident)
                    (flight-has-track-p id)
                    (not (flight-has-track-p id 1)))
             (setf fid id))))
    fid))

@export
(defun airport-in-contig-p (ident)
  (with-connection (curr-db-or-connect)
    (getf
     (retrieve-one
      (format nil
              "select id from airports where contig_us = 1 and ident = '~a' limit 1"
              (sql-sanitize ident)))
     :id)))

@export
(defun fetch-contig-us-airports ()
  (with-connection (curr-db-or-connect)
    (retrieve-all-values
     "select ident from airports where contig_us=1 and ident ~ '^K*[A-Z]{3,4}$'")))

@export
(defun flight-has-track-p (fid &optional (track-type 0))
  (with-connection (curr-db-or-connect)
    (getf
     (retrieve-one
      (format nil
              "select id from flight_tracks where flight_id = ~d and tracktype = ~d limit 1"
              fid track-type))
     :id)))

@export
(defun select-rand-fid (&key (without-meta-p t))
  (with-connection (curr-db-or-connect)
    (let ((r (retrieve-one
              (format nil "select id,origin from flights ~a order by random() limit 1"
                      (if without-meta-p
                          "where meta = ''"
                          "")))))
      (values
       (getf r :id)
       (getf r :origin)))))

@export
(defun select-fids-with-tracks (&optional (tracktype 0))
  (with-connection (curr-db-or-connect)
    (retrieve-all-values
     (format nil "select distinct flight_id from flight_tracks where tracktype = ~d"
             tracktype))))

@export
(defun update-runstate-for-fid (fid runstate)
  (format t "--- updating runstate on fid ~a to ~a.~%" fid runstate)
  (with-connection (curr-db-or-connect)
    (retrieve-one
     (format nil "update flights set runstate = ~d where id = ~d"
             (round runstate) (round fid)))))

@export
(defun do-whole-airport-import ()
  (format t "-- fetching airports csv... ")

  (let ((airports-list (planesnwind.util::fetch-us-airports))) ;; todo:
    (format t "ok, got ~a objs.~%" (length airports-list))

    (loop for this-airport in airports-list
       do
         (let ((apt-ident (string-upcase (rest (assoc :ident this-airport)))))
           (cond
             ((cl-ppcre:scan "^[A-Z]{3}$" apt-ident)
              (if (airport-in-list-p (concatenate 'string "K" apt-ident) airports-list)
                  (format t "-- rejecting ~a because import also contains a K~a.~%" apt-ident apt-ident)
                  (insert-one-airport this-airport :ident-override (concatenate 'string "K" apt-ident))))
             (t
              (insert-one-airport this-airport)))))))

(defun insert-one-airport (apt-al &key ident-override)
  (let* ((elev-ft
         (handler-case (parse-integer (rest (assoc :elev-ft apt-al)))
           (t (c)
             (declare (ignore c))
             ;;(format t "Caught condition and continuing parsing int from '~a': ~a~%"
             ;;        (rest (assoc :elev-ft apt-al)) c)
             -1)))
         (ident-str (string-upcase (if ident-override
                                       ident-override
                                       (rest (assoc :ident apt-al))))))
    (getf
     (retrieve-one
      (insert-into :airports
        (set=
         :ident ident-str
         :name (sql-sanitize (rest (assoc :name apt-al)))
         :state (sql-sanitize (rest (assoc :state apt-al)))
         :posn (:raw (format nil "point(~a,~a)" ;; only works cuz strings
                             (sql-sanitize (rest (assoc :lon apt-al)))
                             (sql-sanitize (rest (assoc :lat apt-al)))))
         :elev_ft elev-ft
         :contig_us (rest (assoc :contig-48 apt-al)))
        (returning :id)))
     :id)))

#|
select count(id) from airports where contig_us=1 and posn[0] > -125.0 and posn[0] < -70.0 and posn[1] > 25.0 and posn[1] < 50.0;

|#

;;; ------------------------------- wind helpers

(defun fmt-time (time-int)
  (format nil "~4,'0d" time-int))

;; planesnwind.util:+db-tz+

@export
(defun remove-windset (wsid)
  (format t "-- heads up, removing windset ~a.~%" wsid)
  (with-connection (curr-db-or-connect)
    (retrieve-one-value
     (format nil "delete from wind_sets where id=~a"
             (round wsid)))))

@export
(defun create-wind-set (obs-meta &key fake-p (err-val 0))
  (if fake-p
      (let* ((at-ts (rest (assoc :at-ts obs-meta)))
             (ts-parts (multiple-value-list
                        (decode-universal-time (planesnwind.util:unix-to-universal-time
                                                at-ts))))
             (ts-hr (nth 2 ts-parts))
             (bounding-hrs
              (cond
                ((and (>= ts-hr 0) (< ts-hr 9)) '(0 9))
                ((and (>= ts-hr 9) (< ts-hr 18)) '(9 18))
                ((and (>= ts-hr 18) (< ts-hr 24)) '(18 24))))
             (day-ts (planesnwind.util:universal-to-unix-time
                      (encode-universal-time
                       0 0 0 (nth 3 ts-parts) (nth 4 ts-parts) (nth 5 ts-parts))))
             (start-ts (+ day-ts (* (first bounding-hrs) 60 60)))
             (end-ts (+ day-ts (* (second bounding-hrs) 60 60))))
        (create-wind-set (alexandria:plist-alist
                          (list
                           :valid-tz "Z"
                           :for-use-tz "Z"
                           :valid-day -1
                           :valid-time 0
                           :foruse-start 0
                           :foruse-end 0
                           :start-ts start-ts
                           :end-ts end-ts))
                         :err-val -1))
      (progn
        (unless (and (string= "Z" (rest (assoc :valid-tz obs-meta)))
                     (string= "Z" (rest (assoc :for-use-tz obs-meta))))
          (error "wind set gave back back unhandled tzs: ~a ~a"
                 (rest (assoc :valid-tz obs-meta))
                 (rest (assoc :for-use-tz obs-meta)))
          )
        (with-connection (curr-db-or-connect)
          (getf
           (retrieve-one
            (insert-into :wind_sets
              (set=
               :ts_fetched (:raw (format nil "to_timestamp(~a)"
                                         (round (get-zulu-time))))
               :valid_day (rest (assoc :valid-day obs-meta))
               :valid_time (fmt-time (rest (assoc :valid-time obs-meta)))
               :valid_tz (rest (assoc :valid-tz obs-meta))
               :foruse_start (fmt-time (rest (assoc :foruse-start obs-meta)))
               :foruse_end (fmt-time (rest (assoc :foruse-end obs-meta)))
               :foruse_tz (rest (assoc :for-use-tz obs-meta))
               :start_ts (:raw (format nil "to_timestamp(~a)"
                                       (round (rest (assoc :start-ts obs-meta)))))
               :end_ts (:raw (format nil "to_timestamp(~a)"
                                     (round (rest (assoc :end-ts obs-meta)))))
               :err (round err-val))
              (returning :id)))
           :id)))))

@export
(defun insert-obslist (wind-set-id obslist)
  (with-connection (curr-db-or-connect)
    (let ((n 0))
      (loop for this-obs in obslist
         do
           (let* (
                  ;;(posn-plist (pop this-obs))
                  ;;(posn-lat (getf posn-plist :lat))
                  ;;(posn-lon (getf posn-plist :lon))
                  (obs-site-ident (pop this-obs)))
             (loop for obs-level in this-obs
                do
                  (destructuring-bind (alt wind-dir wind-spd temp-c) obs-level
                    (retrieve-one
                     (insert-into :wind_set_data
                       (set=
                        :wind_set_id wind-set-id
                        :station_ident obs-site-ident
                        ;;:posn (:raw (format nil "point(~14$,~14$)" posn-lon posn-lat))
                        :alt alt
                        :wind_spd wind-spd
                        :wind_dir wind-dir
                        :temp_c temp-c)))
                    (incf n)))))
      n)))

@export
(defun select-windset-for-ts (ts)
  ;; use winds:find-wset-for-ts instead

  ;; select id,err from wind_sets where end_ts>to_timestamp(0) limit 1;
  (with-connection (curr-db-or-connect)
    (retrieve-one
     (select
         (:id
          :err)
       (from :wind_sets)
       (where (:and
               (:>= :end_ts (:raw (format nil "to_timestamp(~a)" (round ts))))
               (:<= :start_ts (:raw (format nil "to_timestamp(~a)" (round ts))))))
       (limit 1)))))

#|
@export
(defun select-winds-at-level (alt)
  (with-connection (curr-db-or-connect)
    (retrieve-all
     (select
         ((:as '(:raw "posn[0]") :lon)
          (:as '(:raw "posn[1]") :lat)
          (:as :wind_spd :wspd))
       (from :wind_set_data)
       (where (:and
               (:= :wind_set_id 3) ;; todo:
               (:= :alt alt)))))))
|#

;; for dependancies
(defparameter +our-wind-obs-alts+
  '(3000 6000 9000 12000 18000 24000 30000 34000 39000))

(defparameter +our-highest-wind-obs-alt+ (first (last +our-wind-obs-alts+)))

(defun our-bounding-wind-alts (alt)
  (let ((alt-i (round alt)))
    (cond
      ((< alt (first +our-wind-obs-alts+))
       `(-1 ,(first +our-wind-obs-alts+)))
      ((> alt +our-highest-wind-obs-alt+)
       `(,+our-highest-wind-obs-alt+ -1))
      (t
       (let ((list-r (find (round alt) +our-wind-obs-alts+)))
         (if list-r
             list-r
             (let ((lvl-n -1))
               (loop for lvl in +our-wind-obs-alts+
                  while (< lvl alt-i)
                  do
                    (incf lvl-n))
               (list
                (nth lvl-n +our-wind-obs-alts+)
                (nth (1+ lvl-n) +our-wind-obs-alts+)))))))))

#|
@export
(defun select-nearest-winds (pt-plist alt &key (num 4))
  (with-connection (curr-db-or-connect)
    (let* ((alts (our-bounding-wind-alts alt)))
      (cond
        ((integerp alts)
         `((,alts ,(get-nearest-winds-at-alt pt-plist alts :num num))))
        ((listp alts)
         (remove-if-not
          #'second
          (list
           (list (first alts) (get-nearest-winds-at-alt pt-plist (first alts) :num num))
           (list (second alts) (get-nearest-winds-at-alt pt-plist (second alts) :num num)))))
        (t
         (format t "!!! tried to select nearest winds for bad alt: ~a~%" alts)
         nil)))))
|#

#|
@export
(defun get-nearest-winds-at-alt (wset-id pt-plist alt &key (num 4))
  ;; select round((w.posn <@> point(-88.78143,42.84342))::numeric, 3) as
  ;;   dist,wind_spd,wind_dir from wind_set_data w where alt=24000order
  ;;   by dist asc limit 4;
  (with-connection (curr-db-or-connect)
    (loop for ob in
         (retrieve-all
          (select
              ((:as `(:raw ,(format nil "round((wind_set_data.posn <@> point(~10$,~10$))::numeric, 3)"
                                    (getf pt-plist :lon) (getf pt-plist :lat)))
                    :dist)
               (:as :wind_spd :wspd)
               (:as :wind_dir :wdir))
            (from :wind_set_data)
            (where
             (:and
              (:= :alt alt)
              (:= :wind_set_id wset-id)))
            (order-by :dist)
            (limit num)))
       collecting
       ;; this is annoying
         `(:dist ,(float (getf ob :dist))
                 :wdir ,(getf ob :wdir)
                 :wspd ,(getf ob :wspd)))))
|#

(defvar *cached-obslist-posns* nil)

@export
(defun fetch-all-obslist-posns ()
  (or *cached-obslist-posns*
      (setf *cached-obslist-posns*
            (with-connection (curr-db-or-connect)
              (retrieve-all
               "select ident,posn[0] as lon,posn[1] as lat from wx_sites")))))

(import 'planesnwind.util:euclidian-dist)

@export
(defun find-closest-windobs-posns (this-xlon this-ylat &optional (closest-num 4))
  (subseq
   (sort
    (mapcar
     #'(lambda (o)
         (list
          (euclidian-dist this-xlon this-ylat (getf o :lon) (getf o :lat))
          (getf o :ident)))
     (fetch-all-obslist-posns))
    #'< :key #'first)
   0 closest-num))

(defvar *cached-wset-obs* (make-hash-table :test 'equal))
(defvar *cached-wsets* (list))

(defun load-wind-obs-to-cache (wsid)
  (push wsid *cached-wsets*)
  (mapc #'(lambda (o)
            (setf (gethash (format nil "~a-~a-~a" wsid (getf o :station-ident) (getf o :alt)) *cached-wset-obs*)
                  (list (getf o :wind-spd) (getf o :wind-dir))))
        (with-connection (curr-db-or-connect)
          (retrieve-all
           (format nil
                   "select station_ident,alt,wind_spd,wind_dir from wind_set_data where wind_set_id = ~$"
                   (round wsid))))))

@export
(defun get-wind-obs-by-wsid-station-and-alt (wsid site-ident alt &key bail-p)
  (let* ((ht-key (format nil "~a-~a-~a" wsid site-ident alt))
         (ht-r (gethash ht-key *cached-wset-obs*)))

    ;;(format t "--- ~a: ~a~%" ht-key ht-r)
    (cond
      ((and bail-p (not ht-r))
       (format t "--- failed to get wsid ~a after loading to cache. key ~a.~%" wsid ht-key)
       (error "failed to get wsid ~a after loading to cache. key ~a.~%" wsid ht-key))
      ((not ht-r)
       (if (find wsid *cached-wsets*)
           (progn
             (format t "--- wsid ~a is supposedly cached but couldnt find key ~a.~%" wsid ht-key)
             (error "wsid ~a is supposedly cached but couldnt find key ~a.~%" wsid ht-key))
           (progn
             (format t "--- wsid ~a not cached, loading it and trying again.~%" wsid)
             (load-wind-obs-to-cache wsid)
             (get-wind-obs-by-wsid-station-and-alt wsid site-ident alt :bail-p t))))
      (t
       (list :wspd (first ht-r) :wdir (second ht-r))))))

@export
(defun get-closest-windobs-by-wsid-and-alt (wsid alt xlon ylat)
  (mapcar #'(lambda (o) (append (list :dist (first o)) (get-wind-obs-by-wsid-station-and-alt wsid (second o) alt)))
          (find-closest-windobs-posns xlon ylat)))

(defun clear-wx-caches ()
  (setf *cached-wset-obs* (make-hash-table :test 'equal))
  (setf *cached-obslist-posns* nil)
  (setf *cached-wsets* nil))

#|
;; tmp
(defun make-var-strs (winds)
  (let ((xylist "xyl=[")
        (vlist "vl=["))
    (loop for w in winds
       do
         (progn
           (setf xylist
                 (format nil "~a[~8$,~8$]," xylist (getf w :lon) (getf w :lat)))
           (setf vlist
                 (format nil "~a~a.0," vlist (getf w :wspd)))))
    (list
     (concatenate 'string (subseq xylist 0 (- (length xylist) 1)) "]")
     (concatenate 'string (subseq vlist 0 (- (length vlist) 1)) "]"))))
|#

@export
(defun save-wind-model-to-db (wind-set-id wind-model-ht)
  (let ((n 0)
        (start-ts (/ (float (get-internal-real-time)) internal-time-units-per-second)))
    (mapc
     #'(lambda (mset)
         (retrieve-one
          (format
           nil
           "insert into wind_model_data (wind_set_id,mkey,mval) values (~d,'~a',~4$)"
           wind-set-id (first mset) (rest mset)))
         (incf n))
     (alexandria:hash-table-alist wind-model-ht))

    (let ((e-s
           (- (/ (float (get-internal-real-time)) internal-time-units-per-second)
              start-ts)))
      (format t "-- saved ~d wind model pts to db in ~3$ sec.~%" n e-s)
      (values
       n
       e-s))))

;;; ------------------------------- flight helpers

@export
(defun create-flight (flight-alist)
  (with-connection (curr-db-or-connect)
    (let ((f-ident (rest (assoc :ident flight-alist)))
          (origin (rest (assoc :origin flight-alist)))
          (dest (rest (assoc :destination flight-alist))))
      (if (or (not (stringp origin))
              (not (stringp dest))
              (< (min (length dest) (length origin)) 3)
              (> (max (length dest) (length origin)) 4))
          (progn
            (format t "!!! -- rejecting flight due to origin and dest constraints: ~a~%"
                    flight-alist)
            (error "create-flight failed due to origin and dest constraints"))
          (cond
            ((or (not (rest (assoc :actualdeparturetime flight-alist)))
                 (not (rest (assoc :actualarrivaltime flight-alist))))
             (format t "!!! rejecting flight creation cuz missing times.~%")
             nil)
            (t
             (list f-ident
                   (getf
                    (retrieve-one
                     (insert-into :flights
                       (set=
                        :ident f-ident
                        :origin (rest (assoc :origin flight-alist))
                        :dest (rest (assoc :destination flight-alist))
                        :dept_ts (:raw (format nil "to_timestamp(~a)"
                                               (round (rest (assoc :actualdeparturetime flight-alist)))))
                        :arrive_ts (:raw (format nil "to_timestamp(~a)"
                                                 (round (rest (assoc :actualarrivaltime flight-alist)))))
                        :acft_type (rest (assoc :aircrafttype flight-alist))
                        :meta "")
                       (returning :id)))
                    :id))))))))

@export
(defvar *contig-us-apt-idents* nil)

@export
(defun new-select-flight-to-run ()
  (with-connection (curr-db-or-connect)
    (retrieve-one-value
     "select id from flights where runstate=0 or runstate=1 order by random() limit 1")))

@export
(defun find-fid-ready-to-process (&key (set-status-p t))
  (with-connection (curr-db-or-connect)
    (let ((fid
           (retrieve-one-value
            "select id from flights where not exist(meta,'BEST-SEG-D') or meta is null order by random() limit 1")))
      (when set-status-p
        (hstore-upsert :flights '((:processing . "true")) fid))
      fid)))

@export
(defun apt-ident-in-contig-us-p (ident-str)
  (when (not *contig-us-apt-idents*)
    (with-connection (curr-db-or-connect)
      (setf *contig-us-apt-idents*
            (retrieve-all-values
             "select ident from airports where contig_us=1 and ident like 'K%'"))
      (format t "-- didnt yet have contig us apts cached; loaded ~d.~%" (length *contig-us-apt-idents*))))
  (find ident-str *contig-us-apt-idents* :test #'string=))

@export
(defun flight-in-db-p (fl-alist)
  (with-connection (curr-db-or-connect)
    (retrieve-one-value
     (format
      nil
      "select id from flights where extract(epoch from dept_ts)=~d and ident='~a' limit 1"
      (- (rest (assoc :actualdeparturetime fl-alist)) 14400) ;; todo: tzs
      (sql-sanitize (rest (assoc :ident fl-alist)))))))

@export
(defun flight-track-in-db-p (flight-db-id &optional (track-type 1))
  (with-connection (curr-db-or-connect)
    (if
     (retrieve-one
      (format
       nil
       "select id from flight_tracks where flight_id = ~d and tracktype = ~d limit 1"
       flight-db-id track-type))
     t)))

@export
(defun get-flight-details-for-fid (fid)
  (with-connection (curr-db-or-connect)
    (retrieve-one
     (format nil
             "select ident,origin,dest,extract(epoch from dept_ts) as dept_ts,extract(epoch from arrive_ts) as arrive_ts,acft_type,runstate,meta from flights where id = ~d limit 1"
             (round fid)))))

@export
(defun fetch-flightpath-by-flightid (fid)
  (with-connection (curr-db-or-connect)
    (mapcar
     #'alexandria:plist-alist
     (retrieve-all
      (format
       nil
       ;; todo:
       "select extract(epoch from ts) as tss,posn[0] as lon,posn[1] as lat,alt,gspd from flight_tracks where flight_id = ~d and tracktype = 0 order by tss"
       fid)))))
;; from :http

@export
(defun new-find-flights-to-maybe-fetch (&optional (num 10) use-old-way-p)
  (if use-old-way-p
      (let* ((zulu-hr-min
              (subseq (multiple-value-list
                       (decode-universal-time
                        (planesnwind.util:unix-to-universal-time
                         (get-zulu-time))))
                      1 3))
             (fl-hr (+ (float (second zulu-hr-min)) (/ (first zulu-hr-min) 60.0))))
        (format t "z hr is ~a~%" fl-hr)
        (with-connection (curr-db-or-connect)
          (retrieve-all-values
           (format nil
                   ;;"select ident from known_flights where avg_hr < ~2$ and last_fetched_ts<(now() -interval '24 hours') and fetchstate=0 limit ~d"
                   ;; todo: timezone
                   "select ident from known_flights where (try_again_after_ts is null or try_again_after_ts < ~d) and last_fetched_ts<((now()+interval '4 hours') -interval '24 hours') and fetchstate=0 limit ~d"
                   fl-hr (round num)))))
      (find-and-lock-known-flights-maybe num)))

@export
(defun find-and-lock-known-flights-maybe (&optional num)
  (with-connection (curr-db-or-connect)
    (retrieve-all-values
     (format nil
             "with sid as (select ident from known_flights where (try_again_after_ts is null or try_again_after_ts < to_timestamp(~$)) and fetchstate=0 order by random() limit ~d) update known_flights k set fetchstate=1 from sid where k.ident=sid.ident returning k.ident"
            ;; "with sid as (select ident from known_flights where (try_again_after_ts is null or try_again_after_ts < (now() + '4 hours'::interval)) and last_fetched_ts<((now()+interval '4 hours') -interval '24 hours') and fetchstate=0 order by random() limit ~d) update known_flights k set fetchstate=1 from sid where k.ident=sid.ident returning k.ident"
             (get-zulu-time)
             (if num (round num) 1)))))

@export
(defun nab-rand-kf ()
  (with-connection (curr-db-or-connect)
    (retrieve-one-value
     "select ident from known_flights order by random() limit 1")))

#|
@export
(defun find-flights-to-maybe-fetch (&optional (num 10))
  ;; select ident from flights where ident like 'DAL%' or ident like 'JBU%' or ident like 'AAL%' and arrive_ts < NOW() - '1 day'::INTERVAL order by random() limit 10
  (with-connection (curr-db-or-connect)
    (retrieve-all-values
     (format nil
             "select ident from flights where ident like 'DAL%' or ident like 'JBU%' or ident like 'SWA%' or ident like 'UAL%' or ident like 'RPA%' or ident like 'AAL%' and arrive_ts < NOW() - ('1 day'::INTERVAL + '2 hours'::INTERVAL) order by random() limit ~d"
             num))))
|#

@export
(defun insert-flight-track (flight-id posn-list &key (track-type 1) (overwrite-p t))
  (if (not (and posn-list (> (length posn-list) 0)))
      (format t "!!! cant insert empty posn list as track for fid ~a!~%" flight-id)
      (with-connection (curr-db-or-connect)
        (let ((lat-key :latitude)
              (lon-key :longitude)
              (missing-ts (not (assoc :timestamp (first posn-list)))))
          (declare (ignore missing-ts))
          (when (not (and (assoc lat-key (first posn-list))
                          (assoc lon-key (first posn-list))))
            (cond
              ((and (assoc :lat (first posn-list))
                    (assoc :lon (first posn-list)))
               (setf lat-key :lat)
               (setf lon-key :lon))
              ((and (assoc :ylat (first posn-list))
                    (assoc :xlon (first posn-list)))
               (setf lat-key :ylat)
               (setf lon-key :xlon))
              (t
               (error "cant infer lat and lon gets from posn: ~a" (first posn-list)))))
          (with-connection (curr-db-or-connect)
            (when overwrite-p
              (retrieve-one
               (format nil "delete from flight_tracks where flight_id = ~d and tracktype = ~d"
                       flight-id track-type)))
            (let ((n 0))
              (loop for this-posn in posn-list
                 do
                   (progn
                     (retrieve-one
                      (insert-into :flight_tracks
                        (set=
                         :flight_id flight-id
                         :tracktype track-type
                         :ts (:raw (format nil "to_timestamp(~a)" (or
                                                                   (rest (assoc :timestamp this-posn))
                                                                   0)))
                         :posn (:raw (format nil "point(~14$,~14$)"
                                             (rest (assoc lon-key this-posn))
                                             (rest (assoc lat-key this-posn))))
                         :alt (round (or (rest (assoc :altitude this-posn)) -1))
                         :gspd (round (or (rest (assoc :groundspeed this-posn)) -1)))
                        (returning :id)))
                     (incf n)))
              n))))))

(defun delete-flight (fid)
  (with-connection (curr-db-or-connect)
    (retrieve-one-value
     (format
      nil
      "delete from flights where id=~a"
      fid))))

(defun fix-fs (fs)
  (let ((fids '())
        (sorted-fs
         (sort fs
          #'(lambda (a b)
              (> (reduce #'+
                         (loop for k in '(:track0-len :track1-len :track2-len)
                            collecting
                              (getf a k)))
                 (reduce #'+
                         (loop for k in '(:track0-len :track1-len :track2-len)
                            collecting
                              (getf b k))))))))
    (format t "keeping id ~a meta ~a~%" (getf (first sorted-fs) :id)
            (getf (first sorted-fs) :kv))

    (loop for i from 1 below (length sorted-fs)
       do
         (let ((dup (nth i sorted-fs)))
           (format t "      tossing id ~a meta ~a~%"
                   (getf dup :id)
                   (getf dup :kv))
           (push (getf dup :id) fids)))
    fids))

(defun count-ft-pts (fid &optional (track-type 0))
  (with-connection (curr-db-or-connect)
    (retrieve-one-value
     (format
      nil
      "select count(id) from flight_tracks where flight_id=~a and tracktype=~a"
      fid track-type))))

(defun find-dup-flight-set (d)
  (with-connection (curr-db-or-connect)
    (let ((tv (format nil "~0$" (getf d :ts))))
      (setf tv (subseq tv 0 (- (length tv) 1)))
      (let ((ds
             (retrieve-all
              (format
               nil
               "select id,ident,origin,dest,extract(epoch from dept_ts) as dts,extract(epoch from arrive_ts) as ats,slice(meta, ARRAY['BEST-SEG-D', 'CONSTR-BEST-SEG-D']) as kv from flights where ident='~a' and extract(epoch from dept_ts)=~d"
               (sql-sanitize (getf d :ident))
               tv))))
        (loop for dmem in ds
           collecting
             (let* ((fid (getf dmem :id)))
               (append
                (list :track0-len (count-ft-pts fid 0)
                      :track1-len (count-ft-pts fid 1)
                      :track2-len (count-ft-pts fid 2))
                dmem)))))))

(defun find-dup-flights-and-tracks ()
  (with-connection (curr-db-or-connect)
    (let* ((dups
            (retrieve-all
             "select ident,extract(epoch from dept_ts) as ts from flights group by dept_ts, ident having count(*)>1"))
           )
      dups)))


#|

 pg_dump -h "127.0.0.1" -U pnw planesnwind >db1.pgsql

# as postgres
5  createuser pnw
6  createdb -O pnw planesnwind
9  psql -U postgres planesnwind <db1.pgsql

echo "alter user pnw with password 'yaymath'"|psql -U postgres planesnwind
echo "alter role pnw with login"|psql -U postgres planesnwind

|#

@export
(defun find-offset-secs-for-tz (tz-str)
  (with-connection (curr-db-or-connect)
    (let ((r
           (retrieve-one-value
            (format
             nil
             "select extract(epoch from utc_offset) from pg_timezone_names where name='~a' limit 1"
             (sql-sanitize tz-str)))))
      (if (numberp r)
          (round r)
          nil))))


;;;;; ---------- workers

@export
(defun update-db-worker-state (sstr tstr &optional (infotxt ""))
  (with-connection (curr-db-or-connect)
    (let ((r
           (retrieve-one-value
            (format
             nil
             "select id from worker_state where wname='~a' and wstate='~a' limit 1"
             (sql-sanitize sstr) (sql-sanitize tstr)))))
      (retrieve-one-value
       (if r
           (format
            nil
            "update worker_state set last_update_ts=to_timestamp(~d), last_touch_ts=to_timestamp(~d), info='~a' where id = ~d returning id"
            (get-zulu-time) (get-zulu-time) (sql-sanitize infotxt) r)
           (format
            nil
            "insert into worker_state (wname,wstate,last_update_ts,last_touch_ts,info) values ('~a','~a',to_timestamp(~d),to_timestamp(~d),'~a') returning id"
            (sql-sanitize sstr) (sql-sanitize tstr)
            (get-zulu-time) (get-zulu-time) (sql-sanitize infotxt)))))))

@export
(defun worker-log-to-db (namestr tagstr txt &optional (fval -1.0))
  (with-connection (curr-db-or-connect)
    (retrieve-one-value
     (format
      nil
      "insert into worker_logs (wname,ltag,ts,val,txt) values ('~a','~a',to_timestamp(~d),~4$,'~a') returning id"
      (sql-sanitize namestr) (sql-sanitize tagstr)
      (get-zulu-time) (or fval -1.0) (sql-sanitize txt)))))

@export
(defun worker-get-db-conf-key (datkey)
  ;; you can pass symbols as keys yo
  (with-connection (curr-db-or-connect)
    (retrieve-one-value
     (format
      nil
      "select confval from worker_conf where confkey = '~a' limit 1"
      (string-upcase (sql-sanitize (format nil "~a" datkey)))))))

@export
(defun update-known-flight-set-check-after (ts ident)
  (with-connection (curr-db-or-connect)
    (retrieve-one-value
     (format
      nil
      "update known_flights set try_again_after_ts = to_timestamp(~d) where ident = '~a'"
      (round ts) (string-upcase (sql-sanitize ident))))))

@export
(defun update-known-flight-set-fetchstate (fetchstate ident &optional update-last-fetched-ts-p null-checkafter-p)
  (let ((v (if null-checkafter-p ", try_again_after_ts = null" "")))
    (with-connection (curr-db-or-connect)
      (retrieve-one-value
       (if update-last-fetched-ts-p
           (format
            nil
            "update known_flights set fetchstate = ~d, last_fetched_ts = to_timestamp(~d) ~a where ident = '~a'"
            (round fetchstate) (round (get-zulu-time)) v (string-upcase (sql-sanitize ident)))
           (format
            nil
            "update known_flights set fetchstate = ~d ~a where ident = '~a'"
            (round fetchstate) v (string-upcase (sql-sanitize ident))))))))

@export
(defun insert-known-flight (ident origin dest)
  (with-connection (curr-db-or-connect)
    (retrieve-one-value
     (format
      nil
      " insert into known_flights(ident,origin,dest,avg_hr,last_fetched_ts,fetchstate) values ('~a','~a','~a',0.0,to_timestamp(0),0) on conflict do nothing"
      (string-upcase (sql-sanitize ident)) (string-upcase (sql-sanitize origin)) (string-upcase (sql-sanitize dest))))))

@export
(defun count-known-flights ()
  (with-connection (curr-db-or-connect)
    (retrieve-one-value
     "select count(*) from known_flights")))

#|
planesnwind=> create table known_flights as select distinct ident,origin,dest,avg(extract(hour from arrive_ts)+(extract(minute from arrive_ts)/60.0)) as avg_hr,to_timestamp(0) as last_fetched_ts,0 as fetchstate,hstore('') as meta from flights where origin like 'K%' and dest like 'K%' group by ident,origin,dest;
SELECT 1535

|#


;; select avals(slice(meta, ARRAY['GC-SEG-D','BEST-SEG-SECS','ORIG-SEG-SIM-SECS','CONSTR-BEST-SEG-SECS'])) from flights where exist(meta,'BEST-SEG-SECS')

@export
(defun write-results-js-file (&optional (fpath "/home/jackc/Projects/planesnwind/static/results.js"))
  (format t "-- asking db nicely for numbers...~%")

  (let ((r
         (with-connection (curr-db-or-connect)
           (retrieve-all-values
            "select avals(slice(meta, ARRAY['GC-SEG-D','BEST-SEG-SECS','ORIG-SEG-SIM-SECS','CONSTR-BEST-SEG-SECS'])) from flights where exist(meta,'BEST-SEG-SECS')"))))
    (format t "--- got db results, ~d pts, writing file...~%" (length r))
    (with-open-file (out fpath
                         :direction :output
                         :if-exists :supersede)
      (format out "var Results=~a;~%"
              (cl-json:encode-json-to-string r))))
  (format t "-- wrote ~a successfully.~%" fpath))
