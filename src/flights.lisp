(in-package :cl-user)
(defpackage planesnwind.flights
  (:use :cl
        :datafly
        :sxql
        :alexandria
        :planesnwind.util))
(in-package :planesnwind.flights)
(cl-syntax:use-syntax :annot)


;; curl -u jackc:1c932851...2c61b147 'http://flightxml.flightaware.com/json/FlightXML2/GetLastTrack?ident=UAL779'
;; https://flightaware.com/commercial/flightxml/explorer/
;; http://flightxml.flightaware.com/soap/FlightXML2/doc#op_GetLastTrack
;; http://flightxml.flightaware.com/soap/FlightXML2/doc#type_TrackStruct

;;
;; for each KSFO etc
;;   walk this fucker to get arrivals in last 24hr:
;;     curl -u jackc:1c9328518...ccd80eb51962c61b147 'http://flightxml.flightaware.com/json/FlightXML2/Arrived?airport=KBOS&howMany=15&filter=airline'
;;
;; about each flight, ask:
;;
;; curl -u jackca:1c9328518...ccd80eb51962c61b147 'http://flightxml.flightaware.com/json/FlightXML2/GetLastTrack?ident=UAL779'

(push '("application" . "json") drakma:*text-content-types*)

;;(defparameter +test-airports+ '("KSFO" "KBOS" "KDEN" "KORD"))
(defparameter +test-airports+ '("KMSN"))

(defparameter +fa-username+ "jackc")
(defparameter +fa-key+ "1c9328518aaaaaaaaaccd80eb51962c61b147")
(defparameter +fa-url-base+ "http://flightxml.flightaware.com/json/FlightXML2")
(defparameter +fa-resp-len+ 15)

(defun request-fa-arrivals (airport-code offset)
  (let ((req-url (format nil "~a/Arrived?airport=~a&howMany=15&filter=airline&offset=~a"
                         +fa-url-base+ airport-code offset))
        (req-ba `(,+fa-username+ ,+fa-key+)))
    (format t "-- fetching fa arrivals for ~a: offset ~a.~%"
            airport-code offset)

    (multiple-value-bind (response-data response-code)
        (drakma:http-request req-url :basic-authorization req-ba)
      (ecase response-code
        (200
         (let* ((robj (cl-json:decode-json-from-string response-data))
                (actual-robj (rest (assoc :*ARRIVED-RESULT robj)))
                (next-offset (rest (assoc :next--offset actual-robj)))
                (flights-list (rest (assoc :arrivals actual-robj))))

           ;;(format t "-- offset: ~a, next-offset ~a, got ~a objs.~%"
           ;;        offset next-offset (length flights-list))

           (if (or (< (length flights-list) +fa-resp-len+)
                   (= -1 next-offset))
               flights-list
               (append flights-list
                       (request-fa-arrivals airport-code next-offset)))))))))

(defun request-fa-track (flight-ident)
  (let ((req-url (format nil "~a/GetLastTrack?ident=~a"
                         +fa-url-base+ flight-ident))
        (req-ba `(,+fa-username+ ,+fa-key+)))
    (multiple-value-bind (response-data response-code)
        (drakma:http-request req-url :basic-authorization req-ba)
      (ecase response-code
        (200
         (let* ((robj (cl-json:decode-json-from-string response-data))
                (actual-robj (rest (assoc :*GET-LAST-TRACK-RESULT robj)))
                (posn-list (rest (assoc :data actual-robj))))
           posn-list))))))

(defun fetch-and-add-flights (apt-ident)
  (mapcar #'planesnwind.db:create-flight
          (remove-if-not
           #'(lambda (f)
               (and
                (planesnwind.db:apt-ident-in-contig-us-p
                 (rest (assoc :origin f)))
                (planesnwind.db:apt-ident-in-contig-us-p
                 (rest (assoc :destination f)))
                (let ((r (planesnwind.db:flight-in-db-p f)))
                  (when r
                    (format t "-- ~a at ~a already in db: fid ~a.~%"
                            (rest (assoc :ident f))
                            (rest (assoc :actualdeparturetime f))
                            r))
                  (not r))))
           (request-fa-arrivals apt-ident 0))))

(defun fetch-and-add-tracks (ident-id-pairs)
  (let ((n 0))
    (loop for pair in ident-id-pairs
       do
         (if (and (listp pair)
                  (= 2 (length pair)))
             (destructuring-bind (flight-ident flight-db-id) pair
               (format t "-- fetching ~d/~d inserting track for ~a ~a... "
                       n (length ident-id-pairs) flight-db-id flight-ident)

               (if (planesnwind.db:flight-track-in-db-p flight-db-id 0)
                   (format t "already has a track in db, skipping.~%")

                   (let ((tr (request-fa-track flight-ident)))
                     (cond
                       ((and tr
                             (> (length tr) 0))
                        (planesnwind.db:insert-flight-track flight-db-id
                                                            tr
                                                            :track-type 0)
                        (format t "ok, ~d pts.~%" (length tr)))
                       (t
                        (format t "empty track returned, skipping.~%")))

                     (incf n))))
             (progn
               (format t "!!! couldnt insert flight with bad pair: '~a'~%" pair)
               nil)))
    n))


;;; yeah yeah this one

@export
(defun fetch-and-add-flights-and-tracks (apt-arr-ident)
  (fetch-and-add-tracks
   (fetch-and-add-flights apt-arr-ident)))



;;;;;;;;;;;;;;;;;;;;;;; api sux, the world runs on scraping

(defparameter +scr-url-base+ "https://flightaware.com/live/flight")
(defparameter +ua+ "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.67 Safari/537.36")

(defun fetch-flight-json (flight-ident)
  (plump:parse
   (multiple-value-bind (body resp-code resp-headers)
       (drakma:http-request (format nil "~a/~a" +scr-url-base+ flight-ident)
                            :user-agent +ua+
                            :connection-timeout 10)
     (declare (ignore resp-headers))

     (planesnwind.worker:worker-log "fa-pg-scr-req" flight-ident (float resp-code))
     (ecase resp-code
       (200
        ;;(format t "--- body len ~a, headers: ~a~%" (length body) resp-headers)
        ;;(format t "~a~%" body)

        body)))))

(defun parse-page (p)
  (let ((filtered-p (remove-if-not
                      #'(lambda (r) (cl-ppcre:scan "trackpollBootstrap" r))
                      (lquery:$ p "script" (text)))))

    (unless (= 1 (length filtered-p))
      (planesnwind.worker:worker-log "fa-pg-scr-parse-failed" (format nil "should be len 1, but got ~d"
                                                                      (length filtered-p)))
      (error "cant handle parsed page: ~d fitting scripts.~%" (length filtered-p)))

    (let ((stxt (elt filtered-p 0)))
      (subseq stxt 25 (- (length stxt) 1)))))

(defun write-json-file (js &optional (fpath "/tmp/yar.json"))
  (with-open-file (out fpath
                       :direction :output
                       :if-exists :supersede)
    (princ js out)
    (format out "~%"))
  (format t "-- wrote ~a successfully.~%" fpath))

;; America/New_York

(defun find-zulu-times (orig-tz dest-tz orig-ts dest-ts)
  (let ((orig-offset (planesnwind.db:find-offset-secs-for-tz (subseq orig-tz 1))) ;; colons????
        (dest-offset (planesnwind.db:find-offset-secs-for-tz (subseq dest-tz 1))))

    (format t "---- ~a --> ~a, ~a --> ~a~%" orig-tz orig-offset dest-tz dest-offset)
    (when (not (and orig-offset dest-offset))
      (error "cant handle tzs!"))

    (list
     (- orig-ts orig-offset)
     (- dest-ts dest-offset))))

@export
(defun fetch-and-add-scr-path (flight-ident) ;; todo: set and unset fetchstate and ts vals, unwind protect?
  (let* ((flobj
          (cl-json:decode-json-from-string
           (parse-page
            (fetch-flight-json flight-ident))))
         (fl-key-sym (first (first (rest (assoc :flights flobj)))))
         (act-flobj (rest (first (rest (assoc :flights flobj)))))
         (recent-fl-list
          (rest (assoc :flights
                       (rest (assoc :activity-log
                                    act-flobj)))))
         (waypts-list (rest (assoc :waypoints act-flobj)))
         (path-list (rest (assoc :track act-flobj)))
         (fl-plan-obj (rest (assoc :flight-plan act-flobj))))
    (format t "-- ~a :: fl list ~d items, pathlen ~d pts, waypts ~d fstatus ~a~%"
            fl-key-sym (length recent-fl-list)
            (length path-list) (length waypts-list)
            (rest (assoc :flight-status act-flobj)))
    ;;(format t "~a~%" recent-fl-list)

    (if (or (not (rest (assoc :flight-status act-flobj)))
            (not (string= "arrived" (rest (assoc :flight-status act-flobj)))))
        (progn
          (let ((will-arrive-ts
                 (+ (* -1 (planesnwind.db:get-local-tz-offset))
                  (max
                   (+ (or (rest (assoc :departure fl-plan-obj)) 0)
                      (or (rest (assoc :ete fl-plan-obj)) 0))
                   (or (rest (assoc :scheduled (rest (assoc :gate-arrival-times act-flobj)))) 0)
                   (or (rest (assoc :estimated (rest (assoc :gate-arrival-times act-flobj)))) 0)
                   (or (rest (assoc :actual (rest (assoc :gate-arrival-times act-flobj)))) 0)
                   (or (rest (assoc :scheduled (rest (assoc :landing-times act-flobj)))) 0)
                   (or (rest (assoc :estimated (rest (assoc :landing-times act-flobj)))) 0)
                   (or (rest (assoc :actual (rest (assoc :landing-times act-flobj)))) 0)))))
            (planesnwind.db:update-known-flight-set-check-after (+ (* 60 60) will-arrive-ts)
                                                 (rest (assoc :ident act-flobj)))
            (planesnwind.worker:worker-log "fa-pg-scr-not-arrived-yet"
                                           (format nil "~a should arrive by ~a zulu, will check back after ~a."
                                                   (rest (assoc :ident act-flobj))
                                                   will-arrive-ts (+ (* 60 60) will-arrive-ts)))
            (format t "---- ~a hasnt arrived yet, updating check-after in known flights.~%"
                    (rest (assoc :ident act-flobj))))

          (error "flight ~a hasnt arrived yet" (rest (assoc :ident act-flobj))))
        (destructuring-bind (dept-ts arr-ts)
            (find-zulu-times
             ":America/New_York" ":America/New_York"
             (or (rest (assoc :actual (rest (assoc :takeoff-times act-flobj))))
                 (rest (assoc :estimated (rest (assoc :takeoff-times act-flobj))))
                 (rest (assoc :scheduled (rest (assoc :takeoff-times act-flobj)))))
             (or (rest (assoc :actual (rest (assoc :landing-times act-flobj))))
                 (rest (assoc :estimated (rest (assoc :landing-times act-flobj))))
                 (rest (assoc :scheduled (rest (assoc :landing-times act-flobj))))))
          (let ((create-fl-arg
                 (alexandria:plist-alist
                  (list
                   :ident (rest (assoc :ident act-flobj))
                   :origin (rest (assoc :icao (rest (assoc :origin act-flobj))))
                   :destination (rest (assoc :icao (rest (assoc :destination act-flobj))))
                   :actualdeparturetime dept-ts
                   :actualarrivaltime arr-ts
                   :aircrafttype (rest (assoc :type (rest (assoc :aircraft act-flobj)))))))
                (fl-hstore-arg
                 (alexandria:plist-alist
                  (list
                   :acft-heavy (rest (assoc :heavy (rest (assoc :aircraft act-flobj))))
                   :cancelled (rest (assoc :cancelled act-flobj))
                   :diverted (rest (assoc :diverted act-flobj))
                   :specific-fa-id (rest (assoc :flight-id act-flobj))
                   :fplan-alt-fl (rest (assoc :altitude fl-plan-obj))
                   :fplan-dept-ts (rest (assoc :departure fl-plan-obj))
                   :fplan-fuel-gal (rest (assoc :gallons (rest (assoc :fuel-burn fl-plan-obj))))
                   :fplan-fuel-lbs (rest (assoc :pounds (rest (assoc :fuel-burn fl-plan-obj))))
                   :fplan-route (rest (assoc :route fl-plan-obj))
                   :fplan-spd (rest (assoc :speed fl-plan-obj))
                   :flight-status (rest (assoc :flight-status act-flobj))
                   :ga-flight (rest (assoc :ga act-flobj))
                   :pathlen (length path-list))))
                (ready-pathlist (mapcar
                                 #'(lambda (o)
                                     (alexandria:plist-alist
                                      (list
                                       :timestamp (rest (assoc :timestamp o))
                                       :latitude (second (rest (assoc :coord o)))
                                       :longitude (first (rest (assoc :coord o)))
                                       :altitude (rest (assoc :alt o))
                                       :groundspeed (rest (assoc :gs o)))))
                                 path-list))
                (best-dept-ts (or (rest (assoc :actual (rest (assoc :takeoff-times act-flobj))))
                                  (rest (assoc :estimated (rest (assoc :takeoff-times act-flobj))))
                                  (rest (assoc :scheduled (rest (assoc :takeoff-times act-flobj))))))
                (best-arr-ts (or (rest (assoc :actual (rest (assoc :landing-times act-flobj))))
                                 (rest (assoc :estimated (rest (assoc :landing-times act-flobj))))
                                 (rest (assoc :scheduled (rest (assoc :landing-times act-flobj)))))))

            ;; for me at least, tzs returned are always America/New_York

            (format t "tzs given: ~a to ~a~%"
                    (rest (assoc :+tz+ (rest (assoc :origin act-flobj))))
                    (rest (assoc :+tz+ (rest (assoc :destination act-flobj)))))
            (format t "given dept ts ~a ~a, arr ts ~a ~a~%"
                    (planesnwind.util:ts-to-human best-dept-ts) best-dept-ts
                    (planesnwind.util:ts-to-human best-arr-ts) best-arr-ts)
            (format t "given timestamp: ~a ~a~%"
                    (planesnwind.util:ts-to-human (rest (assoc :timestamp act-flobj)))
                    (rest (assoc :timestamp act-flobj)))
            (format t "fplan dept ts: ~a ~a~%"
                    (planesnwind.util:ts-to-human (rest (assoc :departure fl-plan-obj)))
                    (rest (assoc :departure fl-plan-obj)))
            (let ((times (mapcar #'(lambda (o) (rest (assoc :timestamp o))) ready-pathlist)))
              (format t "pathlist times ~a ~a to ~a ~a~%"
                      (planesnwind.util:ts-to-human (apply #'min times)) (apply #'min times)
                      (planesnwind.util:ts-to-human (apply #'max times)) (apply #'max times)))

            (format t "calculated zulu flight times: dept ~a ~a, arr ~a ~a.~%"
                    (planesnwind.util:ts-to-human dept-ts) dept-ts
                    (planesnwind.util:ts-to-human arr-ts) arr-ts)

            (format t "---- flight ~a looks like it worked, setting fetchstate 0, clearing check after, setting last checked.~%" (rest (assoc :ident act-flobj)))

            (planesnwind.db:update-known-flight-set-fetchstate
             0
             (rest (assoc :ident act-flobj)) t t)

            (format t "creating flight: ~%~a~%" create-fl-arg)
            (let ((r (planesnwind.db:create-flight create-fl-arg)))
              (if (not r)
                  (progn
                    (format t "!! flight creation failed!~%")
                    (error "failed to create flight in db!"))

                  (let ((fl-ident (first r))
                        (fid (second r)))

                    (format t "ok, flight ~a created as fid ~d.~%" fl-ident fid)

                    (format t "upserting hstore: ~%~a~%" fl-hstore-arg)
                    (planesnwind.db:hstore-upsert :flights fl-hstore-arg fid)

                    (format t "adding path... ")
                    (planesnwind.db:insert-flight-track fid ready-pathlist :track-type 0)
                    (format t "k, ~d pts written.~%" (length ready-pathlist))
                    t))))))))

#|
flights.
  "UAL77-1511591178-airline-0130".
     aircraft.type
     aircraft.heavy
     blocked
     blockMessage
     cancelled
     codeShare.links.trackLog
     destination.TZ
     destination.icao
     displayIdent
     diverted
     flightId same as top key
     flightPlan.altitude int fl
     flightPlan.departure int timestamp
     flightPlan.fuelBurn.gallons
     flightPlan.fuelBurn.pounds
     flightPlan.directDistance
     flightPlan.plannedDistance
     flightPlan.route
     flightPlan.speed int
     flightStatus "arrived"
     gateArrivalTimes . {actual, estimated, scheduled} all int ts
     gateDepartureTimes same ^
     ga bool
     historical bool
     ident
     landingTimes  . {actual, estimated, scheduled} all int ts
     origin . TZ, icao
     redactedBlockedTail
     redactedCallsign
     redactedTail
     resultUnknown bool
     takeoffTimes  . {actual, estimated, scheduled} all int ts
     timestamp int ts
     track []
        alt int fl
        coord [] lon, lat
        gs int
        timestamp int
     waypoints [] of [], lon, lat

|#


#|
[
{
    "23567545745 ts": [



    ]

"1052985500": [
"AA173",
38.09198,
-75.434349,
1541535623733,
30000,
"B772",
429.3,
213,
"ADSB",
"N784AN",
"LHR",
"RDU",
"AA"
],


"1052736665": [
"EY131",
42.14534,
-75.22245,
1541535633748,
30000,
"B789",
340,
240,
"ADSB",
"A6-BLD",
"AUH",
"IAD",
"EY"
],


}
]
|#

(defparameter +all-fls-rb24-url+ "https://data.rb24.com/live?airline=&station=&aircraft=&airport=&fn=&far=&fms=&adsb=true&asdi=true&ocea=true&mlat=true&sate=true&esti=true&hfdl=true&adsbsate=true&diverted=false&delayed=false&getAirports=false&zoom=4&flightid=&designator=iata&timestamp=false&bounds=60.015,-62.923,9.438,-126.82")

(defun filter-flight-p (fl)
  (or
   (not (cl-ppcre:scan "^[A-Z]{2,3}[0-9]+" (first fl)))
   (if (and (fourth fl) (> (length (fourth fl)) 0)) (not (cl-ppcre:scan "^N[0-9]+" (fourth fl))))
   (= 0 (length (fifth fl)))
   (= 0 (length (sixth fl)))
   (not (cl-ppcre:scan "^K*[A-Z]{3}$" (fifth fl)))
   (not (cl-ppcre:scan "^K*[A-Z]{3}$" (sixth fl)))))

(defun handle-rb24-resp-obj (ro)
  (let ((fl-list (first ro))
        (cts (second ro))
        (us-apts (planesnwind.db:fetch-contig-us-airports))
        (fls))
    (format t "--- rb resp says: ~%~a~%" cts)

    (setf fls
          (remove-if-not
           #'(lambda (k)
               (and (find (fifth k) us-apts :test #'string=)
                    (find (sixth k) us-apts :test #'string=)))
           (mapcar #'(lambda (r)
                       (append
                        (subseq r 0 4)
                        (list
                         (if (= 4 (length (fifth r))) (fifth r) (concatenate 'string "K" (fifth r)))
                         (if (= 4 (length (sixth r))) (sixth r) (concatenate 'string "K" (sixth r))))))
                   (remove-if #'filter-flight-p
                              (mapcar #'(lambda (o)
                                          (list
                                           (nth 0 o)
                                           (nth 4 o)
                                           (nth 5 o)
                                           (nth 9 o)
                                           (nth 10 o)
                                           (nth 11 o)
                                           (nth 12 o)))
                                      (mapcar #'rest fl-list))))))
    (format t "--- after filtering, we have ~a flights that look good.~%" (length fls))
    fls))

(defun insert-new-known-flights (fls)
  (loop for fl in fls
     collecting
       (planesnwind.db:insert-known-flight (first fl) (fifth fl) (sixth fl))))

@export
(defun do-whole-find-new-known-flights-thing ()
  (format t "--- starting known flights count: ~a~%" (planesnwind.db:count-known-flights))

  (let ((r (length
            (insert-new-known-flights
             (handle-rb24-resp-obj
              (cl-json:decode-json-from-string
               (drakma:http-request +all-fls-rb24-url+)))))))
    (format t "--- got ~a flights we like; count after inserts is ~a.~%"
            r (planesnwind.db:count-known-flights))
    r))
