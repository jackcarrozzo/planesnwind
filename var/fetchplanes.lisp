;;


;; curl -u jackc:1c9328518...ccd80eb51962c61b147 'http://flightxml.flightaware.com/json/FlightXML2/GetLastTrack?ident=UAL779'

;; https://flightaware.com/commercial/flightxml/explorer/

;; http://flightxml.flightaware.com/soap/FlightXML2/doc#op_GetLastTrack

;; http://flightxml.flightaware.com/soap/FlightXML2/doc#type_TrackStruct



;; todo: determine what flights begin and end in 24 hr period

;; todo: cronjob to suck down wind data so we have a whole contiguous day when needed

;; n num available in airlineflightinfostruct from Airline FLightInfo call
;; aircrafttype --> aircrafttypestruct - desc, manf, type
;; BlockIdentCheck

;; decodeflightroute
;; decode route

;; flightinfo - request time series of flight infos

;; get historical track
;; get last track

;; lat lons to distance - great circle

;; search could be helpful

;; https://datahub.io/core/airport-codes
;; https://opendata.socrata.com/dataset/Airport-Codes-mapped-to-Latitude-Longitude-in-the-/rxrh-4cxm





;;
;; for each KSFO etc
;;   walk this fucker to get arrivals in last 24hr:
;;     curl -u jackc:1c9328518...ccd80eb51962c61b147 'http://flightxml.flightaware.com/json/FlightXML2/Arrived?airport=KBOS&howMany=15&filter=airline'
;;
;; about each flight, ask:
;;
;; curl -u jackc:1c9328518...ccd80eb51962c61b147 'http://flightxml.flightaware.com/json/FlightXML2/GetLastTrack?ident=UAL779'

(ql:quickload 'drakma)

(push '("application" . "json") drakma:*text-content-types*)

(defparameter +test-airports+ '("KSFO" "KBOS" "KDEN" "KORD"))

(defparameter +fa-username+ "jackc")
(defparameter +fa-key+ "1c93285182b976941eac0ccd80eb51962c61b147")
(defparameter +fa-url-base+ "http://flightxml.flightaware.com/json/FlightXML2")
(defparameter +fa-resp-len+ 15)

(defun request-fa-arrivals (airport-code offset)
  (let ((req-url (format nil "~a/Arrived?airport=~a&howMany=15&filter=airline&offset=~a"
                         +fa-url-base+ airport-code offset))
        (req-ba `(,+fa-username+ ,+fa-key+)))
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
