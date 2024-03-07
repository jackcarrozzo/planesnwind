;;

;; https://flightaware.com/live/flight/UAL779

(push '("application" . "json") drakma:*text-content-types*)

(defun parse-page (p)
  (let ((stxt
         (elt (remove-if-not
               #'(lambda (r) (cl-ppcre:scan "trackpollBootstrap" r))
               (lquery:$ p "script" (text)))
              0)))
    (subseq stxt 25 (- (length stxt) 1))))

(defun write-json-file (js &optional (fpath "/tmp/yar.json"))
  (with-open-file (out fpath
                       :direction :output
                       :if-exists :supersede)
    (princ js out)
    (format out "~%"))
  (format t "-- wrote ~a successfully.~%" fpath))

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
