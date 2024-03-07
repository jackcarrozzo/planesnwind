(in-package :cl-user)
(defpackage planesnwind.util
  (:use :cl
        :datafly
        :sxql
        :alexandria
        :drakma
        :cl-ppcre))
(in-package :planesnwind.util)
(cl-syntax:use-syntax :annot)

@export
(defparameter +pnw-ver+ 0.1)

(push '("application" . "json") drakma:*text-content-types*)

@export
(defparameter +model-spatial-range+ '(:min-xlon -125
                                      :max-xlon -70
                                      :min-ylat 25
                                      :max-ylat 50))

@export
(defparameter +db-tz+ "America/New_York")


@export
(defun euclidian-dist (xlon1 ylat1 xlon2 ylat2)
  (sqrt
   (+ (expt (- xlon2 xlon1) 2)
      (expt (- ylat2 ylat1) 2))))

;;; ----- initial data stuff

#|
"id","ident","type","name","latitude_deg","longitude_deg","elevation_ft","continent","iso_country","iso_region","municipality","scheduled_service","gps_code","iata_code","local_code","home_link","wikipedia_link","keywords"
6523,"00A","heliport","Total Rf Heliport",40.07080078125,-74.93360137939453,11,"NA","US","US-PA","Bensalem","no","00A",,"00A",,,
323361,"00AA","small_airport","Aero B Ranch Airport",38.704022,-101.473911,3435,"NA","US","US-KS","Leoti","no","00AA",,"00AA",,,
6524,"00AK","small_airport","Lowell Field",59.94919968,-151.695999146,450,"NA","US","US-AK","Anchor Point","no","00AK",,"00AK",,,
6525,"00AL","small_airport","Epps Airpark",34.86479949951172,-86.77030181884766,820,"NA","US","US-AL","Harvest","no","00AL",,"00AL",,,
|#

(defparameter +contig-us-states+
  (mapcar #'string
          '(AL AZ AR CA CO CT DE FL GA ID IL IN IA KS KY LA ME MT NE NV NH NJ
            NM NY NC ND OH OK OR MD MA MI MN MS MO PA RI SC SD TN TX UT VT VA WA WV WI WY)))

(defparameter +airports-csv-url+ "http://ourairports.com/data/airports.csv")
;;(setf +airports-csv-url+ "http://localhost/small.csv")

(defun remove-quotes (field-list)
  (mapcar
   #'(lambda (f)
       (if (stringp f)
           (remove #\" f)
           f))
   field-list))

(defun fetch-us-airports ()
  ;; (get-internal-real-time) internal-time-units-per-second

  (multiple-value-bind (resp-data resp-code)
      (drakma:http-request +airports-csv-url+)
    (ecase resp-code
      (200
       (remove nil
               (loop for csv-line in (cl-ppcre:split (string #\Newline) resp-data)
                  collecting
                    (let ((airport-details-list (remove-quotes (cl-ppcre:split "," csv-line))))
                      (cond
                        ((or
                          ;;(> (length (nth 1 airport-details-list)) 4)
                          ;;(not (cl-ppcre:scan "^[A-Z]{3}$" (nth 1 airport-details-list)))
                          (not (string= "US" (string-upcase (nth 8 airport-details-list))))
                          )
                         nil)
                        (t
                         (let* ((state-str (nth 9 airport-details-list))
                                (state-code (if (and (> (length state-str) 3)
                                                     (string= "US-" (subseq state-str 0 3)))
                                                (subseq state-str 3 5)
                                                "ZZ")))
                           `((:ident . ,(nth 1 airport-details-list))
                             (:name . ,(nth 3 airport-details-list))
                             (:lat . ,(nth 4 airport-details-list))
                             (:lon . ,(nth 5 airport-details-list))
                             (:elev-ft . ,(nth 6 airport-details-list))
                             (:state . ,state-code)
                             (:country . ,(nth 8 airport-details-list))
                             (:contig-48 . ,(if (find state-code +contig-us-states+ :test #'string=)
                                                1 0)))))))))))))

;;; ------ time and date stuff ------

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))
@export
(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

@export
(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

@export
(defun get-unix-time ()
  (universal-to-unix-time (get-universal-time)))

#|
(defconstant *day-names*
  '("Monday" "Tuesday" "Wednesday"
    "Thursday" "Friday" "Saturday"
    "Sunday"))
|#

@export
(defun ts-to-human (ts-secs)
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time
       (unix-to-universal-time ts-secs))

    #|
    (format nil "~2,'0d:~2,'0d:~2,'0d of ~a, ~d/~2,'0d/~d (GMT~@d), ~a."
            hour
            minute
            second
            (nth day-of-week *day-names*)
            month
            date
            year
            (- tz)
            (if dst-p
                "DST in effect"
                "DST not in effect"))
    |#

    (declare (ignore day-of-week tz dst-p))
    (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour minute second)))

;;; ------ sql str adjustment help -----

@export
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with "
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
                         :start2 old-pos
                         :test test)
       do (write-string string out
                        :start old-pos
                        :end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

;; change ' to ''
;; (only safe for postgres)
@export
(defun sql-sanitize (str)
  (if (numberp str)
      str
      (replace-all str "'" "''")))

;;; ---------- file io -------
@export
(defun save-obj (filename obj)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print obj out))))

@export
(defun load-obj (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (read in))))

;;; -------------- math n stuff --------

@export
(defun deg2rad (d)
  (/ (* d pi)
     180.0))

@export
(defun rad2deg (r)
  (/ (* r 180.0)
     pi))

@export
(defun deg2dir (d)
  (let ((r (- 90.0 d)))
    (cond
      ((> r 360.0)
       (- r 360.0))
      ((< r 0.0)
       (+ r 360.0))
      (t
       r))))

@export
(defun dir2deg (d)
  (deg2dir d))

@export
(defun idw-interp (pts dist-key val-key &key angular-p (power 2.0))
  (case (length pts)
    (0
     (format t "!!! cant interpolate across 0 pts.~%")
     nil)
    (1
     (getf (first pts)
           val-key))
    (otherwise
     ;; todo: 0.0 distances cause div zero conditions;
     ;;   if any of the pts that we get have a distance
     ;;   of 0.0, dont bother calculating: that point's
     ;;   value must be the answer
     (if angular-p
         (let ((xtsum 0.0)
               (ytsum 0.0)
               (bsum 0.0))
           (loop for pt in pts
              do
                (let ((v-rads (deg2rad (getf pt val-key)))
                      (dist-to-pow (expt (getf pt dist-key) power)))
                  (incf xtsum
                        (/ (cos v-rads) dist-to-pow))
                  (incf ytsum
                        (/ (sin v-rads) dist-to-pow))
                  (incf bsum
                        (/ 1.0 dist-to-pow))))
           (let ((r (rad2deg
                     (atan (/ ytsum bsum)
                           (/ xtsum bsum)))))
             (if (< r 0.0)
                 (+ 360.0 r)
                 r)))
         (let ((tsum 0.0)
               (bsum 0.0))
           (loop for pt in pts
              do
                (let ((dist-to-pow (expt (getf pt dist-key) power)))
                  (incf tsum
                        (/ (getf pt val-key)
                           dist-to-pow))
                  (incf bsum
                        (/ 1.0 dist-to-pow))))
           (/ tsum bsum))))))
