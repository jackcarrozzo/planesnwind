(ql:quickload '(drakma cl-ppcre cl-json))

(push '("application" . "json") drakma:*text-content-types*)

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
  (multiple-value-bind (resp-data resp-code)
      (drakma:http-request +airports-csv-url+)
    (ecase resp-code
      (200
       (remove nil
               (loop for csv-line in (cl-ppcre:split (string #\Newline) resp-data)
                  collecting
                    (let ((airport-details-list (remove-quotes (cl-ppcre:split "," csv-line))))
                      (cond
                        ((or (not (string= "US" (nth 8 airport-details-list)))
                             (not (string= "yes" (nth 11 airport-details-list))))
                         nil)
                        (t
                         (let ((state-code (subseq (nth 9 airport-details-list) 3)))
                           `((ident . ,(nth 1 airport-details-list))
                             (name . ,(nth 3 airport-details-list))
                             (lat . ,(nth 4 airport-details-list))
                             (lon . ,(nth 5 airport-details-list))
                             (elev-ft . ,(nth 6 airport-details-list))
                             (state . ,state-code)
                             (contig-48 . ,(if (find state-code +contig-us-states+ :test #'string=)
                                               1 0)))))))))))))
