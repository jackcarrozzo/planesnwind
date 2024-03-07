(in-package :cl-user)
(defpackage planesnwind.winds
  (:use :cl
        :datafly
        :sxql
        :cl-ppcre
        :drakma
        :planesnwind.util))
(in-package :planesnwind.winds)
(cl-syntax:use-syntax :annot)

@export
(defparameter +wind-obs-alts+
  '(3000 6000 9000 12000 18000 24000 30000 34000 39000))

;; calc this once cuz we use it a lot and walking the list
;; is expensive
(defparameter +highest-wind-obs-alt+ (first (last +wind-obs-alts+)))

@export
(defun bounding-wind-alts (alt)
  (let ((alt-i (round alt)))
    (cond
      ((< alt (first +wind-obs-alts+))
       `(-1 ,(first +wind-obs-alts+)))
      ((> alt +highest-wind-obs-alt+)
       `(,+highest-wind-obs-alt+ -1))
      (t
       (let ((list-r (find (round alt) +wind-obs-alts+)))
         (if list-r
             list-r
             (let ((lvl-n -1))
               (loop for lvl in +wind-obs-alts+
                  while (< lvl alt-i)
                  do
                    (incf lvl-n))
               (list
                (nth lvl-n +wind-obs-alts+)
                (nth (1+ lvl-n) +wind-obs-alts+)))))))))

;;(defparameter +windtemp-page-url+ "https://aviationweather.gov/windtemp/data?date=")
(defparameter +windtemp-page-url+ "https://aviationweather.gov/windtemp/data?level=low&fcst=06&region=all&layout=on&date=")

;; fcst:
;;  24 - 1800 - 0600
;;  06 0200 - 0900
;;  12 0900 1800
;;  24 1800-0600
(defun fetch-windtemp-text (url)
  ;; region=all
  (first
   (cl-ppcre:split
    (concatenate 'string (string #\Newline) "</pre>")
    (second
     (cl-ppcre:split "<pre>"
                     (drakma:http-request url
                                          :connection-timeout 10))))))
;; used for tests
(defparameter wxblock
  " (Extracted from FBUS31 KWNO 180158)
FD1US1
DATA BASED ON 180000Z
VALID 180600Z   FOR USE 0200-0900Z. TEMPS NEG ABV 24000

FT  3000    6000    9000   12000   18000   24000  30000  34000  39000
BRL 3513 3314+03 3219+02 3122+00 2930-12 2837-25 286839 279847 770455
DBQ 3312 3216+02 3223+00 3127-01 3031-14 2944-26 286141 288149 780056
DSM 3008 3222+04 3325+01 3224-01 2927-13 2833-25 286139 277948 279456
MCW 2809 3119+03 3223+01 3326-02 3025-14 2938-26 285541 287050 279256
JOT 3416 3216+01 3225+02 3131-01 3042-13 2950-26 296941 289348 781256
SPI 0116 3213+04 3219+03 3025+01 2932-11 2940-24 287738 771047 771856
EVV 0120 3511+05 3118+05 2932+04 2831-11 2950-21 278835 278746 780956
FWA 3318 3322-04 3237+02 3143-02 3161-13 3067-26 297742 780848 782956
IND 3615 3315+02 3227+03 3130+00 2938-12 2952-25 298438 782646 782957
GCK      1711+07 1811+04 1714+00 2020-12 2337-23 236737 236847 740253
GLD      1923    2022+03 2119+00 2023-13 2332-25 236539 237448 248554
ICT 1009 1010+07 9900+05 3006+02 2418-10 2639-22 246836 248046 750454
SLN 1607 1106+07 9900+03 3111+02 2413-11 2634-23 257037 257447 750554
LOU 0121 3412+05 3123+05 2932+03 2939-11 2954-22 289535 279746 781457
ECK 3222 3136-08 3139-07 3251-09 3287-17 8202-29 801944 803151 792554
MKG 3413 3026-05 3235-02 3248-04 3160-15 3168-28 309043 299951 790957
MQT 3113 3126-06 3238-06 3248-08 3268-16 3276-29 318146 318954 319258
SSM 3217 3128-12 3239-09 3250-12 3283-19 3298-31 820347 319954 319854
TVC 3216 3127-08 3238-06 3250-07 3276-16 3280-29 319945 801153 800656
AXN 2319 2818+04 3123+03 3128-02 3034-16 3037-28 294443 294852 285658
DLH 2620 3019+00 3230-01 3242-05 3148-17 3151-28 306144 306753 306759
INL 2432 2826+01 3033-01 3143-04 3151-17 3149-28 315745 316054 306259
MSP 2314 2916+02 3325+01 3332-03 3138-16 3043-28 295143 295751 286558
CGI 0121 3608+06 2916+07 2923+04 2833-08 2943-22 276935 277945 770054
COU 3614 0119+05 3516+04 3019+03 2826-11 2842-22 278636 269446 770856
MKC 9900 0117+05 0217+03 3210+01 2720-11 2836-23 267437 268747 760855
SGF 0317 0319+06 3110+06 2815+03 2621-09 2836-22 256135 267845 269854
STL 0116 3412+05 3217+04 3025+03 2827-12 2947-22 278936 269346 771056
DIK      2418+10 2610+06 2609+00 2808-17 2711-28 271044 261552 262659
GFK 2440 2825+08 3027+03 3031-02 3036-17 3037-29 303544 303952 294260
MOT      2820+11 2920+06 2921-01 3023-17 3023-29 302844 292753 282860
BFF      1932    1931+02 1925-01 2031-14 2028-27 224942 236650 238255
GRI      2511+07 9900+04 2506+00 2417-12 2631-25 265639 257248 258756
OMA 2219 2919+06 3417+02 3216-01 2722-13 2837-25 275639 267748 269056
ONL      2416+08 2505+04 2706-01 2516-15 2725-27 265141 256450 258756
ABR 2229 2822+08 2816+05 2916-01 3019-16 2924-28 282943 273351 274158
FSD 2024 2718+07 3016+04 3215-02 3022-15 2828-27 274442 265650 268057
PIR      2419+09 2411+05 2508-01 2713-16 2618-28 252843 253851 254757
RAP      2125+09 1919+05 1816+00 1812-15 2014-28 232943 233651 233757
GRB 3216 3120-04 3235-03 3244-04 3150-15 3160-28 308143 308552 299557
LSE 3011 3118+00 3328+00 3233-02 3140-14 3045-27 295742 286551 288157")

;; while we're here:
;; -- winds group 9900 means light and variable; 990012 is l+v at +12 C
(defun parse-windtemp-field (instr)
  (if instr
        (let ((first-pass-vals
               (ecase (length instr)
                 (4
                  (list
                   (* 10 (parse-integer (subseq instr 0 2)))
                   (parse-integer (subseq instr 2 4))
                   -1))
                 (7
                  (list
                   (* 10 (parse-integer (subseq instr 0 2)))
                   (parse-integer (subseq instr 2 4))
                   (let ((fmt-sign (subseq instr 4 5)))
                     (cond
                       ((string= "+" fmt-sign)
                        (parse-integer (subseq instr 5 7)))
                       ((string= "-" fmt-sign)
                        (* -1
                           (parse-integer (subseq instr 5 7))))
                       (t
                        (format nil "chunk makes no sense: '~a', len 6.'~%" instr)
                        -1)))))
                 (6
                  ;; being 6 chars long implies that a sign wasnt needed, implying the reading
                  ;;   is (a) negative and (b) above 24000 MSL
                  (list
                   (* 10 (parse-integer (subseq instr 0 2)))
                   (parse-integer (subseq instr 2 4))
                   (* -1 (parse-integer (subseq instr 4 6))))))))
          (let ((wdir (first first-pass-vals))
                (wspd (second first-pass-vals))
                (temp (third first-pass-vals)))
            ;; this could be more aestetically pleasing
            (when (and (<= 510 wdir) ;; per spec
                       (>= 860 wdir))
              (setf wdir (- wdir 500))
              (if (> 0 wdir) (incf wdir 360))
              (incf wspd 100))
            (list wdir wspd temp)))))

(defun parse-windtemp-block (block-text)
  ;; postmodern elegance
  (let ((state 0)
        (block-lines (cl-ppcre:split (string #\Newline) block-text))
        ;;(robj '())
        (metaobj '())
        (rlist '()))
    (loop for block-line in block-lines
       do
         (cond
           ((= 0 state)
            (multiple-value-bind (strs parts)
                (cl-ppcre:scan-to-strings "BASED ON (..)(....)(.)" block-line)
              (when strs
                (format t "- data based on day '~a' time '~a' tz '~a'~%" (aref parts 0) (aref parts 1) (aref parts 2))

                (setf metaobj
                      `((:data-from-day . ,(parse-integer (aref parts 0)))
                        (:data-from-time . ,(parse-integer (aref parts 1)))
                        (:data-from-tz . ,(aref parts 2))))))

            (multiple-value-bind (strs parts)
                (cl-ppcre:scan-to-strings "VALID (..)(....)(.) .* FOR USE (....)-(....)(.). TEMPS NEG ABV (....)" block-line)
              (when strs
                (format t "- valid day '~a', time '~a', tz '~a', for use '~a' to '~a', neg abv '~a'~%"
                        (aref parts 0) (aref parts 1) (aref parts 2)
                        (aref parts 3) (aref parts 4) (aref parts 5))

                (let* ((day (rest (assoc :data-from-day metaobj)))
                       (start-hr (/ (parse-integer (aref parts 3)) 100))
                       (end-hr (/ (parse-integer (aref parts 4)) 100))
                       (now-zulu-secs (planesnwind.db:get-zulu-time))
                       (now-parts (multiple-value-list
                                   (decode-universal-time (planesnwind.util:unix-to-universal-time
                                                           now-zulu-secs))))
                       (wind-zulu-day (planesnwind.util:universal-to-unix-time
                                       (encode-universal-time
                                        0 0 0 day (nth 4 now-parts) (nth 5 now-parts))))
                       (start-ts (+ wind-zulu-day (* start-hr 60 60)))
                       (end-ts (+ wind-zulu-day (* (if (< end-hr start-hr)
                                                       (+ 24 end-hr)
                                                       end-hr)
                                                   60 60))))


                  (format t "---- trying to make day ~a mon ~a yr ~a~%"
                          day (nth 4 now-parts) (nth 5 now-parts))
                  (format t "-- wind day ~a from ~a to ~a, zulu day start ~a ~a~%"
                          day start-hr end-hr wind-zulu-day (planesnwind.util:ts-to-human wind-zulu-day))
                  (format t "---- wind for use ~a ~a to ~a ~a zulu.~%"
                          start-ts (planesnwind.util:ts-to-human start-ts)
                          end-ts (planesnwind.util:ts-to-human end-ts))



                  (setf metaobj
                        (append metaobj
                                `((:valid-day . ,(parse-integer (aref parts 0)))
                                  (:valid-time . ,(parse-integer (aref parts 1)))
                                  (:valid-tz . ,(aref parts 2))
                                  (:foruse-start . ,(parse-integer (aref parts 3)))
                                  (:foruse-end . ,(parse-integer (aref parts 4)))
                                  (:for-use-tz . ,(aref parts 5))
                                  (:neg-above . ,(parse-integer (aref parts 6)))
                                  (:start-ts . ,start-ts)
                                  (:end-ts . ,end-ts)))))

                (incf state))))
           ((= 1 state)
            ;; rather than parse this baby out and use the given alt values, please forgive me for
            ;;   hoping and expecting that their values won't change, and if they do, at least it's detected
            (when (cl-ppcre:scan-to-strings "^FT  3000    6000    9000   12000   18000   24000  30000  34000  39000" block-line)
              (incf state)))
           (t
            (let ((line-parts (cl-ppcre:split " "
                                              (cl-ppcre:regex-replace-all " [ ]+" block-line " ")))
                  (obs-alts '(3000 6000 9000 12000 18000 24000 30000 34000 39000)))

              ;; todo: clean this poor guy up
              (case (length line-parts)
                 (10
                  (push
                   (append (list (first line-parts))
                           (loop for i from 1 to 9
                              collecting
                                (append (list (pop obs-alts))
                                        (parse-windtemp-field
                                         (nth i line-parts)))))
                   rlist))
                 (9
                  (pop obs-alts)
                  (push
                   (append (list (first line-parts))
                           (loop for i from 1 to 8
                              collecting
                                (append (list (pop obs-alts))
                                        (parse-windtemp-field
                                         (nth i line-parts)))))
                   rlist))
                 (8
                  (pop obs-alts)
                  (pop obs-alts)
                  (push
                   (append (list (first line-parts))
                           (loop for i from 1 to 7
                              collecting
                                (append (list (pop obs-alts))
                                        (parse-windtemp-field
                                         (nth i line-parts)))))
                   rlist))
                 (7
                  (pop obs-alts) ;;
                  (pop obs-alts) ;; i really am sorry about this
                  (pop obs-alts) ;;
                  (push
                   (append (list (first line-parts))
                           (loop for i from 1 to 6
                              collecting
                                (append (list (pop obs-alts))
                                        (parse-windtemp-field
                                         (nth i line-parts)))))
                   rlist))
                 (otherwise
                  (format t "-- ignoring too-short wxblock line: ~a~%" block-line)))))))

    `((:obslist . ,rlist)
      (:meta . ,metaobj))))

(defun add-posns-to-obslist (windtemp-alist)

  #|(let ((obslist (rest (assoc :obslist windtemp-alist)))
        (metaobj (rest (assoc :meta windtemp-alist))))

    ;; i wish there was an in-place way to do this instead
    ;; of making a whole new list
    `((:meta . ,metaobj)
      (:obslist . ,(remove-if-not #'first ;; todo: check this
                    (loop for this-obs in obslist
                       collecting
                         (cons
                          (planesnwind.db:fetch-airport-posn
                           (concatenate 'string
                                        "K" (first this-obs)))
                          this-obs))))))
  |#
  windtemp-alist)

(defparameter +wind-base+ "https://aviationweather.gov/windtemp/data?level=low&region=all&layout=on")

@export
(defun import-windset (qstr)
  ;; "https://aviationweather.gov/windtemp/data?level=low&fcst=24&region=bos&layout=on&date="
  (let* ((url (concatenate 'string +wind-base+ qstr))
         (obs (add-posns-to-obslist
               (parse-windtemp-block
                (fetch-windtemp-text url))))
         (metaobj (rest (assoc :meta obs)))
         (obslist (rest (assoc :obslist obs)))
         (wind-set-id))

    (cond
      ((not (and metaobj obslist))
       (format t "!!!! windset ~a failed to parse, prolly sad.~%" qstr)
       (error "windset ~a failed to parse, prolly sad.~%" qstr)
       nil)
      ((setf wind-set-id (planesnwind.db:create-wind-set metaobj))
       (format t "--- wind set created, page parsed ok; wsid ~d - inserting... ~%" (round wind-set-id))
       (planesnwind.db:insert-obslist wind-set-id obslist)
       (format t "--- looks like ~d obs went in ok.~%" (length obslist))
       (values
        wind-set-id
        (length obslist)))
      (t
       (format t "!!! had trouble creating wind set: ~%~a~%" metaobj)
       (error "had trouble creating windset")
       nil))))

@export
(defun import-windset-for-ts (ts)
  (let* ((ts-parts
          (multiple-value-list
           (decode-universal-time (planesnwind.util:unix-to-universal-time ts))))
         (ts-year (nth 5 ts-parts))
         (ts-mon (nth 4 ts-parts))
         (ts-day (nth 3 ts-parts))
         (ts-hr (nth 2 ts-parts))
         (fcast
          (cond
            ((and (>= ts-hr 0) (< ts-hr 9)) 6)
            ((and (>= ts-hr 9) (< ts-hr 18)) 12)
            ((and (>= ts-hr 18) (< ts-hr 24)) 24)
            (t
             (error "windset for hr ~a is shot." ts-hr)))))

    ;; todo: back up req day if hour < 02

    (format t "reqd zulu time is ~a~%" (planesnwind.util:ts-to-human ts))

    (format nil "&fcst=~2,'0d&date=~d~2,'0d~2,'0d0000"
            fcast ts-year ts-mon ts-day)))

#|

request day 30 fc 06 : 02 - 09 on 30th
                  12 : 09 - 18 on 30th
                  24 : 18 on 30th to 06 on 31st

;; fcst:
;;  24 - 1800 - 0600
;;  06 0200 - 0900
;;  12 0900 1800
;;  24 1800-0600

   (now-zulu-secs (planesnwind.db:get-zulu-time))
                       (now-parts (multiple-value-list
                                   (decode-universal-time (planesnwind.util:unix-to-universal-time
                                                          now-zulu-secs))))
                       (wind-zulu-day (planesnwind.util:universal-to-unix-time
                                       (encode-universal-time
                                        0 0 0 day (nth 4 now-parts) (nth 5 now-parts))))
                       (start-ts (+ wind-zulu-day (* start-hr 60 60)))
                       (end-ts (+ wind-zulu-day (* (if (< end-hr start-hr)
                                                       (+ 24 end-hr)
                                                       end-hr)
                                                   60 60))))


                  (format t "---- trying to make day ~a mon ~a yr ~a~%"
                          day (nth 4 now-parts) (nth 5 now-parts))
                  (format t "-- wind day ~a from ~a to ~a, zulu day start ~a ~a~%"
                          day start-hr end-hr wind-zulu-day (planesnwind.util:ts-to-human wind-zulu-day))
|#

#|
for x in 4j3 2xg t07 t06 t01 ssm mbw imb hat h61 h52 h51 emi eck czi; do grep -i $x f.txt; done|sed 's/# //i'|sed 's//d/ig' |sed "s/'/m/ig"|sed 's/,//ig'

4J3 S Apalachicola South 28d 30m -85d00m 0
2XG Gauge 30d 20m -78d30m 0
T07 S Mobile South 28d 30m -88d00m 0
T06 Ship Shoal 28d 30m -91d00m 0
T01 West Cameron 28d 30m -93d30m 0
SSM Sault Ste. Marie 46d 24m -84d18m 690
MBW Medicine Bow 41d 50m -106d00m 7000
IMB Kimberly 44d 38m -119d42m 5220
HAT Cape Hatteras 35d 16m -75d33m 11
H61 W Fort Myers West 26d 30m -84d00m 0
H52 Mid Gulf 26d 00m -89d30m 0
H51 Bay City 26d 30m -95d00m 0
EMI Westminster 39d 29m -76d58m 820
ECK Peck 43d 15m -82d43m 810
CZI CrazyWoman 43d 59m -106d26m 4798

http://www.nws.noaa.gov/directives/sym/pd01008012curr.pdf

WINDS> (mapcar #'(lambda (l) (format t "insert into airports (ident,name,posn,elev_ft,contig_us) values ('K~a','WX ~a',point(~a,~a),~a,1);~%" (first l) (second l) (+ (fifth l) (/ (sixth l) 60.0)) (+ (third l) (/ (fourth l) 60.0)) (seventh l))) (mapcar #'(lambda (o) (append (list (elt o 0) (elt o 1)) (loop for i from 2 to 6 collecting (or (parse-integer (elt o i) :junk-allowed t) (parse-integer (subseq (elt o i) 1)))))) (mapcar #'(lambda (l) (nth-value 1 (cl-ppcre:scan-to-strings "^(...) (.+) ([0-9-]+)d ([0-9]+)m ([0-9-]+)d(.+)m (.+)$" l))) (cl-ppcre:split #\newline +stupid-obs-locns+))))

insert into airports (ident,name,posn,elev_ft,contig_us) values ('K4J3','WX S Apalachicola South',point(-85.0,28.5),0,1);
insert into airports (ident,name,posn,elev_ft,contig_us) values ('K2XG','WX Gauge',point(-77.5,30.333334),0,1);
insert into airports (ident,name,posn,elev_ft,contig_us) values ('KT07','WX S Mobile South',point(-88.0,28.5),0,1);
insert into airports (ident,name,posn,elev_ft,contig_us) values ('KT06','WX Ship Shoal',point(-91.0,28.5),0,1);
insert into airports (ident,name,posn,elev_ft,contig_us) values ('KT01','WX West Cameron',point(-92.5,28.5),0,1);
insert into airports (ident,name,posn,elev_ft,contig_us) values ('KSSM','WX Sault Ste. Marie',point(-83.7,46.4),690,1);
insert into airports (ident,name,posn,elev_ft,contig_us) values ('KMBW','WX Medicine Bow',point(-106.0,41.833332),7000,1);
insert into airports (ident,name,posn,elev_ft,contig_us) values ('KIMB','WX Kimberly',point(-118.3,44.633335),5220,1);
insert into airports (ident,name,posn,elev_ft,contig_us) values ('KHAT','WX Cape Hatteras',point(-74.45,35.266666),11,1);
insert into airports (ident,name,posn,elev_ft,contig_us) values ('KH61','WX W Fort Myers West',point(-84.0,26.5),0,1);
insert into airports (ident,name,posn,elev_ft,contig_us) values ('KH52','WX Mid Gulf',point(-88.5,26.0),0,1);
insert into airports (ident,name,posn,elev_ft,contig_us) values ('KH51','WX Bay City',point(-95.0,26.5),0,1);
insert into airports (ident,name,posn,elev_ft,contig_us) values ('KEMI','WX Westminster',point(-75.03333,39.483334),820,1);
insert into airports (ident,name,posn,elev_ft,contig_us) values ('KECK','WX Peck',point(-81.28333,43.25),810,1);
insert into airports (ident,name,posn,elev_ft,contig_us) values ('KCZI','WX CrazyWoman',point(-105.566666,43.983334),4798,1);

curl http://www.nws.noaa.gov/directives/sym/pd01008012curr.pdf |pdftotext -layout - - |awk '{$1=$1};1' |awk '/^[A-Z0-9]+ /'|sed -n 's/^\([A-Z0-9]\{3\}\)[ #]* \(.*\) \([0-9-]\{2,4\}\).*\([0-9]\{2\}\).* \([0-9-]\{2,4\}\).*\([0-9]\{2\}\).* \([0-9,]\{1,7\}\)$/\1::\\"\2\\"::\3::\4::\5::\6:: \7/p'|sed -n 's/^\(.*::.*::.*::.*::.*::.*::\) \([0-9]\{0,2\}\)[,]\{0,1\}\([0-9]\{1,3\}\)$/\1\2\3/p'|sed -n 's/::/,/gp'|sed -n 's/^\(.*\),\([0-9-]*\),\([0-9-]*\),\([0-9-]*\),\([0-9-]*\),\([0-9]*\)$/echo \1,\2,\3,\4,\5,`echo \"scale=6\;if (\2 \< 0) \2 - (\3 \/ 60) else \2 + (\3 \/ 60)\" |bc -l`,`echo \"scale=6\;if (\4 \< 0) \4 - (\5 \/ 60) else \4 + (\5 \/ 60)\" |bc -l`,\6/p'|sh|wc -l

(defvar ggggg (mapcar #'(lambda (r) (destructuring-bind (ident name lat lon alt) (coerce (nth-value 1 (cl-ppcre:scan-to-strings "^(.*),\"(.*)\",.*,.*,.*,.*,(.*),(.*),(.*)$" r)) 'list) (format nil "insert into wx_sites(ident,name,posn,alt_ft) values ('~a','~a',point(~a,~a),~a);" ident (planesnwind.db::sql-sanitize name) lon lat alt))) (cl-ppcre:split #\newline r)))
|#

(defparameter +stupid-obs-locns+
  "4J3 S Apalachicola South 28d 30m -85d00m 0
2XG Gauge 30d 20m -78d30m 0
T07 S Mobile South 28d 30m -88d00m 0
T06 Ship Shoal 28d 30m -91d00m 0
T01 West Cameron 28d 30m -93d30m 0
SSM Sault Ste. Marie 46d 24m -84d18m 690
MBW Medicine Bow 41d 50m -106d00m 7000
IMB Kimberly 44d 38m -119d42m 5220
HAT Cape Hatteras 35d 16m -75d33m 11
H61 W Fort Myers West 26d 30m -84d00m 0
H52 Mid Gulf 26d 00m -89d30m 0
H51 Bay City 26d 30m -95d00m 0
EMI Westminster 39d 29m -76d58m 820
ECK Peck 43d 15m -82d43m 810
CZI CrazyWoman 43d 59m -106d26m 4798")

;;; ------------ wind model stuff ------------------

(defvar *wind-model-ht* (make-hash-table :test 'equal))

@export
(defun clear-wind-model-ht ()
  (setf *wind-model-ht*
        (make-hash-table :test 'equal)))

#|
49.234266, -124.911378
47.281549, -67.937092
25.008041, -80.613455
39.926447, -124.663929

so x=lon= -125 to -70 and
   y=lat= 25 to 50
|#

@export
(defun make-ht-key (wset-id component xlon ylat alt)
  (format nil "~d-~a-~d-~d-~d"
          (round wset-id) (string component) (round xlon)
          (round ylat) (round alt)))


@export
(defun create-wind-model-at-alt (wset-id alt &key (d-xlon 1) (d-ylat 1))
  (if (not (find alt +wind-obs-alts+))
      (error "cant create wind model layer in nonstandard alt")
      (let ((n 0)
            (min-xlon (getf planesnwind.util:+model-spatial-range+ :min-xlon))
            (max-xlon (getf planesnwind.util:+model-spatial-range+ :max-xlon))
            (min-ylat (getf planesnwind.util:+model-spatial-range+ :min-ylat))
            (max-ylat (getf planesnwind.util:+model-spatial-range+ :max-ylat))
            (start-ts (/ (float (get-internal-real-time)) internal-time-units-per-second)))
        (loop for this-xlon from min-xlon to max-xlon by d-xlon
           do
             (loop for this-ylat from min-ylat to max-ylat by d-ylat
                do
                  (let* (

                         ;;(pt-plist (list :lon this-xlon :lat this-ylat))
                         ;;(near-winds-list (planesnwind.db:get-nearest-winds-at-alt
                         ;;                  wset-id pt-plist alt))

                         (near-winds-list (planesnwind.db:get-closest-windobs-by-wsid-and-alt
                                           wset-id alt this-xlon this-ylat)))
                    (setf (gethash (make-ht-key wset-id :wspd this-xlon this-ylat alt)
                                   *wind-model-ht*)
                          (planesnwind.util:idw-interp
                           near-winds-list :dist :wspd))
                    (setf (gethash (make-ht-key wset-id :wdir this-xlon this-ylat alt)
                                   *wind-model-ht*)
                          (planesnwind.util:idw-interp
                           near-winds-list :dist :wdir :angular-p t))
                    (incf n))))
        (values
         (- (/ (float (get-internal-real-time)) internal-time-units-per-second) start-ts)
         n))))

@export
(defun create-wind-model (wset-id &key (min-level 9000))
  ;; todo: values incr n
  (let* ((n 0)
        (r-sec
         (reduce #'+
                 (mapcar #'(lambda (a)
                             (multiple-value-bind (secs num) (create-wind-model-at-alt wset-id a)
                               (incf n num)
                               secs))
                         (remove-if #'(lambda (v) (< v min-level))
                                    +wind-obs-alts+)))))
    (format t "-- built wind model in ~a sec making ~a interpolations.~%" r-sec n)))

@export
(defvar *this-wsid* nil)

@export
(defun set-this-wsid (wsid)
  (setf *this-wsid* wsid))

@export
(defun query-this-wind-model (comp xlon ylat alt)
  (when (not *this-wsid*)
    (format t "!!!!! attempt to query wind model without setting wsid.~%")
    (error "windmodel queried without valid wsid set."))

  (query-wind-model-full *this-wsid* comp xlon ylat alt))

@export
(defun query-wind-model-full (wset-id component xlon ylat alt)
  (let ((bwa (bounding-wind-alts alt)))
    (cond
      ((integerp bwa)
       (gethash (make-ht-key wset-id component xlon ylat bwa)
                *wind-model-ht*))
      ((listp bwa)
       (destructuring-bind (lower-alt upper-alt) bwa
         (cond
           ((= -1 lower-alt)
            (gethash (make-ht-key wset-id component xlon ylat upper-alt)
                     *wind-model-ht*))
           ((= -1 upper-alt)
            (gethash (make-ht-key wset-id component xlon ylat lower-alt)
                     *wind-model-ht*))
           (t
            (let ((upper-val (gethash (make-ht-key wset-id component xlon ylat upper-alt)
                                      *wind-model-ht*))
                  (lower-val (gethash (make-ht-key wset-id component xlon ylat lower-alt)
                                      *wind-model-ht*)))
              (cond
                ((and upper-val lower-val)
                 (planesnwind.util:idw-interp
                  (list
                   (list :d (- alt lower-alt) :v lower-val)
                   (list :d (- upper-alt alt) :v upper-val))
                  :d :v :angular-p (eql :wdir component)))
                (upper-val
                 upper-val)
                (t
                 lower-val))))))))))

@export
(defun find-wset-id-for-ts (ts &optional (retry-times 2))
  (let* ((r (planesnwind.db:select-windset-for-ts ts))
        (wset-id (getf r :id))
        (err (getf r :err)))

    (unless r
     (format t "-- no windset exists to cover ~a (~a), gonna fetch it.~%"
             ts (planesnwind.util:ts-to-human ts))
     (let ((start-sec (/ (float (get-internal-real-time)) internal-time-units-per-second))
           (fake-id (planesnwind.db:create-wind-set `((:at-ts . ,ts)) :fake-p t)))
       (format t "-------- fake ws id is ~a~%" fake-id)

       (multiple-value-bind (wid numobs)
           (import-windset (import-windset-for-ts ts))
         (format t "--- windset id ~a has ~a components and took ~3$s to fetch and insert.~%"
                 wid numobs
                 (- (/ (float (get-internal-real-time)) internal-time-units-per-second) start-sec))
         (planesnwind.worker:worker-log "windset-create-success"
                                        (format nil "wsid ~d, ~d obs, requested for ts ~a"
                                                wid
                                                numobs
                                                (planesnwind.util:ts-to-human ts))
                                        (- (/ (float (get-internal-real-time)) internal-time-units-per-second) start-sec))
         (setf wset-id wid))
       (planesnwind.db:remove-windset fake-id)))

    (cond
      ((and err
            (not (= 0 err)))

       (if (= -1 err)
           (cond
             ((> retry-times 0)
              (format t "--- wset id ~a has err -1, waiting 60 seconds and trying again (rt times ~d).~%"
                      wset-id retry-times)
              (planesnwind.worker:worker-log "windset-find-wait"
                                             (format nil "wset id ~a has error val ~a, retries ~d, waiting for it."
                                                     wset-id err retry-times)
                                             (float err))
              (sleep 60)
              (find-wset-id-for-ts ts (- retry-times 1)))
             (t
              (format t "----- wset id ~a has error val ~a and retries ~d, punting.~%" wset-id err retry-times)
              (planesnwind.worker:worker-log "windset-find-err"
                                             (format nil "wset id ~a has error val ~a, retries ~d, punting."
                                                     wset-id err retry-times)
                                             (float err))
              (error "wset id ~a has error val ~a for a while now, punting.~%" wset-id err)))))
      ((not wset-id)
       (format t "------ nil wset id :/ ~%")
       (planesnwind.worker:worker-log "windset-find-nil"
                                      (format nil "wset id nil for ts ~a" (planesnwind.util:ts-to-human ts))
                                      (float err))
       (error "nil wsid in find-wset-for-ts"))
      (t
       (format t "--- using wset ~a for ts ~a, ~a~%" wset-id ts (planesnwind.util:ts-to-human ts))
       wset-id))))

@export
(defun ensure-or-build-wind-model (wsid)
  (if (and
       wsid
       (query-wind-model-full wsid :wdir -100.0 35 34000)
       (query-wind-model-full wsid :wspd -100.0 35 34000))
      (progn
        (format t "-   (btw, wind model for wsid ~a already built, reusing it)~%" wsid)
        (planesnwind.worker:worker-log "windset-model-already-built"
                                       ""
                                       (float wsid))
        (values wsid :already-there))
      (progn
        (format t "-- building wind model for wsid ~a...~%" wsid)
        (planesnwind.worker:worker-log "windset-model-needs-building"
                                       ""
                                       (float wsid))
        (create-wind-model wsid)
        (values wsid :built-it))))

@export
(defun dump-whole-wind-model ()
  (alexandria:hash-table-alist *wind-model-ht*))

;; todo: wind model error evaluation against obs data
