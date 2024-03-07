(ql:quickload '(drakma cl-ppcre))

(defparameter +windtemp-page-url+ "https://aviationweather.gov/windtemp/data?date=")
;;(defparameter +windtemp-page-url+ "https://aviationweather.gov/windtemp/data?level=low&fcst=06&region=all&layout=on&date=")

(defun fetch-windtemp-text ()
  (first
   (cl-ppcre:split
    (concatenate 'string (string #\Newline) "</pre>")
    (second
     (cl-ppcre:split "<pre>"
                     (drakma:http-request +windtemp-page-url+))))))

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
                        (error (format nil "chunk makes no sense: '~a', len 6.'~%" instr)))))))
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
            (list wdir wspd temp)))
        -1))

(defun parse-windtemp-block (block-text)
  ;; postmodern elegance
  (let ((state 0)
        (block-lines (cl-ppcre:split (string #\Newline) block-text))
        (robj '())
        (rlist '()))
    (loop for block-line in block-lines
       do
         (cond
           ((= 0 state)
            (multiple-value-bind (strs parts)
                (cl-ppcre:scan-to-strings "BASED ON (..)(....)(.)" block-line)
              (when strs
                (format t "- data based on day '~a' time '~a' tz '~a'~%" (aref parts 0) (aref parts 1) (aref parts 2))

                (setf robj
                      `((data-from-day . ,(parse-integer (aref parts 0)))
                        (data-from-time . ,(parse-integer (aref parts 1)))
                        (data-from-tz . ,(aref parts 2))))))

            (multiple-value-bind (strs parts)
                (cl-ppcre:scan-to-strings "VALID (..)(....)(.) .* FOR USE (....)-(....)(.). TEMPS NEG ABV (....)" block-line)
              (when strs
                (format t "- valid day '~a', time '~a', tz '~a', for use '~a' to '~a', neg abv '~a'~%"
                        (aref parts 0) (aref parts 1) (aref parts 2)
                        (aref parts 3) (aref parts 4) (aref parts 5))

                (setf robj
                      (append robj
                              `((valid-day . ,(parse-integer (aref parts 0)))
                                (valid-time . ,(parse-integer (aref parts 1)))
                                (valid-tz . ,(aref parts 2))
                                (foruse-start . ,(parse-integer (aref parts 3)))
                                (foruse-end . ,(parse-integer (aref parts 4)))
                                (for-use-tz . ,(aref parts 5))
                                (neg-above . ,(parse-integer (aref parts 6))))))

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
              (push
               (ecase (length line-parts)
                 (10
                  (append (list (first line-parts))
                          (loop for i from 1 to 9
                             collecting
                               (append (list (pop obs-alts))
                                       (parse-windtemp-field
                                        (nth i line-parts))))))
                 (9
                  (pop obs-alts)
                  (append (list (first line-parts))
                          (list -1)
                          (loop for i from 1 to 8
                             collecting
                               (append (list (pop obs-alts))
                                       (parse-windtemp-field
                                        (nth i line-parts)))))))
               rlist)))))

    (push `(obslist . ,rlist) robj)
    robj))
