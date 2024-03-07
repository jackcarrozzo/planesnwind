(in-package :cl-user)
(defpackage planesnwind.paths
  (:use :cl
        :datafly
        :sxql
        :alexandria
        :planesnwind.util))
(in-package :planesnwind.paths)
(cl-syntax:use-syntax :annot)

(import 'planesnwind.util:rad2deg)
(import 'planesnwind.util:deg2rad)

;; ---------- great circle math -----
;; handy eqns: https://movable-type.co.uk/scripts/latlong.html
;;

@export
(defparameter +earth-r-meters+ 6371e3)

#|
var φ1 = lat1.toRadians();
var φ2 = lat2.toRadians();
var Δφ = (lat2-lat1).toRadians();
var Δλ = (lon2-lon1).toRadians();
|#

(defun haversine-etc (xlon0 ylat0 xlon1 ylat1)
  ;; takes coords in degs
  (let* ((xlon0-rads (deg2rad xlon0))
         (ylat0-rads (deg2rad ylat0))
         (xlon1-rads (deg2rad xlon1))
         (ylat1-rads (deg2rad ylat1))
         (dxlon-rads (deg2rad (- xlon1 xlon0)))
         (dylat-rads (deg2rad (- ylat1 ylat0)))
         (hs-a (+ (expt (sin (/ dylat-rads 2.0)) 2)
                  (* (cos ylat0-rads) (cos ylat1-rads)
                     (expt (sin (/ dxlon-rads 2.0)) 2))))
         (hs-c (* 2.0 (atan (sqrt hs-a) (sqrt (- 1.0 hs-a)))))
         (hs-dist-m (* hs-c +earth-r-meters+))
         (mp-bx (* (cos ylat1-rads) (cos dxlon-rads)))
         (mp-by (* (cos ylat1-rads) (sin dxlon-rads)))
         (mp-ylat-rads (atan (+ (sin ylat0-rads) (sin ylat1-rads))
                             (sqrt (+ (expt (+ (cos ylat0-rads) mp-bx) 2)
                                      (expt mp-by 2)))))
         (mp-xlon-rads (+ xlon0-rads (atan mp-by
                                           (+ (cos ylat0-rads) mp-bx)))))
    (declare (ignore xlon1-rads))
    (values
     hs-dist-m
     (list :ylat (rad2deg mp-ylat-rads)
           :xlon (rad2deg mp-xlon-rads)))))

(defun interp-gc-pt (xlon1 ylat1 xlon2 ylat2 p)
  (let* ((d (haversine-etc xlon1 ylat1 xlon2 ylat2))
         (sigma (/ d +earth-r-meters+))
         (a (/ (sin (* sigma (- 1.0 p))) (sin sigma)))
         (b (/ (sin (* p sigma)) (sin sigma)))
         (x (+ (* a (cos (deg2rad ylat1)) (cos (deg2rad xlon1)))
               (* b (cos (deg2rad ylat2)) (cos (deg2rad xlon2)))))
         (y (+ (* a (cos (deg2rad ylat1)) (sin (deg2rad xlon1)))
               (* b (cos (deg2rad ylat2)) (sin (deg2rad xlon2)))))
         (z (+ (* a (sin (deg2rad ylat1)))
               (* b (sin (deg2rad ylat2))))))
    (list
     :ylat (rad2deg (atan z (sqrt (+ (expt x 2) (expt y 2)))))
     :xlon (rad2deg (atan y x)))))

(defun bezier (xlon0 ylat0 xlon1 ylat1 xlon2 ylat2 at-p)
  (alexandria:plist-alist
   (list
    :lon (+
          (* (expt (- 1.0 at-p) 2) xlon0)
          (* 2.0 (- 1.0 at-p) at-p xlon1)
          (* (expt at-p 2) xlon2))
    :lat (+
          (* (expt (- 1.0 at-p) 2) ylat0)
          (* 2.0 (- 1.0 at-p) at-p ylat1)
          (* (expt at-p 2) ylat2)))))

(defun cubic-bezier (xlon0 ylat0 xlon1 ylat1 xlon2 ylat2 xlon3 ylat3 at-p)
  (alexandria:plist-alist
   (list
    :lon (+
          (* (expt (- 1.0 at-p) 3) xlon0)
          (* 3.0 (expt (- 1.0 at-p) 2) at-p xlon1)
          (* 3.0 (- 1.0 at-p) (expt at-p 2) xlon2)
          (* (expt at-p 3) xlon3))
    :lat (+
          (* (expt (- 1.0 at-p) 3) ylat0)
          (* 3.0 (expt (- 1.0 at-p) 2) at-p ylat1)
          (* 3.0 (- 1.0 at-p) (expt at-p 2) ylat2)
          (* (expt at-p 3) ylat3)))))


(defun bezierize-path (ordered-posn-list &key (anch-pos 0.5) (anch-dist 0.0) (num-results 10))
  (let* ((start-p (first ordered-posn-list))
         (start-xlon (rest (assoc :lon start-p)))
         (start-ylat (rest (assoc :lat start-p)))
         (end-p (first (last ordered-posn-list)))
         (end-xlon (rest (assoc :lon end-p)))
         (end-ylat (rest (assoc :lat end-p)))
         (ray-dist (sqrt
                    (+
                     (expt (- end-xlon start-xlon) 2)
                     (expt (- end-ylat start-ylat) 2))))
         (anch-ind (round (* anch-pos (length ordered-posn-list))))
         (mid-p (nth anch-ind ordered-posn-list))
         (mid-xlon (rest (assoc :lon mid-p)))
         (mid-ylat (rest (assoc :lat mid-p)))
         (adj-p (nth (if (< anch-pos 0.5)
                         (+ anch-ind 1)
                         (- anch-ind 1))
                     ordered-posn-list))
         (adj-xlon (rest (assoc :lon adj-p)))
         (adj-ylat (rest (assoc :lat adj-p)))
         (tangent-angle-deg
          (planesnwind.util:rad2deg
           (if (< anch-pos 0.5)
               (atan (- adj-xlon mid-xlon)
                     (- adj-ylat mid-ylat))
               (atan (- mid-xlon adj-xlon)
                     (- mid-ylat adj-ylat)))))
         (anch-v-angle-rad
          (planesnwind.util:deg2rad
           (+ tangent-angle-deg 0.0))) ;; 90
         (anch-xlon (+ mid-xlon
                       (* anch-dist ray-dist (cos anch-v-angle-rad))))
         (anch-ylat (+ mid-ylat
                       (* anch-dist ray-dist (sin anch-v-angle-rad)))))
    (loop for inp from 0.0 to 1.0 by (/ 1.0 num-results)
       collecting
         (bezier start-xlon start-ylat anch-xlon anch-ylat end-xlon end-ylat inp))))

(defun cubic-bezierize-path (ordered-posn-list
                             &key (anch1-pos 0.25) (anch1-dist 0.0)
                               (anch2-pos 0.75) (anch2-dist 0.0) (num-results 10))

  ;; this could be erm, tidier
  (let* ((start-p (first ordered-posn-list))
         (start-xlon (rest (assoc :lon start-p)))
         (start-ylat (rest (assoc :lat start-p)))
         (end-p (first (last ordered-posn-list)))
         (end-xlon (rest (assoc :lon end-p)))
         (end-ylat (rest (assoc :lat end-p)))
         (ray-dist (sqrt
                    (+
                     (expt (- end-xlon start-xlon) 2)
                     (expt (- end-ylat start-ylat) 2))))
         (anch1-ind (round (* anch1-pos (length ordered-posn-list))))
         (anch2-ind (round (* anch2-pos (length ordered-posn-list))))
         (mid1-p (nth anch1-ind ordered-posn-list))
         (mid1-xlon (rest (assoc :lon mid1-p)))
         (mid1-ylat (rest (assoc :lat mid1-p)))
         (adj1-p (nth (if (< anch1-pos 0.5)
                          (+ anch1-ind 1)
                          (- anch1-ind 1))
                      ordered-posn-list))
         (adj1-xlon (rest (assoc :lon adj1-p)))
         (adj1-ylat (rest (assoc :lat adj1-p)))
         (tangent-angle-deg1
          (planesnwind.util:rad2deg
           (if (< anch1-pos 0.5)
               (atan (- adj1-xlon mid1-xlon)
                     (- adj1-ylat mid1-ylat))
               (atan (- mid1-xlon adj1-xlon)
                     (- mid1-ylat adj1-ylat)))))
         (anch-v-angle-rad1
          (planesnwind.util:deg2rad
           (+ tangent-angle-deg1 0.0))) ;; 90
         (anch1-xlon (+ mid1-xlon
                       (* anch1-dist ray-dist (cos anch-v-angle-rad1))))
         (anch1-ylat (+ mid1-ylat
                        (* anch1-dist ray-dist (sin anch-v-angle-rad1))))
         (mid2-p (nth anch2-ind ordered-posn-list))
         (mid2-xlon (rest (assoc :lon mid2-p)))
         (mid2-ylat (rest (assoc :lat mid2-p)))
         (adj2-p (nth (if (< anch2-pos 0.5)
                          (+ anch2-ind 1)
                          (- anch2-ind 1))
                      ordered-posn-list))
         (adj2-xlon (rest (assoc :lon adj2-p)))
         (adj2-ylat (rest (assoc :lat adj2-p)))
         (tangent-angle-deg2
          (planesnwind.util:rad2deg
           (if (< anch2-pos 0.5)
               (atan (- adj2-xlon mid2-xlon)
                     (- adj2-ylat mid2-ylat))
               (atan (- mid2-xlon adj2-xlon)
                     (- mid2-ylat adj2-ylat)))))
         (anch-v-angle-rad2
          (planesnwind.util:deg2rad
           (+ tangent-angle-deg2 0.0))) ;; 90
         (anch2-xlon (+ mid2-xlon
                       (* anch2-dist ray-dist (cos anch-v-angle-rad2))))
         (anch2-ylat (+ mid2-ylat
                        (* anch2-dist ray-dist (sin anch-v-angle-rad2)))))
    (loop for inp from 0.0 to 1.0 by (/ 1.0 num-results)
       collecting
         (cubic-bezier start-xlon start-ylat
                       anch1-xlon anch1-ylat
                       anch2-xlon anch2-ylat
                       end-xlon end-ylat
                       inp))))

;; seems like optimal anchor posns are 0.2-0.8 and dists -0.4 to 0.4

(defun segmented-cubic-bezierize-path (ordered-posn-list anch-def
                                       &optional (num-results 50))

  #| allow diff num segments later
  '(pos-s1-a1 dis-s1-a1 pos-s1-a2 dis-s1-a2 splitpt1-dist-from-gc
    pos-s2-a1 dis-s2-a1 pos-s2-a2 dis-s2-a2 splitpt2-dist-from-gc
    pos-s3-a1 dis-s3-a1 pos-s3-a2 dis-s3-a2)
  |#

  (if (not (= 14 (length anch-def)))
      (error "only 3-seg handled for now, your conf list is wrong."))

  ;; this could be erm, way tidier
  (let* ((start-p (first ordered-posn-list))
         (start-xlon (rest (assoc :lon start-p)))
         (start-ylat (rest (assoc :lat start-p)))
         (end-p (first (last ordered-posn-list)))
         (end-xlon (rest (assoc :lon end-p)))
         (end-ylat (rest (assoc :lat end-p)))
         (ray-dist (sqrt
                    (+
                     (expt (- end-xlon start-xlon) 2)
                     (expt (- end-ylat start-ylat) 2))))
         (ray-angle-deg
          (mod (+ 360.0 (- 90.0 (rad2deg (atan (- end-ylat start-ylat)
                                               (- end-xlon start-xlon)))))
               360))
         (ray-tangent-deg (mod (+ (- ray-angle-deg 90) 360) 360)) ;; in airplane degrees
         (ray-tangent-math-ang (mod (+ (- 90 ray-tangent-deg) 360) 360))
         (ray-tang-rad (deg2rad ray-tangent-math-ang))
         (num-segments (/ (1+ (length anch-def)) 5.0))
         (last-splitpt-dist 0.0)
         (out-posns '()))

    ;;(format t "---- ray start ~3$,~3$, ~3$ long, dir ~1$ deg. tang ~1$ deg.~%"
    ;;        start-ylat start-xlon ray-dist ray-angle-deg ray-tangent-deg)

    (loop for seg-num from 0 below num-segments
       collecting
         (destructuring-bind (anch1-pos anch1-dist anch2-pos anch2-dist)
             (subseq anch-def (* seg-num 5) (+ (* seg-num 5) 4))
           (let* ((this-split-pt-dist (or (nth (+ 4 (* seg-num 5)) anch-def) 0.0))
                  (seg-gc-start-pt (interp-gc-pt start-xlon start-ylat
                                              end-xlon end-ylat
                                              (* seg-num (/ 1.0 num-segments))))
                  (seg-gc-end-pt (interp-gc-pt start-xlon start-ylat
                                              end-xlon end-ylat
                                              (* (1+ seg-num) (/ 1.0 num-segments))))
                  (seg-gc-start-xlon (getf seg-gc-start-pt :xlon))
                  (seg-gc-start-ylat (getf seg-gc-start-pt :ylat))
                  (seg-gc-end-xlon (getf seg-gc-end-pt :xlon))
                  (seg-gc-end-ylat (getf seg-gc-end-pt :ylat))
                  (seg-start-xlon (+ seg-gc-start-xlon
                                     (* (/ last-splitpt-dist num-segments) ray-dist (cos ray-tang-rad))))
                  (seg-start-ylat (+ seg-gc-start-ylat
                                     (* (/ last-splitpt-dist num-segments) ray-dist (sin ray-tang-rad))))
                  (seg-end-xlon (+ seg-gc-end-xlon
                                   (* (/ this-split-pt-dist num-segments) ray-dist (cos ray-tang-rad))))
                  (seg-end-ylat (+ seg-gc-end-ylat
                                   (* (/ this-split-pt-dist num-segments) ray-dist (sin ray-tang-rad))))
                  (seg-gc-anch1-pt (interp-gc-pt seg-start-xlon seg-start-ylat
                                                 seg-end-xlon seg-end-ylat
                                                 anch1-pos))
                  (seg-gc-anch2-pt (interp-gc-pt seg-start-xlon seg-start-ylat
                                                 seg-end-xlon seg-end-ylat
                                                 anch2-pos))
                  (seg-gc-anch1-xlon (getf seg-gc-anch1-pt :xlon))
                  (seg-gc-anch1-ylat (getf seg-gc-anch1-pt :ylat))
                  (seg-gc-anch2-xlon (getf seg-gc-anch2-pt :xlon))
                  (seg-gc-anch2-ylat (getf seg-gc-anch2-pt :ylat))
                  (seg-anch1-dxlon (* anch1-dist (/ ray-dist num-segments)
                                      (cos ray-tang-rad)))
                  (seg-anch1-dylat (* anch1-dist (/ ray-dist num-segments)
                                      (sin ray-tang-rad)))
                  (seg-anch2-dxlon (* anch2-dist (/ ray-dist num-segments)
                                      (cos ray-tang-rad)))
                  (seg-anch2-dylat (* anch2-dist (/ ray-dist num-segments)
                                      (sin ray-tang-rad))))

             #|(format t "-- seg ~d, gc pts ~2$,~2$ to ~2$,~2$; splitpt-dists ~1$ and ~1$.~%"
                     seg-num seg-gc-start-ylat seg-gc-start-xlon seg-gc-end-ylat
                     seg-gc-end-xlon last-splitpt-dist this-split-pt-dist)
             (format t "--     start ~2$,~2$ d ~2$,~2$~%"
                     seg-start-ylat seg-start-xlon
                     (- seg-start-ylat seg-gc-start-ylat)
                     (- seg-start-xlon seg-gc-start-xlon))
             (format t "--     anch1  ~2$,~2$ to ~2$,~2$ by ~2$,~2$.~%"
                     seg-gc-anch1-ylat seg-gc-anch1-xlon
                     (+ seg-anch1-dylat seg-gc-anch1-ylat) (+ seg-anch1-dxlon seg-gc-anch1-xlon)
                     seg-anch1-dylat seg-anch1-dxlon)
             (format t "--     anch2  ~2$,~2$ to ~2$,~2$ by ~2$,~2$.~%"
                     seg-gc-anch2-ylat seg-gc-anch2-xlon
                     (+ seg-anch2-dylat seg-gc-anch2-ylat) (+ seg-anch2-dxlon seg-gc-anch2-xlon)
                     seg-anch2-dylat seg-anch2-dxlon)
             (format t "--     end    ~2$,~2$ d ~2$,~2$.~%"
                     seg-end-ylat seg-end-xlon
                     (- seg-end-ylat seg-gc-end-ylat)
                     (- seg-end-xlon seg-gc-end-xlon))|#

             (let ((seg-pts
                    (loop for inp from 0.0 to 1.0 by (/ num-segments num-results)
                       collecting
                         (cubic-bezier seg-start-xlon seg-start-ylat
                                       (+ seg-anch1-dxlon seg-gc-anch1-xlon)
                                       (+ seg-anch1-dylat seg-gc-anch1-ylat)
                                       (+ seg-anch2-dxlon seg-gc-anch2-xlon)
                                       (+ seg-anch2-dylat seg-gc-anch2-ylat)
                                       seg-end-xlon seg-end-ylat
                                       inp))))
               (push (if (> seg-num 0)
                         (rest seg-pts)
                         seg-pts)
                     out-posns))

             (setf last-splitpt-dist this-split-pt-dist))))

    ;;(format t "---- ray end ~3$,~3$.~%" end-ylat end-xlon)

    (reduce #'append
            (reverse
             out-posns))))

(defun trim-path (path-list &optional (min-fl 100))
  (remove-if-not
   #'(lambda (o)
       (> (rest (assoc :alt o)) min-fl))
   path-list))

#|
;; todo: test this more
(defun vect-add (v1-r v1-theta-deg v2-r v2-theta-deg)
  (let* ((dx (+ (* v1-r (cos (planesnwind.util:deg2rad v1-theta-deg)))
                (* v2-r (cos (planesnwind.util:deg2rad v2-theta-deg)))))
         (dy (+ (* v1-r (sin (planesnwind.util:deg2rad v1-theta-deg)))
                (* v2-r (sin (planesnwind.util:deg2rad v2-theta-deg)))))
         (rdeg (planesnwind.util:rad2deg
                (atan dy dx)))
         (rmag (sqrt (+ (expt dx 2) (expt dy 2))))
         (rdeg-adj (cond
                     ((< rdeg 0.0)
                      (+ 360.0 rdeg))
                     ((> rdeg 360.0)
                      (- rdeg 360.0))
                     (t
                      rdeg))))

    ;; actually the problem is more finding the tailwind component of the wind
    ;; for a given plane dir
    ))
|#

(defun calc-twind-comp (plane-dir wind-spd wind-headed-dir)
  (* wind-spd (cos (deg2rad (- plane-dir wind-headed-dir)))))

(defun recip-dir (d)
  (let ((r (- d 180.0)))
    (if (< r 0.0)
        (+ 360.0 r)
        r)))

(defun evaluate-path (ordered-posn-list &key (airspeed 160))
  (let* ((start-pt (first ordered-posn-list))
         (start-xlon (rest (assoc :lon start-pt)))
         (start-ylat (rest (assoc :lat start-pt)))
         (last-xlon start-xlon)
         (last-ylat start-ylat)
         (tdist 0.0)
         (tsec 0.0))
    (loop for this-pt in (subseq ordered-posn-list 1)
       do
         (let* ((this-xlon (rest (assoc :lon this-pt)))
                (this-ylat (rest (assoc :lat this-pt)))
                (mid-xlon (/ (+ this-xlon last-xlon) 2.0))
                (mid-ylat (/ (+ this-ylat last-ylat) 2.0))
                (seg-dir-deg
                 (recip-dir (planesnwind.util:deg2dir
                     (planesnwind.util:rad2deg
                      (atan (- this-ylat last-ylat)
                            (- this-xlon last-xlon))))))
                (seg-dist (sqrt (+ (expt (- this-xlon last-xlon) 2)
                                   (expt (- this-ylat last-ylat) 2))))
                (wdir (planesnwind.winds:query-this-wind-model :wdir mid-xlon mid-ylat 34000)) ;;todo:
                (wspd (planesnwind.winds:query-this-wind-model :wspd mid-xlon mid-ylat 34000))
                (twcomp (calc-twind-comp seg-dir-deg wspd wdir)))
           ;; (calc-twind-comp seg-dir-deg wspd wdir)
           (format t "-- at ~3$,~3$: seg dist ~2$ at dir ~2$ deg. wind ~1$ from ~1$ deg. ~%- twc ~3$, spd ~1$, secs ~1$~%"
                   mid-ylat mid-xlon seg-dist seg-dir-deg wspd wdir twcomp
                   (+ airspeed twcomp) (/ seg-dist (+ airspeed twcomp)))

           (calc-twind-comp seg-dir-deg wspd wdir)

           (incf tdist seg-dist)
           (incf tsec (/ seg-dist (+ airspeed twcomp)))))
    (format t "---- path total dist ~2$, secs ~2$.~%" tdist tsec)
    (values tsec tdist)))
;; units n stuff
;; confirm angles

;; (planesnwind.http:set-current-flightpath (segmented-cubic-bezierize-path fpa '(0.3 0.5 0.7 0.0 0.1 0.3 0.0 0.7 0.0 -0.1 0.3 0.0 0.7 0.0) :num-results 70))

;;(import 'planesnwind.winds:query-wind-model)

#|
(evaluate-flightpath (analyze-flightpath ff) :path-mod-fn #'(lambda (l) (segmented-cubic-bezierize-path l (list 0.3 0.0 0.7 0.0 0.0 0.3 0.2 0.7 0.0 0.0 0.3 0.0 0.7 0.0) :num-results 50)))
|#

#|
PATHS> (optimize-fp ff)

(16156.937201662244d0
 (0.3 -0.0107572265 0.7 -0.04792226 -0.12201778 0.3 -0.0087472 0.7 0.025934346
      -0.1208367 0.3 -0.057088826 0.7 -0.07797009))
16156.937201662244d0
115.11526393036183d0
PATHS>

|#

(defun optimize-restricted ()
  (let* ((fids-1 (planesnwind.db:select-fids-with-tracks 1))
        (fids-2 (planesnwind.db:select-fids-with-tracks 2))
        (fids-wo-rest (remove-if
                       #'(lambda (f) (find f fids-2)) fids-1))
        (n 0))
    (loop for fid in fids-wo-rest
       do
         (progn
           (format t "-- ~d of ~d, ~1$, fid ~d~%" (incf n) (length fids-wo-rest)
                   (* 100.0 (/ n (length fids-wo-rest))) fid)
           (handler-case (fetch-and-optimize-and-save-fp fid :flight-track-type 2)
             (error (c)
               (format t "condition during fid ~d: ~a~%" fid c)
               (planesnwind.db:hstore-upsert :flights (list (cons "rejected" "true")) fid)))
           7))))

(defun optimize-all ()

  (let ((fid (planesnwind.db:g))
        (n 0))
    (loop while fid
       do
         (progn
           (handler-case (fetch-and-optimize-and-save-fp fid :flight-track-type 2)
             (error (c)
               (format t "condition during fid ~d: ~a~%" fid c)
               (planesnwind.db:hstore-upsert :flights (list (cons "rejected" "true")) fid)))
           (ignore-errors
             (fetch-and-optimize-and-save-fp fid :flight-track-type 2))
           (incf n)
           (setf fid
                 (planesnwind.db:g))))
    n))

@export
(defun check-then-optimize-and-save-flight (fid)
  (planesnwind.worker:worker-log "fid-start-check" (format nil "~a" (round fid)))

  (let* ((fl-details (planesnwind.db:get-flight-details-for-fid fid))
         (runstate (getf fl-details :runstate))
         (wsid (planesnwind.winds:ensure-or-build-wind-model
                (planesnwind.winds:find-wset-id-for-ts
                 (round (getf fl-details :dept-ts))))))

    (planesnwind.winds:set-this-wsid wsid) ;; todo:

    (cond
      ((or (not fl-details)
           (not wsid))
       (format t "!!!!! missing flight info for given fid ~a, wset id ~a!~%" fid wsid)
       (planesnwind.worker:worker-log "fid-missing-details" (format nil "fid ~a, wset-id ~a"
                                                                    (round fid)
                                                                    (round wsid)))
       nil)
      ((= 0 runstate)
       (format t "--- fid ~a at runstate ~d, wsid ~a, processing unconstrained.~%"
               fid runstate wsid)
       (if (planesnwind.db:flight-has-track-p fid 1)
           (progn
             (format t "--- actually punting, fid ~a already has a path of type 1.~%"
                     fid)
             (planesnwind.worker:worker-log "fid-already-has-path-1" (format nil "~a" (round fid)))
             (planesnwind.db:update-runstate-for-fid fid 1)
             nil)
           (fetch-and-optimize-and-save-fp fid :flight-track-type 1 :constrained-p nil)))
      ((= 1 runstate)
       (format t "--- fid ~a at runstate ~d, wsid ~a, processing constrained.~%"
               fid runstate wsid)
       (if (planesnwind.db:flight-has-track-p fid 2)
           (progn
             (planesnwind.worker:worker-log "fid-already-has-path-2" (format nil "~a" (round fid)))
             (format t "--- actually punting, fid ~a already has a path of type 2.~%"
                     fid)
             (planesnwind.db:update-runstate-for-fid fid 3)
             nil)
           (fetch-and-optimize-and-save-fp fid :flight-track-type 2 :constrained-p t)))
      ((or (= -1 runstate)
           (= -2 runstate))
       (format t "--- fid ~a at runstate ~d, punting.~%" fid runstate)
       (planesnwind.worker:worker-log "fid-wrong-runstate" (format nil "fid ~a, runstate ~a"
                                                                   (round fid) (round runstate)))
       nil)
      (t
       (format t "--- fid ~a at unexpected runstate ~d, punting.~%" fid runstate)
       (planesnwind.worker:worker-log "fid-wrong-runstate" (format nil "fid ~a, runstate ~a"
                                                                   (round fid) (round runstate)))
       nil))))

(defun fetch-and-optimize-and-save-fp (fid &key (flight-track-type 1) (constrained-p t))
  (format t "~d-" fid)

  (planesnwind.worker:worker-log "fid-opt-start" (format nil "fid ~a, tracktype ~a"
                                                         (round fid)
                                                         flight-track-type))
  (planesnwind.db:update-runstate-for-fid fid (round (* -10 flight-track-type)))

  (let ((fpath (planesnwind.db:fetch-flightpath-by-flightid fid))
        (start-ts (/ (float (get-internal-real-time)) internal-time-units-per-second)))
    (if (< (length fpath) 10)
        (progn
          (format t "rejecting fp for id ~d, only has ~d pts: ~a~%"
                  fid (length fpath) fpath)
          (planesnwind.worker:worker-log "fid-opt-tooshort" (format nil "fid ~a, pts ~a"
                                                                    (round fid)
                                                                    (round (length fpath))))
          (planesnwind.db:update-runstate-for-fid fid -1)
          nil)
        (multiple-value-bind (res fanalysis dsecs ddist)
            (optimize-fp fpath :filter-shorter-p constrained-p)

          (declare (ignore dsecs ddist))
          (destructuring-bind (&key orig-seg-real-secs orig-seg-sim-secs gc-seg-secs
                                    best-seg-secs orig-seg-sim-d
                                    gc-seg-d best-seg-d best-params
                                    optimize-secs paths-checked
                                    opt-restricted found-better-path)
              res

            (declare (ignore orig-seg-real-secs gc-seg-secs orig-seg-sim-d gc-seg-d best-seg-d paths-checked opt-restricted found-better-path))

            (planesnwind.worker:worker-log "fid-opt-secs" "" optimize-secs)

            (let* ((start-ind (getf fanalysis :select-start-ind))
                   (end-ind (getf fanalysis :select-end-ind))
                   (orig-path-seg (subseq fpath
                                          start-ind
                                          (1+ end-ind)))
                   (best-path-seg
                    (segmented-cubic-bezierize-path orig-path-seg best-params
                                                    (- end-ind start-ind))))

              ;;(planesnwind.http::set-orig-flightpath fpath)
              ;;(planesnwind.http::set-current-flightpath
              ;; best-path-seg)

              (let ((res-chunk (append res fanalysis)))
                (setf (getf res-chunk :best-params)
                      (cl-json:encode-json-to-string
                       (getf res-chunk :best-params)))
                (planesnwind.db:hstore-upsert :flights
                                              (if constrained-p
                                                  (loop for node in (alexandria:plist-alist res-chunk)
                                                     collecting
                                                       (cons (format nil "CONSTR-~a"
                                                                     (first node))
                                                             (rest node)))
                                                  (alexandria:plist-alist res-chunk))
                                              fid)
                (planesnwind.db:insert-flight-track fid best-path-seg :track-type flight-track-type)

                (planesnwind.worker:worker-log (if constrained-p
                                                   "fid-opt-constrained-perc"
                                                   "fid-opt-unconstrained-perc") (format nil "fid ~a"
                                                                                         (round fid))
                                               (* 100.0 (/ (- best-seg-secs orig-seg-sim-secs) orig-seg-sim-secs)))

                (format t "-- id ~d: opt ~1$%, ~d pts to db, ~3$ sec.~%"
                        fid (* 100.0 (/ (- best-seg-secs orig-seg-sim-secs) orig-seg-sim-secs))
                        (- end-ind start-ind)
                        (- (/ (float (get-internal-real-time)) internal-time-units-per-second)
                           start-ts))

                (planesnwind.db:update-runstate-for-fid
                 fid (if constrained-p 2 1))
                res-chunk)))))))

(defun optimize-fp (pathlist &key (num-paths-per-run 20) (num-runs 200) (filter-shorter-p t))
  (let ((analysis (analyze-flightpath pathlist))
        (best-secs)
        (best-d)
        (best-params (if filter-shorter-p
                         (list 0.3 0.1 0.7 0.1 0.1 0.3 0.1 0.7 0.1 0.1 0.3 0.1 0.7 0.1)
                         (list 0.3 0.0 0.7 0.0 0.0 0.3 0.0 0.7 0.0 0.0 0.3 0.0 0.7 0.0)))
        (start-ts (/ (float (get-internal-real-time)) internal-time-units-per-second))
        (n 0)
        (r))

    (multiple-value-bind (orig-secs orig-d)
        (evaluate-flightpath analysis pathlist)
      (multiple-value-bind (gc-secs gc-d)
          (evaluate-flightpath
           analysis
           pathlist
           :path-mod-fn #'segmented-cubic-bezierize-path
           :path-mod-params best-params)

        (setf best-secs orig-secs)
        (setf best-d orig-d)

        (loop for i from 0 below num-runs
           do
             (let ((list-of-psets (list best-params)))
               (loop for j from 0 below num-paths-per-run
                  do
                    (push (alter-params best-params)
                          list-of-psets))
               (let* ((res
                       (loop for pset in list-of-psets
                          collecting
                            (multiple-value-bind (p-secs p-dist)
                                (evaluate-flightpath analysis
                                                     pathlist
                                                     :path-mod-fn #'segmented-cubic-bezierize-path
                                                     :path-mod-params pset)
                              (incf n)
                              (list
                               p-secs
                               p-dist
                               pset))))
                      (filtered-r (if filter-shorter-p
                                      (remove-if #'(lambda (o) (< (second o) orig-d)) res)
                                      res))
                      (sorted-r (sort filtered-r #'< :key #'first))
                      (best-r (if sorted-r
                                  (first sorted-r))))
                 (when best-r
                   (setf r best-r)
                   (setf best-secs (first best-r))
                   (setf best-d (second best-r))
                   (setf best-params (third best-r))))))

        #|
        (format t "best sec ~a orig ~a~%" best-secs orig-secs)
        (format t "best d ~a orig ~a~%" best-d orig-d)
        (format t "r ~a~%" r)
        |#

        (values
         (list
          :opt-restricted (if filter-shorter-p "true" "false")
          :found-better-path (if filter-shorter-p
                                 (if r "true" "false")
                                 "true")
          :orig-seg-real-secs (getf analysis :selected-secs)
          :orig-seg-sim-secs orig-secs
          :gc-seg-secs gc-secs
          :best-seg-secs best-secs
          :orig-seg-sim-d orig-d
          :gc-seg-d gc-d
          :best-seg-d best-d
          :best-params best-params
          :optimize-secs (- (/ (float (get-internal-real-time)) internal-time-units-per-second)
                            start-ts)
          :paths-checked n)
         analysis
         (- best-secs orig-secs)
         (- best-d orig-d))))))

(defun alter-params (p)
  (let ((s (random 3.0)))
    (if (< s 1.0)
        (let ((p-filter (list 1 3 4 6 8 9 11 13)))
          (loop for i from 0 below (length p)
             collecting
               (if (find i p-filter)
                   (+ (nth i p) (- (random 0.1) 0.05))
                   (nth i p))))
        (if (< s 2.0)
            (loop for i from 0 below (length p)
               collecting
                 (+ (nth i p) (- (random 0.1) 0.05)))
            (loop for i from 0 below (length p)
               collecting
                 (if (> (random 1.0) 0.5)
                     (- (random 0.3) 0.15)
                     (nth i p)))))))

(defun evaluate-flightpath (fp-analysis pathlist &key path-mod-fn path-mod-params)
  ;; this is uh
  (destructuring-bind (&key select-start-ind climb-end-ind
                            desc-start-ind select-end-ind roc-fps rod-fps
                            cruise-alt total-secs selected-secs climb-aspd
                            cruise-aspd desc-aspd)
      fp-analysis
    (let* ((tsum 0.0)
           (dsum 0.0)
           (orig-seg-selected (subseq pathlist select-start-ind select-end-ind))
           (pathlist-used orig-seg-selected)
           (actual-start-ind 0)
           (actual-end-ind (length pathlist-used))
           (first-pt (nth actual-start-ind pathlist))
           (last-xlon (rest (assoc :lon first-pt)))
           (last-ylat (rest (assoc :lat first-pt)))
           (last-alt (* 100.0 (rest (assoc :alt (nth select-start-ind pathlist)))))
           (end-pt (nth select-end-ind pathlist))
           (end-xlon (rest (assoc :lon end-pt)))
           (end-ylat (rest (assoc :lat end-pt)))
           (end-alt (rest (assoc :alt end-pt)))
           (seg-aspd climb-aspd)
           (state :climb)
           (desc-secs (/ (- cruise-alt 10000.0) rod-fps))
           (desc-dist-m (abs (* desc-secs desc-aspd))))
      ;;(format t "hi ~a ~a ~a ~a~%" last-xlon last-ylat end-xlon end-ylat)

      (when path-mod-fn
        (setf pathlist-used
              (apply path-mod-fn
                     (list
                      orig-seg-selected
                      path-mod-params
                      (length orig-seg-selected))))
        (setf actual-start-ind 0)
        (setf actual-end-ind (- (length pathlist-used) 1)))

      (loop for this-ind from (1+ actual-start-ind) below actual-end-ind
         do
           (progn
             ;;(format t "~d from ~d to ~d: ~a~%" this-ind (1+ actual-start-ind)
             ;;        actual-end-ind (length pathlist-used))

             (let* ((this-pt (nth this-ind pathlist-used))
                    (this-xlon (rest (assoc :lon this-pt)))
                    (this-ylat (rest (assoc :lat this-pt)))
                    (dist-m (haversine-etc last-xlon last-ylat this-xlon this-ylat))
                    (dist-to-end-m (haversine-etc this-xlon this-ylat end-xlon end-ylat))
                    (seg-dir-acft (- 90.0 (rad2deg
                                           (atan (- this-ylat last-ylat)
                                                 (- this-xlon last-xlon)))))
                    (mid-xlon (/ (+ this-xlon last-xlon) 2.0))
                    (mid-ylat (/ (+ this-ylat last-ylat) 2.0))
                    (wdir (planesnwind.winds:query-this-wind-model :wdir mid-xlon mid-ylat last-alt))
                    (wind-headed-dir (mod (+ wdir 180.0) 360))
                    (wspd (planesnwind.winds:query-this-wind-model :wspd mid-xlon mid-ylat last-alt))
                    (twind (calc-twind-comp seg-dir-acft wspd wind-headed-dir))
                    (groundspeed-kts (+ twind seg-aspd))
                    (time-secs (/ dist-m (kts-to-m-s groundspeed-kts))))
               (cond
                 ((eql :climb state)
                  (if (< last-alt (* 0.95 cruise-alt))
                      (incf last-alt (* roc-fps time-secs))
                      (progn
                        (setf last-alt cruise-alt)
                        (setf seg-aspd cruise-aspd)
                        (setf state :cruise))))
                 ((eql :cruise state)
                  (if (< dist-to-end-m desc-dist-m)
                      (progn
                        (setf seg-aspd desc-aspd)
                        (setf state :desc))))
                 ((eql :desc state)
                  ;; todo: desc too fast sometimes
                  (if (> last-alt 10000)
                      (incf last-alt (* rod-fps time-secs)))))

               (incf tsum time-secs)
               (incf dsum dist-m)

               #|
               (format t "~a alt ~1$ distm ~1$ dir ~1$ wdir ~1$ tw ~1$ gs ~1$ secs ~1$ dte ~1$. dn ~1$~%"
               state last-alt dist-m seg-dir-acft wind-headed-dir twind groundspeed-kts
               time-secs dist-to-end-m desc-dist-m)
               |#

               (setf last-xlon this-xlon)
               (setf last-ylat this-ylat))))
      (values
       tsum
       dsum))))

(defun analyze-flightpath (pathlist)
  (let* ((first-pt (first pathlist))
         (path-start-sec (rest (assoc :tss first-pt)))
         (last-pt (first (last pathlist)))
         (path-end-sec (rest (assoc :tss last-pt)))
         (cruise-alt (analyze-cruise-alt pathlist)))
    (multiple-value-bind (rates select-start-ind climb-end-ind desc-start-ind select-end-ind)
        (analyze-climb-desc pathlist cruise-alt)
      (let ((selected-start-sec (rest (assoc :tss (nth select-start-ind pathlist))))
            (selected-end-sec (rest (assoc :tss (nth select-end-ind pathlist)))))
        (list
         :select-start-ind select-start-ind
         :climb-end-ind climb-end-ind
         :desc-start-ind desc-start-ind
         :select-end-ind select-end-ind
         :roc-fps (first rates)
         :rod-fps (second rates)
         :cruise-alt cruise-alt
         :total-secs (- path-end-sec path-start-sec)
         :selected-secs (- selected-end-sec selected-start-sec)
         :climb-aspd (avg-airspeed pathlist select-start-ind climb-end-ind)
         :cruise-aspd (avg-airspeed pathlist climb-end-ind desc-start-ind)
         :desc-aspd (avg-airspeed pathlist desc-start-ind select-end-ind))))))

(defun meters-s-to-kts (mps)
  (* 1.94384 mps))

(defun kts-to-m-s (kts)
  (/ kts 1.94384))

(defun avg-airspeed (pts start-ind end-ind)
  (let* ((distsum 0.0)
         (tw-sum 0.0)
         (num 0)
        (first-pt (nth start-ind pts))
        (last-secs (rest (assoc :tss first-pt)))
        (last-xlon (rest (assoc :lon first-pt)))
        (last-ylat (rest (assoc :lat first-pt)))
         (last-alt (* 100.0 (rest (assoc :alt first-pt)))))
    (loop for pti from (1+ start-ind) to end-ind
       do
         (let* ((this-pt (nth pti pts))
                (this-secs (rest (assoc :tss this-pt)))
                (this-xlon (rest (assoc :lon this-pt)))
                (this-ylat (rest (assoc :lat this-pt)))
                (this-alt (* 100.0 (rest (assoc :alt this-pt))))
                (dist (haversine-etc last-xlon last-ylat this-xlon this-ylat))
                (plane-dir (- 90.0
                              (rad2deg (atan (- this-ylat last-ylat)
                                             (- this-xlon last-xlon)))))
                (plane-spd (meters-s-to-kts
                            (/ dist (- this-secs last-secs))))
                (mid-xlon (/ (+ this-xlon last-xlon) 2.0))
                (mid-ylat (/ (+ this-ylat last-ylat) 2.0))
                (mid-alt (/ (+ last-alt this-alt) 2.0))
                (wind-dir (planesnwind.winds:query-this-wind-model :wdir mid-xlon mid-ylat mid-alt))
                (wind-spd (planesnwind.winds:query-this-wind-model :wspd mid-xlon mid-ylat mid-alt)))

           ;;(format t "-- ~3$,~3$ at ~2$ ft --> ~2$ kts~%" mid-xlon mid-ylat mid-alt wind-spd)

           (when wind-dir
             ;;(format t "plane ~1$ kts at ~1$ deg, wind ~1$ kts headed ~1$ deg, tw ~1$.~%"
             ;;        plane-spd plane-dir wind-spd (recip-dir wind-dir)
             ;;        (calc-twind-comp plane-dir wind-spd (recip-dir wind-dir)))

             (incf num)
             (incf distsum dist)
             (incf tw-sum (calc-twind-comp plane-dir wind-spd (recip-dir wind-dir))))

           (setf last-secs this-secs)
           (setf last-xlon this-xlon)
           (setf last-ylat this-ylat)
           (setf last-alt this-alt)))
    (let ((avg-gnd-kts (meters-s-to-kts
                        (/ distsum (- last-secs (rest (assoc :tss first-pt))))))
          (avg-tw (/ tw-sum num)))
      (- avg-gnd-kts avg-tw))))

(defun analyze-cruise-alt (pathlist)
  (let* ((pathpts (length pathlist))
         (num 0)
         (altsum 0.0))
    (loop for n from (round (* 0.3 pathpts)) to (round (* 0.7 pathpts))
       do
         (let ((this-pt (nth n pathlist)))
           (incf altsum (* 100 (rest (assoc :alt this-pt))))
           (incf num)))
    (/ altsum num)))

;; rates in fps
(defun analyze-climb-desc (pathlist cruise-alt)
  ;; todo: cleanup
  (let ((cruise-fl (/ cruise-alt 100.0))
        (ind 0)
        (ourpath-start-ind)
        (ourpath-end-ind)
        (climb-start-sec)
        (climb-start-alt)
        (climb-end-ind)
        (climb-end-sec)
        (climb-end-alt)
        (desc-start-ind)
        (desc-start-sec)
        (desc-start-alt)
        (desc-end-sec)
        (desc-end-alt))
    (loop while (< (rest (assoc :alt (nth (incf ind) pathlist))) 100)) ;; todo: config

    (setf ourpath-start-ind ind)
    (setf climb-start-sec (rest (assoc :tss (nth ind pathlist))))
    (setf climb-start-alt (rest (assoc :alt (nth ind pathlist))))

    (loop while (< (rest (assoc :alt (nth (incf ind) pathlist))) (* 0.9 cruise-fl)))

    (setf climb-end-ind ind)
    (setf climb-end-sec (rest (assoc :tss (nth ind pathlist))))
    (setf climb-end-alt (rest (assoc :alt (nth ind pathlist))))

    (loop while (> (rest (assoc :alt (nth (incf ind) pathlist))) (* 0.9 cruise-fl)))

    (setf desc-start-ind ind)
    (setf desc-start-sec (rest (assoc :tss (nth ind pathlist))))
    (setf desc-start-alt (rest (assoc :alt (nth ind pathlist))))

    (loop while (> (rest (assoc :alt (nth (incf ind) pathlist))) 100)) ;; todo: config

    (setf ourpath-end-ind ind)
    (setf desc-end-sec (rest (assoc :tss (nth ind pathlist))))
    (setf desc-end-alt (rest (assoc :alt (nth ind pathlist))))

    #|(format t "climb ~1$ sec alt ~1$ to ~1$~%"
            (- climb-end-sec climb-start-sec)
            climb-start-alt climb-end-alt)
    (format t "desc  ~1$ sec alt ~1$ to ~1$~%"
            (- desc-end-sec desc-start-sec)
    desc-start-alt desc-end-alt)|#

    (values
     (list
      (/ (* 100.0 (- climb-end-alt climb-start-alt))
         (- climb-end-sec climb-start-sec))
      (/ (* 100.0 (- desc-end-alt desc-start-alt))
         (- desc-end-sec desc-start-sec)))
     ourpath-start-ind
     climb-end-ind
     desc-start-ind
     ourpath-end-ind)))
