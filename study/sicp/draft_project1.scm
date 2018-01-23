(define time-to-impact
  (lambda (vertical-velocity elevation)
    (let ((v vertical-velocity)
          (h elevation)
          (a 9.8))
      (root1 (a v h)))))


(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (time-to-impact vertical-(velocity
                    (- elevation target-elevation))))

(define pi (acos -1))

(define degree2radian
  (lambda (deg)
    (/ (* deg pi) 180.)))

(define travel-ndistance-simple
  (lambda (elevation velocity angle)
    (let ((angle-radian (degree2radian angle))
          (vertical-velocity (* velocity
                                (cos angle-radian)))
          (horizontal-velocity (* velocity
                                  (sin angle-radian)))
          (flight-time (time-to-impact vertical-velocity
                                       elevation)))
      (position 0 horizontal-velocity 0 flight-time))))

(define find-best-angle
  (lambda (velocity elevation)
    (let ((epsilon 0.01)
          (my-distance
           (lambda (angle) (travel-distance-simple
                            elevation velocity angle)))
          (find-best-angle-helper
           (lambda (angle)
             (if (> (my-distance angle)
                    (my-distance (+ angle epsilon)))
                    angle
                    (find-best-angle-helper (+ angle epsilon))))))
      (find-best-angle-helper 0))))


(define precision 0.5)

(define abs-difference
  (lambda (x y) (abs (- x y))))

(abs-difference -2 3)  ; -> 5
(abs-difference 3 -2)  ; -> 5
(abs-difference 3 2)   ; -> 1
(abs-difference -3 -2) ; -> 1

(define angle-increment 1)

(define find-angle-to-distance-helper
  (lambda (elevation velocity target-distance angle angle-list)
    (let* ((modeled-distance (travel-distance elevation velocity angle))
           (updated-angle-list
            (if (<= (abs-difference modeled-distance target-distance)
                    precision)
                (append angle-list (list angle))
                angle-list)))
      (if (> angle 90)
          angle-list
          (find-angle-to-distance-helper
           elevation
           velocity
           target-distance
           (+ angle angle-increment)
           updated-angle-list)))))

(define find-angle-to-distance
  (lambda (elevation velocity distance)
    (find-angle-to-distance-helper
     elevation velocity distance -90 '())))

(find-angle-to-distance 1 45 0)
(find-angle-to-distance 1 45 92.23)

(define integrate-t
  (lambda (t dt)
    (+ t dt)))

(define integrate-with-time-helper
  (lambda (x y u v t dt g m beta)
    (if (<= y 0)
        (list x t)
        (integrate-with-time-helper
         (integrate-x x u dt)
         (integrate-y y v dt)
         (integrate-u u v dt g m beta)
         (integrate-v u v dt g m beta)
         (integrate-t t dt)
         dt
         g
         m
         beta))))

(define integrate-with-time
  (lambda (x0 y0 u0 v0 t0 dt g m beta)
    (integrate-with-time-helper x0 y0 u0 v0 t0 dt g m beta)))

(define travel-distance-and-time
  (lambda (elevation velocity angle)
    (let ((x0 0)
          (y0 elevation)
          (u0 (horizontal-velocity velocity angle))
          (v0 (vertical-velocity velocity angle))
          (t0 0)
          (dt 0.01))
      (integrate-with-time x0 y0 u0 v0 t0 dt gravity mass beta))))

(travel-distance-and-time 1 45 0)  ; -> 19.34 meters
(travel-distance-and-time 1 45 90) ; -> ~ 0 meters
(travel-distance-and-time 1 45 45) ; -> 92.23 meters

;; Bad code, a lot of code reuse. Refactor later

(define find-angle-and-time-to-distance-helper
  (lambda (elevation velocity target-distance angle angle-time-list)
    (let* ((modeled-distance
            (car (travel-distance-and-time elevation velocity angle)))
           (modeled-time
            (cadr (travel-distance-and-time elevation velocity angle)))
           (updated-angle-time-list
            (if (<= (abs-difference modeled-distance target-distance)
                    precision)
                (append angle-time-list
                        (list (cons angle modeled-time)))
                angle-time-list)))
      (if (> angle 90)
          angle-time-list
          (find-angle-and-time-to-distance-helper
           elevation
           velocity
           target-distance
           (+ angle angle-increment)
           updated-angle-time-list)))))

(define find-angle-and-time-to-distance
  (lambda (elevation velocity distance)
    (find-angle-and-time-to-distance-helper
     elevation velocity distance -90 '())))

(find-angle-to-distance 1 45 0)
(find-angle-to-distance 1 45 92.23)

(find-angle-and-time-to-distance 1 45 0)
(find-angle-and-time-to-distance 1 45 92.23)

(travel-distance 1 45 32)
(travel-distance 1 45 33)
(travel-distance 1 45 34)
(travel-distance 1 45 35)

(define large-pair (cons #f (power 2 100)))

(define cdr-min-helper
  (lambda (list-of-pairs min-pair)
    (let* ((this-pair (car list-of-pairs))
           (out-pair
            (if (<= (cdr this-pair)
                    (cdr min-pair))
                this-pair
                min-pair)))
      (if (null? (cdr list-of-pairs))
          out-pair
          (cdr-min-helper (cdr list-of-pairs)
                          out-pair)))))

(define cdr-min
  (lambda (list-of-pairs)
    (cdr-min-helper list-of-pairs large-pair)))

(cdr-min (list (cons 1 100) (cons 2 10)))

(define fastest-trajectory-to-distance
  (lambda (elevation velocity distance)
    (cdr-min (find-angle-and-time-to-distance
              elevation velocity distance))))

(fastest-trajectory-to-distance 1 45 0)
(fastest-trajectory-to-distance 1 45 92.23)

(define travel-distance-with-bounces-simple-helper
         (lambda (elevation velocity angle number-of-bounces
                             traveled-distance i)
            (if (> i number-of-bounces)
                traveled-distance
                (travel-distance-with-bounces-simple-helper
                 0
                 (/ velocity 2)
                 angle
                 number-of-bounces
                 (+ traveled-distance
                    (travel-distance elevation velocity angle))
                 (+ i 1)))))

(define travel-distance-with-bounces-simple
  (lambda (elevation velocity angle number-of-bounces)
    (travel-distance-with-bounces-simple-helper
     elevation velocity angle number-of-bounces 0 0))))

(travel-distance-with-bounces-simple 1 45 0 0)
(travel-distance-with-bounces-simple 1 45 0 1)
(travel-distance-with-bounces-simple 1 45 45 0)
(travel-distance-with-bounces-simple 1 45 45 1)

(define make-posvel
  (lambda (x y u v)
    (list x y u v)))

(define make-initial-posvel
  (lambda (elevation velocity angle)
    (make-posvel
     0
     elevation
     (horizontal-velocity velocity angle)
     (vertical-velocity velocity angle))))

(define get-x
  (lambda (posvel)
    (list-ref posvel 0)))

(define get-y
  (lambda (posvel)
    (list-ref posvel 1)))

(define get-u
  (lambda (posvel)
    (list-ref posvel 2)))

(define get-v
  (lambda (posvel)
    (list-ref posvel 3)))

(define integrate-helper2
  (lambda (x y u v dt g m beta)
    (let ((x-new (integrate-x x u dt))
          (y-new (integrate-y y v dt))
          (u-new (integrate-u u v dt g m beta))
          (v-new (integrate-v u v dt g m beta)))
    (if (<= y-new 0)
        (make-posvel x y u v)
        (integrate-helper2 x-new y-new u-new v-new dt g m beta)))))

(define integrate2
  (lambda (x0 y0 u0 v0 dt g m beta)
    (integrate-helper2 x0 y0 u0 v0 dt g m beta)))

(define travel-distance2
  (lambda (x0 y0 u0 v0)
    (let ((dt 0.01))
      (integrate2 x0 y0 u0 v0 dt gravity mass beta))))

(travel-distance2 0 1 31.81980515339464 31.819805153394636)

(define travel-posvel
  (lambda (posvel)
    (travel-distance2
     (get-x posvel)
     (get-y posvel)
     (get-u posvel)
     (get-v posvel))))

(define epsilon 0.1)

(define travel-distance-with-bounces-helper
  (lambda (traveled-posvel)
    (let* ((this-x (get-x traveled-posvel))
           (this-y (get-y traveled-posvel))
           (this-u (get-u traveled-posvel))
           (this-v (get-v traveled-posvel)))
      (if (or (<= this-u epsilon) (<= (abs this-v) epsilon))
          this-x
          (travel-distance-with-bounces-helper
           (travel-distance2
            this-x this-y this-u (abs this-v)))))))

(define travel-distance-with-bounces
  (lambda (elevation velocity angle)
    (travel-distance-with-bounces-helper
     (make-initial-posvel elevation velocity angle))))

(travel-distance-with-bounces 1 45 90) ; -> 0
(travel-distance-with-bounces 1 45 0)  ; -> 0
(travel-distance-with-bounces 1 45 45) ; -> 232.94

(make-initial-posvel 1 45 45) ; -> (0 1 31.81980515339464 31.819805153394636)

(travel-posvel (make-posvel 0 1 31.81980515339464 31.819805153394636))

(travel-distance2 0 1 31.81980515339464 31.819805153394636)

(travel-distance2 0 1 45 45) ; -> (92.12252786947717 .13332396910813327 10.803079666460809 -21.125617939178348)
(travel-distance2 0 1 45 45) ; -> (92.12252786947717 .13332396910813327 10.803079666460809 -21.125617939178348)
(travel-distance 1 45 45)

(travel-distance2
 92.12252786947717 .13332396910813327 10.803079666460809 21.125617939178348)
(travel-distance2
 100.08059274561998 3.1530191398337994e-2 9.34083809761667 4.136161626762059)
(travel-distance2
 101.745354448121 2.661917495674097e-3 9.173556358056414 1.087822360248587)
(travel-distance2
 102.20317110343295 1.563420642799271e-3 9.1330432750258 .3157480160909896)
(trave-distance2
 102.29450014936218 1.0601155611239673e-3 9.125173170372692 -.14828790108433176)
