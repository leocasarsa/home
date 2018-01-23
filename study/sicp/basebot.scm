;;; Project 1, 6.001, Spring 2005

;;; idea is to simulate a baseball robot

;; imagine hitting a ball with an initial velocity of v
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

(define square
  (lambda (x) (* x x)))

;; these are constants that will be useful to us
(define gravity 9.8)  ;; in m/s
(define pi (acos -1))

;; Problem 1
;; Write a procedure that takes as input values for a , v , u , and t
;; and returns as output the position of the baseball at time t .

(define position
  (lambda (a v u t)
    (+ (*
        (/ 1 2)
        a
        (square t))
       (* v t)
       u)))

;; Test cases

(position 0 0 0 0)    ; -> 0
(position 0 0 20 0)   ; -> 20
(position 0 5 10 10)  ; -> 60
(position 2 2 2 2)    ; -> 10
(position 5 5 5 5)    ; -> 185/2

;; TODO : add typical test cases and boundary conditions

;; Problem 2
;; Given the coefficients of the quadratic equation az^2 + bz + c = 0,
;; write a procedure to find one of the roots (call this root1 ), and
;; another procedure to find the other root.

(define radicand
  (lambda (a b c) (- (square b) (* 4 a c))))

(define roots
  (lambda (a b c plus-or-minus)
    (let ((radicand (- (square b)
                       (* 4 a c))))
      (if (and (>= radicand 0) (not (eq? a 0)))
          (/
           (plus-or-minus (- b)
                          (sqrt radicand))
           (* 2 a))
          #f))))

(define root1
  (lambda (a b c)
    (roots a b c +)))

(define root2
  (lambda (a b c)
    (roots a b c -)))

;; test cases
(root1 0 0 0)  ; -> #f
(root1 0 0 1)  ; -> #f
(root1 0 1 1)  ; -> #f
(root1 1 1 1)  ; -> #f
(root1 1 -2 1) ; -> 1
(root1 1 0 -4) ; -> 2

(root2 0 0 0)  ; -> #f
(root2 0 0 1)  ; -> #f
(root2 0 1 1)  ; -> #f
(root2 1 1 1)  ; -> #f
(root2 1 -2 1) ; -> 1
(root2 1 0 -4) ; -> -2

;; Problem 3
;; Given an initial upward velocity (in meters per second, or m/s) and
;; initial elevation or height (in meters, or m), write a procedure that
;; computes how long the baseball will be in flight. Remember that
;; gravity is a downward acceleration of 9.8m/s 2 . Note that to solve
;; this you will need a root of a quadratic equation. Try using root1,
;; and using root2.  Only one of these solutions makes sense. Which one?
;; And why? Use this to create a correct version of the procedure below.

(define time-to-impact
  (lambda (vertical-velocity elevation)
    (let ((v0 vertical-velocity)
          (h elevation)
          (a gravity))
      (root2 (- (/ a 2))
             v0
             h))))

;; For any initial upward velocity and downward acceleration (gravity)
;; there will be two times in which the ball hit the floor, one lying in
;; the past or present (negative or null time), and one in the future
;; (positive time).  Thus, the positive root is the correct version. With
;; negative acceleration, the positive root is root2.

;; test cases
(time-to-impact 0 0)  ; -> 0
(time-to-impact gravity 0)  ; -> 2
(time-to-impact 0 (* 2 gravity))  ; -> 2
(time-to-impact (* (sqrt 2) gravity) gravity)  ; -> (+ 2 (sqrt 2))
(time-to-impact (* (sqrt 2) gravity) (- gravity))  ; -> (sqrt 2)

;; Note that if we want to know when the ball drops to a particular
;; height r (for receiver), we have

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (time-to-impact vertical-velocity
                    (- elevation target-elevation))))

;; test cases
(time-to-height 0 0 0)  ; -> 0
(time-to-height gravity 0 0)  ; -> 2
(time-to-height gravity 1 1)  ; -> 1
(time-to-height 0 (* 2 gravity) 0)  ; -> 2
(time-to-height 0 gravity (- gravity))  ; -> 2
(time-to-height (* (sqrt 2) gravity) gravity 0)  ; -> (+ 2 (sqrt 2))
(time-to-height (* (sqrt 2) gravity) (- gravity) 0)  ; -> (sqrt 2)

;; Problem 4
;; Suppose the baseball is hit with some velocity v, at a starting angle
;; alpha relative to the horizontal (in degrees), and from an initial
;; elevation (in meters). We wish to compute the distance in the
;; horizontal direction the baseball will travel by the time it lands.
;; once we can solve for t_impact, we can use it to figure out how far
;; the ball went.

;; conversion procedure
(define degree2radian
  (lambda (deg)
    (/ (*  deg pi) 180.)))

(degree2radian 0) ; -> 0
(degree2radian 90) ; -> ~1.57
(degree2radian 270) ; -> ~4.71

(define vertical-velocity
  (lambda (velocity angle)
    (* velocity (sin (degree2radian angle)))))

(vertical-velocity 1 0) ; -> 0
(vertical-velocity 1 90) ; -> 1
(vertical-velocity 1 45) ; -> (/ (sqrt 2) 2)

(define horizontal-velocity
  (lambda (velocity angle)
    (* velocity (cos (degree2radian angle)))))

(horizontal-velocity 1 0) ; -> 1
(horizontal-velocity 1 90) ; -> 0
(horizontal-velocity 1 45) ; -> (/ (sqrt 2) 2)

(define travel-distance-simple
  (lambda (elevation velocity angle)
    (let ((flight-time (time-to-impact
                        (vertical-velocity velocity angle)
                        elevation)))
      (position 0
                (horizontal-velocity velocity angle)
                0
                flight-time))))

;; let's try this out for some example values.  Note that we are going to
;; do everything in metric units, but for quaint reasons it is easier to
;; think about things in English units, so we will need some conversions.

(travel-distance-simple 1 0 0) ; -> 0
(travel-distance-simple 1 1 90) ; -> 0
(travel-distance-simple 1 1 0) ; -> .45

(define meters-to-feet
  (lambda (m)
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    (/ s 3600)))

;; what is time to impact for a ball hit at a height of 1 meter
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; at an angle of 0 (straight horizontal)
;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)
;; at an angle of (/ pi 4) radians or 45 degrees

(time-to-impact (vertical-velocity 45 0) 1) ; -> .45
(time-to-impact (vertical-velocity 45 90) 1) ; -> 9.2
(time-to-impact (vertical-velocity 45 45) 1) ; -> 6.5

;; what is the distance traveled in each case?
;; record both in meters and in feet

(travel-distance-simple 1 45 0)  ; -> 20 meters
(travel-distance-simple 1 45 90) ; -> ~0 meters
(travel-distance-simple 1 45 45) ; -> 207.6 meters

(meters-to-feet (travel-distance-simple 1 45 0))  ; -> 207.6 feet
(meters-to-feet (travel-distance-simple 1 45 90)) ; -> ~0
 feet
(meters-to-feet (travel-distance-simple 1 45 45)) ; -> ~685.2 feet

;; Problem 5

;; these sound pretty impressive, but we need to look at it more
;; carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance assume that angle is between 0 and (/ pi 2) radians or
;; between 0 and 90 degrees

;; Write a procedure find-best-angle that takes as arguments an initial
;; elevation and an initial velocity, and which finds the best angle at
;; which to hit the baseball to optimize distance traveled.

(define angle-increment 1)
(define increment-angle
  (lambda (angle) (+ angle angle-increment)))

(define find-distances-per-angle
  (lambda (velocity elevation)
    (let* ((find-distances-per-angle-helper
            (lambda (angle angle-list)
              (display angle-list) (newline)
              (if (> angle 90)
                  angle-list
                  (find-distances-per-angle-helper
                   increment-angle
                   (append (angle-list
                            (cons angle
                                  (travel-distance-simple
                                   elevation velocity angle)))))))))
      (find-distances-per-angle-helper 0 '()))))


(define find-best-angle-helper
  (lambda (velocity elevation angle)
    (let ((my-distance
           (lambda (angle)
             (travel-distance-simple elevation velocity angle))))
           (if (> (my-distance angle)
                  (my-distance (+ angle angle-increment)))
               angle
               (find-best-angle-helper
                velocity
                elevation
                (+ angle angle-increment))))))

(define find-best-angle
  (lambda (velocity elevation)
    (find-best-angle-helper velocity elevation 0)))

;; find best angle
(find-best-angle 45 1)  ; -> 44.86

;; try for other velocities
(find-best-angle 90 1)  ; -> 44.97
(find-best-angle 150 1) ; -> 44.99


























(find-best-angle 1 1) ; -> 12.43

;; try for other heights
(find-best-angle 90 0)  ; -> 45
(find-best-angle 90 10)  ; -> 44.65
(find-best-angle 90 100)  ; -> 41.90
(find-best-angle 150 100) ; -> 42.8
(find-best-angle 1 100) ; -> 1.29

;; Problem 6

;; problem is that we are not accounting for drag on the ball (or on spin
;; or other effects, but let's just stick with drag)
;;
;; Newton's equations basically say that ma = F, and here F is really two
;; forces.  One is the effect of gravity, which is captured by mg.  The
;; second is due to drag, so we really have
;;
;; a = drag/m + gravity
;;
;; drag is captured by 1/2 C rho A vel^2, where
;; C is the drag coefficient (which is about 0.5 for baseball sized spheres)
;; rho is the density of air (which is about 1.25 kg/m^3 at sea level
;; with moderate humidity, but is about 1.06 in Denver)
;; A is the surface area of the cross section of object, which is pi D^2/4
;; where D is the diameter of the ball (which is about 0.074m for a baseball)
;; thus drag varies by the square of the velocity, with a scaling factor
;; that can be computed

;; We would like to again compute distance , but taking into account
;; drag.
;; Basically we can rework the equations to get four coupled linear
;; differential equations
;; let u be the x component of velocity, and v be the y component of velocity
;; let x and y denote the two components of position (we are ignoring the
;; third dimension and are assuming no spin so that a ball travels in a plane)
;; the equations are
;;
;; dx/dt = u
;; dy/dt = v
;; du/dt = -(drag_x/m + g_x)
;; dv/dt = -(drag_y/m + g_y)
;; we have g_x = - and g_y = - gravity
;; to get the components of the drag force, we need some trig.
;; let speed = (u^2+v^2)^(1/2), then
;; drag_x = - drag * u /speed
;; drag_y = - drag * v /speed
;; where drag = beta speed^2
;; and beta = 1/2 C rho pi D^2/4
;; note that we are taking direction into account here

;; we need the mass of a baseball -- which is about .15 kg.

;; so now we just need to write a procedure that performs a simple integration
;; of these equations -- there are more sophisticated methods but a simple one
;; is just to step along by some step size in t and add up the values

;; dx = u dt
;; dy = v dt
;; du = - 1/m speed beta u dt
;; dv = - (1/m speed beta v + g) dt

;; initial conditions
;; u_0 = V cos alpha
;; v_0 = V sin alpha
;; y_0 = h
;; x_0 = 0

;; we want to start with these initial conditions, then take a step of size dt
;; (which could be say 0.1) and compute new values for each of these parameters
;; when y reaches the desired point (<= 0) we stop, and return the distance (x)

(define drag-coeff 0.5)
(define density 1.25)  ; kg/m^3
(define mass .145)  ; kg
(define diameter 0.074)  ; m
(define beta (* .5 drag-coeff density (* 3.14159 .25 (square diameter))))

(define make-speed
  (lambda (u v)
    (sqrt (+ (square u 2)
             (square v 2)))))

(define dx
  (lambda (u dt)
    (* u dt)))

(dx 1 .01) ; -> .01

(define integrate-x
  (lambda (x u dt)
    (+ x
       (dx u dt))))

(integrate-x 1 1 .01) ; -> 1.01

(define dy
  (lambda (v dt)
    (* v dt)))

(dy 2 .01) ; -> .02

(define integrate-y
  (lambda (y v dt)
    (+ y
       (dy v dt))))

(integrate-y 1 2 .01) ; -> .02

(define du
  (lambda (u v dt g m beta)
    (* -1
       (* (/ 1 m)
          (make-speed u v)
          beta
          u)
       dt)))

(du 0 1 .01 gravity mass beta) ; -> 0

(define integrate-u
  (lambda (u v dt g m beta)
    (+ u
       (du u v dt g m beta))))

(integrate-u 0 1 .01 gravity mass beta) ; -> 0

(define dv
  (lambda (u v dt g m beta)
    (* -1
       (+ (* (/ 1 m)
             (make-speed u v)
             beta
             v)
           g)
       dt)))

(dv 0 1 .01 mass gravity beta) ; -> 0.1 g
(dv 0 1 0 mass gravity beta) ; -> 0

(define integrate-v
  (lambda (u v dt g m beta)
    (+ v
       (dv u v dt g m beta))))

(integrate-v 0 1 .01 mass gravity beta) ; -> 0.1 g

(define integrate-helper
  (lambda (x y u v dt g m beta)
    (let ((x-new (integrate-x x u dt))
          (y-new (integrate-y y v dt))
          (u-new (integrate-u u v dt g m beta))
          (v-new (integrate-v u v dt g m beta)))
    (if (<= y-new 0)
        x
        (integrate-helper x-new y-new u-new v-new dt g m beta)))))

(define integrate
  (lambda (x0 y0 u0 v0 dt g m beta)
    (integrate-helper x0 y0 u0 v0 dt g m beta)))

(define travel-distance
  (lambda (elevation velocity angle)
    (let ((x0 0)
          (y0 elevation)
          (u0 (horizontal-velocity velocity angle))
          (v0 (vertical-velocity velocity angle))
          (dt 0.01))
      (integrate x0 y0 u0 v0 dt gravity mass beta))))

(travel-distance 1 45 0)  ; -> 19.34 meters
(travel-distance 1 45 90) ; -> ~ 0 meters
(travel-distance 1 45 45) ; -> 92.23 meters
(travel-distance 0 45 45) ; -> 91.68 meters
(travel-distance 0 45 0)
;; what about Denver?

(define density 1.06)  ; kg/m^3
(define beta (* .5 drag-coeff density (* 3.14159 .25 (square diameter))))

(travel-distance 1 45 0)  ; -> 19.59 meters
(travel-distance 1 45 90) ; -> ~ 0 meters
(travel-distance 1 45 45) ; -> 99.82 meters

;; Back to Boston

(define density 1.25)  ; kg/m^3
(define beta (* .5 drag-coeff density (* 3.14159 .25 (square diameter))))


;; Problem 7

;; now let's turn this around.  Suppose we want to throw the ball.  The same
;; equations basically hold, except now we would like to know what angle to
;; use, given a velocity, in order to reach a given height (receiver) at a
;; given distance

;; a cather trying to throw someone out at second has to get it roughly 36 m
;; (or 120 ft) how quickly does the ball get there, if he throws at 55m/s,
;;  at 45m/s, at 35m/s?

;; try out some times for distances (30, 60, 90 m) or (100, 200, 300 ft)
;; using 45m/s

;; Problem 8

;; Problem 9
