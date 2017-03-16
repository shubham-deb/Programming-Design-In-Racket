;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname screensaver-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; GOAL: to build a screensaver

;; start with (screensaver 0.5)

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide screensaver
         initial-world
         world-after-tick
         world-after-key-event
         world-circ1
         world-circ2
         world-paused?
         new-circle
         circ-x
         circ-y
         circ-vx
         circ-vy)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;CONSTANTS

;; dimensions of the canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)

;; drawing image constants
(define CIRCLE-IMAGE (circle 40 "outline" "blue"))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; dimensions of the circle
(define CIRCLE1-X-COORD 200)
(define CIRCLE2-X-COORD 200)
(define CIRCLE1-Y-COORD 100)
(define CIRCLE2-Y-COORD 200)
(define HALF-CIRCLE-WIDTH  (/ (image-width  CIRCLE-IMAGE) 2))
(define HALF-CIRCLE-HEIGHT (/ (image-height CIRCLE-IMAGE) 2))
(define CIRCLE-Y-LIMIT (- CANVAS-HEIGHT (/ (image-height CIRCLE-IMAGE) 2)))
(define CIRCLE-X-LIMIT (- CANVAS-WIDTH (/ (image-width CIRCLE-IMAGE) 2)))

;; velocities of respective circles
(define CIRCLE1-X-VELOCITY -12)
(define CIRCLE1-Y-VELOCITY 20)
(define CIRCLE2-X-VELOCITY 23)
(define CIRCLE2-Y-VELOCITY -14)

;; examples KeyEvents for testing
(define pause-key-event " ")
(define non-pause-key-event "q")   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

(define-struct world (circ1 circ2 paused?))
;; A WorldState is a (make-world Circle Circle Boolean)
;; Interpretation: 
;; Circle1 and Circle2 are the two circles 
;; paused? describes whether or not the circle is paused.

;; TEMPLATE:
;; worldstate-fn : WorldState -> ??
;; (define (worldstate-fn w)
;; (... (world-circc1 w) (world-circc2 w) (world-paused? w)))

(define-struct circ (x y vx vy))
;; A Circle is a (make-circ NonNegInt NonNegInt Integer Integer)
;; Interpretation: 
;; x, y represents the position of the circle.
;; vx, vy give the velocities in x and y direction respectively

;; Template:
;; circle-fn : Circle -> ??
;; (define (circle-fn c)
;; (... (circ-x c) (circ-y c) (circ-vx c) (circ-vy c)))

;; examples of circles, for testing

(define paused-circle1-at-100-180 (make-circ 100 180 -12 20))
(define unpaused-circle1-at-100-180 (make-circ 100 180 -12 20))

(define paused-circle2-at-120-340 (make-circ 120 340 23 -14))
(define unpaused-circle2-at-120-340 (make-circ 120 340 23 -14))

(define unpaused-circle1-at-88-200 (make-circ 88 200 -12 20))
(define unpaused-circle2-at-143-336 (make-circ 143 336 23 -14))

;; examples of worlds, for testing

(define paused-world
  (make-world
   paused-circle1-at-100-180
   paused-circle2-at-120-340
   true))

(define unpaused-world
  (make-world
   unpaused-circle1-at-100-180
   unpaused-circle2-at-120-340
   false))

;; in paused world, both the circles stop moving in the canvas
(define paused-world-after-tick
  (make-world
   paused-circle1-at-100-180
   paused-circle2-at-120-340
   true))

;; in an unpaused world, both the circles move in the canvas
(define unpaused-world-after-tick
  (make-world
   unpaused-circle1-at-88-200
   unpaused-circle2-at-143-336
   false))

;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTION DEFINITIONS

;; SCREENSAVER FUNCTION

;; screensaver : PosReal -> WorldState
;; GIVEN: the speed of the simulation, in seconds/tick
;; EFFECT: runs the simulation, starting with the initial state as
;;         specified in the problem set.
;; RETURNS: the final state of the world
;; EXAMPLES :
;; (screensaver 0) will run the simulation
;; (screensaver 99) will run the simulation
;; DESIGN STRATEGY: combine simpler functions
(define (screensaver initial-speed)
  (big-bang (initial-world initial-speed)
            (on-tick world-after-tick initial-speed)
            (on-draw world-to-scene)
            (on-key world-after-key-event)))

;;TESTS:none

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set
;; DESIGN STRATEGY: combine simpler functions

(define (initial-world val)
  (make-world
   (make-circ CIRCLE1-X-COORD CIRCLE1-Y-COORD CIRCLE1-X-VELOCITY CIRCLE1-Y-VELOCITY)
   (make-circ CIRCLE2-X-COORD CIRCLE2-Y-COORD CIRCLE2-X-VELOCITY CIRCLE2-Y-VELOCITY)
   true))

;; TESTS
(begin-for-test
  (check-equal? (initial-world 23)
                (make-world (make-circ 200 100 -12 20)(make-circ 200 200 23 -14) true)))

;; world-after-tick : WorldState -> WorldState
;; RETURNS: the world state that should follow the given world state
;;          after a tick.
;; STRATEGY: Use template for WorldState on w           
(define (world-after-tick w)
  (if (world-paused? w)
      w
      (make-world
       (new-circle
       (circ-x (world-circ1 w)) (circ-y (world-circ1 w))
       (circ-vx (world-circ1 w)) (circ-vy (world-circ1 w)))
       (new-circle
       (circ-x (world-circ2 w)) (circ-y (world-circ2 w))
       (circ-vx (world-circ2 w)) (circ-vy (world-circ2 w)))
       (world-paused? w))))

;; TESTS
(begin-for-test
  (check-equal? (world-after-tick (make-world (make-circ 200 180 -12 20) (make-circ 100 120 23 -14) false))
                (make-world (make-circ 188 200 -12 20) (make-circ 123 106 23 -14) false)
                "The next tick of (200,180,-12,20) and (100,120,23,-14) should be (188,200,-12,20) and (123,106,23,-14) respectively")
  (check-equal? (world-after-tick (make-world (make-circ 200 180 -12 20) (make-circ 100 120 23 -14) true))
                (make-world (make-circ 200 180 -12 20) (make-circ 100 120 23 -14) true))
                "The next tick on pausing (200,180,-12,20) and (100,120,23,-14) should be (200,180,-12,20) and (100,120,23,-14) respectively")

;; new-circle : NonNegInt NonNegInt Int Int -> Circle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a circle centered at (x,y), which will travel with
;;          velocity (vx, vy).
;; EXAMPLES:
;; (new-circle 40 40 -12 20) -> (make-circ 40 40 12 -20)
;; (new-circle 340 240 24 36) -> (make-circ 364 276 24 36)
;; DESIGN STRATEGY : Cases on x and y coordinates of the circle
(define (new-circle x y vx vy)
  (cond
    [(and (would-circle-go-past-right? x vx) (would-circle-go-past-bottom? y vy))
     (make-circ (- CANVAS-WIDTH HALF-CIRCLE-WIDTH) (- CANVAS-HEIGHT HALF-CIRCLE-HEIGHT) (- vx) (- vy))]
    [(and (would-circle-go-past-top? y vy) (would-circle-go-past-right? x vx))
     (make-circ (- CANVAS-WIDTH HALF-CIRCLE-WIDTH) HALF-CIRCLE-HEIGHT (- vx) (- 0 vy))]
    [(and (would-circle-go-past-left? x vx) (would-circle-go-past-bottom? y vy))
     (make-circ HALF-CIRCLE-WIDTH (- CANVAS-HEIGHT HALF-CIRCLE-HEIGHT) (- vx) (- vy))]
    [(and (would-circle-go-past-left? x vx) (would-circle-go-past-top? y vy))
     (make-circ HALF-CIRCLE-WIDTH HALF-CIRCLE-HEIGHT (- vx) (- vy))]   
    [(would-circle-go-past-bottom? y vy)
     (make-circ (+ x vx) (+ y (- CANVAS-HEIGHT y HALF-CIRCLE-HEIGHT)) vx (- vy))]
    [(would-circle-go-past-right? x vx)
     (make-circ (+ x (- CANVAS-WIDTH x HALF-CIRCLE-WIDTH))  (+ y vy) (- vx) vy)]
    [(would-circle-go-past-top? y vy)
     (make-circ (+ x vx) HALF-CIRCLE-HEIGHT vx (- vy))]
    [(would-circle-go-past-left? x vx)
     (make-circ HALF-CIRCLE-WIDTH (+ y vy) (- vx) vy)]
    [else (make-circ (+ x vx) (+ y vy)  vx vy)]))

;; TESTS
(begin-for-test
  (check-equal? (new-circle 200 100 -12 20) (make-circ 188 120 -12 20)
                "(200,100) should be (-188,120) when applying velocity (-12,20)")
  (check-equal? (new-circle 45 80 -15 20) (make-circ 40 100 15 20)
                "(45,80) should be (40,100) when applying velocity (-15,20)")
  (check-equal? (new-circle 390 200 15 -20) (make-circ 360 180 -15 -20)
                "(390,200) should be (360,180) when applying velocity (15,-20)")
  (check-equal? (new-circle 300 299 15 -20) (make-circ 315 260 15 20)
                "(300,299) should be (315,260) when applying velocity (15,-20)")
  (check-equal? (new-circle 230 10 15 -20) (make-circ 245 40 15 20)
                "(230,10) should be (245,40) when applying velocity (15,-20)")
  (check-equal? (new-circle 230 10 15 -20) (make-circ 245 40 15 20)
                "(230,10) should be (245,40) when applying velocity (15,-20)")
  (check-equal? (new-circle 10 10 -15 -20) (make-circ 40 40 15 20)
                "(10,10) should be (40,40) when applying velocity (-15,-20)")
  (check-equal? (new-circle 390 10 15 -20) (make-circ 360 40 -15 20)
                "(390,10) should be (360,40) when applying velocity (15,-20)")
  (check-equal? (new-circle 10 290 -15 20) (make-circ 40 260 15 -20)
                "(10,290) should be (40,260) when applying velocity (-15,20)")
  (check-equal? (new-circle 380 260 45 40) (make-circ 360 260 -45 -40)
                "(380,260) should be (360,260) when applying velocity (45,40)")
  )

;; HELPER FUNCTION
;; would-circle-go-past-bottom?: NonNegInt Integer -> Boolean
;; GIVEN: y-coordinate and the velocity vy
;; RETURNS: true iff the circle goes below the canvas,
;;          else returns false
;; EXAMPLES:
;; (would-circle-go-past-bottom? 20 -20) -> true
;; (would-circle-go-past-bottom? 100 -15) -> false
;; DESIGN STRATEGY: combine simpler functions
(define (would-circle-go-past-bottom? y vy)
  (>=
   (+ y vy (/ (image-height CIRCLE-IMAGE) 2))
   CANVAS-HEIGHT))

;; TESTS
(begin-for-test
  (check-equal? (would-circle-go-past-bottom? 299 20) #true
                "y-coordinate 299 should go past bottom when applying velocity 20"))

;; HELPER FUNCTION
;; would-circle-go-past-right?: NonNegInt Integer -> Boolean
;; GIVEN: x-coordinate and the velocity vx
;; RETURNS: true iff the circle goes beyond the right boundary of the canvas
;;          else returns false
;; EXAMPLES:
;; (would-circle-go-past-right? 390 20) -> true
;; (would-circle-go-past-right? 300 -15) -> false
;; DESIGN STRATEGY: combine simpler functions
(define (would-circle-go-past-right? x vx)
  (>=
   (+ x vx (/ (image-width CIRCLE-IMAGE) 2))
   CANVAS-WIDTH))

;; TESTS
(begin-for-test
  (check-equal? (would-circle-go-past-right? 370 40) #true
                "x-coordinate 370 should go past right boundary when applying velocity 40"))

;; HELPER FUNCTION
;; would-circle-go-past-top?: NonNegInt Integer -> Boolean
;; GIVEN: y-coordinate and the velocity vy
;; RETURNS: true iff the circle goes above the top boundary of the canvas,
;;          else returns false
;; EXAMPLES:
;; (would-circle-go-past-top? 44 -20) -> true
;; (would-circle-go-past-top? 121 -15) -> false
;; DESIGN STRATEGY: combine simpler functions
(define (would-circle-go-past-top? y vy)
  (<=
   (+ (- y (/ (image-height CIRCLE-IMAGE) 2)) vy)
   0))

;; TESTS
(begin-for-test
  (check-equal? (would-circle-go-past-top? 20 -50) #true
                "y-coordinate 20 should go past top boundary when applying velocity -50"))

;; HELPER FUNCTION
;; would-circle-go-past-left?: NonNegInt Integer -> Boolean
;; GIVEN: x-coordinate and the velocity vx
;; RETURNS: true iff the circle goes beyond the left boundary of the canvas,
;;          else returns false
;; EXAMPLES:
;; (would-circle-go-past-bottom? 80 -90) -> true
;; (would-circle-go-past-bottom? 100 -15) -> false
;; DESIGN STRATEGY: combine simpler functions
(define (would-circle-go-past-left? x vx)
  (<=
   (+ (- x (/ (image-width CIRCLE-IMAGE) 2)) vx)
   0))

;; TESTS
(begin-for-test
  (check-equal? (would-circle-go-past-left? 10 -40) #true
                "x-coordinate 10 should go past left boundary when applying velocity -40"))

;; world-to-scene : WorldState -> Scene
;; RETURNS: given a worldstate, it returns a scene that portrays the given world.
;; EXAMPLE:
;; (world-to-scene paused-world) should return a canvas with
;; two circles, one at (100,180) and one at (120,340)
;; DESIGN STRATEGY: Use template for WorldState on w
(define (world-to-scene w)
  (place-circle (world-circ1 w) (place-circle (world-circ2 w) EMPTY-CANVAS)))

;;TESTS
(begin-for-test
  (check-equal? (world-to-scene
                 (make-world (make-circ 100 200 -12 20) (make-circ 200 200 23 -14) false))
                (place-circle (make-circ 100 200 -12 20) (place-circle (make-circ 200 200 23 -14)
                                                                       EMPTY-CANVAS)))
  )

;; HELPER FUNCTION
;; place-circle : Circle Scene -> Scene
;; GIVEN: a circle and a scene
;; RETURNS: a scene like the given one, but with the given circle
;;          painted blue on the outline and its coordinates painted on it.
;; EXAMPLE:
;; (place-circle (make-circ 200 100 -12 20) EMPTY-CANVAS) will
;; return the given circle with (-12,20) appended at the centre
;; DESIGN STRATEGY: Use template of Circle on c
(define (place-circle c img)
  (place-image
   CIRCLE-IMAGE
   (circ-x c) (circ-y c) 
   (place-image
    (text (string-append (number->string (circ-vx c)) "," (number->string (circ-vy c))) 14 "blue")
    (circ-x c) (circ-y c) img))
  )

;; world-after-key-event : WorldState KeyEvent -> WorldState
;; RETURNS: the WorldState that should follow the given worldstate
;;          after the given keyevent.
;; on space, toggle paused?-- ignore all others
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: cases on KeyEvent kev
(define (world-after-key-event w kev)
  (cond
    [(key=? kev " ")
     (world-with-paused-toggled w)]
    [else w]))

;; HELPER FUNCTION
;; world-with-paused-toggled : WorldState -> WorldState
;; RETURNS: a world just like the given one, but with paused? toggled
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Use template for WorldState on w
(define (world-with-paused-toggled w)
  (make-world
   (world-circ1 w)
   (world-circ2 w)
   (not (world-paused? w))))

;; for world-after-key-event, we need 4 tests: a paused world, and an
;; unpaused world, and a pause-key-event and a non-pause key event.

(begin-for-test
  (check-equal?
   (world-after-key-event paused-world pause-key-event)
   unpaused-world
   "after pause key, a paused world should become unpaused")
  
  (check-equal?
   (world-after-key-event unpaused-world pause-key-event)
   paused-world
   "after pause key, an unpaused world should become paused")
  
  (check-equal?
   (world-after-key-event paused-world non-pause-key-event)
   paused-world
   "after a non-pause key, a paused world should be unchanged")
  
  (check-equal?
   (world-after-key-event unpaused-world non-pause-key-event)
   unpaused-world
   "after a non-pause key, an unpaused world should be unchanged"))