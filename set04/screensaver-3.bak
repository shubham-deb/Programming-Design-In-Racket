;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname screensaver-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
         circ-vy
         circ-selected?
         world-after-mouse-event
         circ-after-mouse-event)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;CONSTANTS

;; dimensions of the canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)

;; drawing image constants
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define BLUE-CIRCLE-IMAGE (circle 40 "outline" "blue"))
(define RED-CIRCLE-IMAGE (circle 40 "outline" "red"))
(define RED-POINTER (circle 5 "solid" "red"))

;; dimensions of the circle
(define CIRCLE1-X-COORD 200)
(define CIRCLE2-X-COORD 200)
(define CIRCLE1-Y-COORD 100)
(define CIRCLE2-Y-COORD 200)
(define HALF-CIRCLE-WIDTH  (/ (image-width  BLUE-CIRCLE-IMAGE) 2))
(define HALF-CIRCLE-HEIGHT (/ (image-height BLUE-CIRCLE-IMAGE) 2))
(define CIRCLE-Y-LIMIT (- CANVAS-HEIGHT (/ (image-height BLUE-CIRCLE-IMAGE) 2)))
(define CIRCLE-X-LIMIT (- CANVAS-WIDTH (/ (image-width BLUE-CIRCLE-IMAGE) 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS
(define-struct world (circ1 circ2 paused?))
;; A WorldState is a (make-world Circle Circle Boolean)
;; Interpretation: 
;; cir1 and cir2 are the two circles 
;; paused? describes whether or not the circle is paused.

;; TEMPLATE:
;; worldstate-fn : WorldState -> ??
;; (define (worldstate-fn w)
;; (... (world-cir1 w) (world-cir2 w) (world-paused? w)))

(define-struct circ (x y vx vy selected? o-x o-y clicked?))
;; A Circle is a
;;(make-circ NonNegInt NonNegInt Integer Integer Boolean Integer Integer Boolean)
;; Interpretation: 
;; x, y represents the x and y coordinates of the circle.
;; vx, vy give the velocities in x and y direction respectively
;; o-x and o-y represent the offsets in x and y direction when the circle is dragged
;; clicked? represents if the mouse is clicked or not

;; Template:
;; circle-fn : Circle -> ??
;; (define (circle-fn c)
;; (... (circ-x c) (circ-y c) (circ-vx c) (circ-vy c)(circ-o-x c)(circ-o-y c)(circ-clicked? c)))

;; examples of circles, for testing

(define paused-circle1-at-100-180 (make-circ 100 180 -12 20 false 0 0 false))
(define unpaused-circle1-at-100-180 (make-circ 100 180 -12 20 false 0 0 false))

(define paused-circle2-at-120-340 (make-circ 120 340 23 -14 false 0 0 false))
(define unpaused-circle2-at-120-340 (make-circ 120 340 23 -14 false 0 0 false))

(define unpaused-circle1-at-88-200 (make-circ 88 200 -12 20 false 0 0 false))
(define unpaused-circle2-at-143-336 (make-circ 143 336 23 -14 false 0 0 false))

(define selected-circle1-at-100-180 (make-circ 100 180 -12 20 true 0 0 false))
(define unselected-circle1-at-100-180 (make-circ 100 180 -12 20 false 0 0 false))

(define selected-circle2-at-120-340 (make-circ 120 340 23 -14 true 0 0 false))
(define unselected-circle2-at-120-340 (make-circ 120 340 23 -14 false 0 0 false))
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

;; examples KeyEvents for testing
(define pause-key-event " ")
(define non-pause-key-event "q")   


;; example MouseEvents for testing:
(define button-down-event "button-down")
(define drag-event "drag")
(define button-up-event "button-up")
(define other-event "enter")

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
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;TESTS: none

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set
;; DESIGN STRATEGY: combine simpler functions

(define (initial-world y)
  (make-world
    (make-circ CIRCLE1-X-COORD CIRCLE1-Y-COORD  -12 20 false 0 0 false)
    (make-circ CIRCLE2-X-COORD CIRCLE2-Y-COORD 23 -14 false 0 0 false)
    true
    ))

;; TESTS
(begin-for-test
  (check-equal? (initial-world 23)
                (make-world (make-circ 200 100 -12 20 false 0 0 false)
                            (make-circ 200 200 23 -14 false 0 0 false) true)
                "Initial world should be initialized specified in problem set
                  but it is not!"))

;; world-after-tick : WorldState -> WorldState
;; RETURNS: the world state that should follow the given world state
;;          after a tick.
;; STRATEGY: Use template for WorldState on w

(define (world-after-tick w)
  (if (world-paused? w)
      w
      (make-world
       (new-circle (circ-x (world-circ1 w)) (circ-y (world-circ1 w))
       (circ-vx (world-circ1 w)) (circ-vy (world-circ1 w))
       (circ-selected? (world-circ1 w)) (circ-o-x (world-circ1 w))(circ-o-y (world-circ1 w)) (circ-clicked? (world-circ1 w)))
       
       (new-circle (circ-x (world-circ2 w)) (circ-y (world-circ2 w))
       (circ-vx (world-circ2 w)) (circ-vy (world-circ2 w))
       (circ-selected? (world-circ2 w)) (circ-o-x (world-circ2 w))(circ-o-y (world-circ2 w)) (circ-clicked? (world-circ2 w)))
       
       (world-paused? w))))

;; TESTS
(begin-for-test
  (check-equal?
   (world-after-tick
   (make-world (make-circ 200 180 -12 20 false 0 0 false)
                                 (make-circ 100 120 23 -14 false 0 0 false) false ))

   (make-world (make-circ 188 200 -12 20 false 0 0 false)
               (make-circ 123 106 23 -14 false 0 0 false) false)
   
   "The next tick of (200,180,-12,20) and (100,120,23,-14) should be
    (188,200,-12,20) and (123,106,23,-14) respectively")
  
  (check-equal? (world-after-tick
  (make-world (make-circ 200 180 -12 20 false 0 0 false)
              (make-circ 100 120 23 -14 false 0 0 false) true))
                
   (make-world (make-circ 200 180 -12 20 false 0 0 false)
   (make-circ 100 120 23 -14 false 0 0 false) true))
  
    "The next tick on pausing (200,180,-12,20) and (100,120,23,-14)
    should be (200,180,-12,20) and (100,120,23,-14) respectively")

;; new-circle : NonNegInt NonNegInt Int Int -> Circle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: an unselected circle centered at (x,y), which will travel with
;;          velocity (vx, vy).
;; EXAMPLES:
;; (new-circle 40 40 -12 20 0 0 false) -> (make-circ 40 40 12 -20)
;; (new-circle 340 240 24 36) -> (make-circ 364 276 24 36)
;; DESIGN STRATEGY : Cases on x and y coordinates of the given circle
(define (new-circle x y vx vy s o-x o-y c)
  (if s
    (make-circ x y vx vy s o-x o-y c)
    (cond
    [(and (would-circ-go-past-right? x vx) (would-circ-go-past-bottom? y vy))
     (make-circ (- CANVAS-WIDTH HALF-CIRCLE-WIDTH) (- CANVAS-HEIGHT HALF-CIRCLE-HEIGHT) (- vx) (- vy) s o-x o-y c)]
    [(and (would-circ-go-past-top? y vy) (would-circ-go-past-right? x vx))
     (make-circ (- CANVAS-WIDTH HALF-CIRCLE-WIDTH) HALF-CIRCLE-HEIGHT (- vx) (- 0 vy) s o-x o-y c)]
    [(and (would-circ-go-past-left? x vx) (would-circ-go-past-bottom? y vy))
     (make-circ HALF-CIRCLE-WIDTH (- CANVAS-HEIGHT HALF-CIRCLE-HEIGHT) (- vx) (- vy) s o-x o-y c)]
    [(and (would-circ-go-past-left? x vx) (would-circ-go-past-top? y vy))
     (make-circ HALF-CIRCLE-WIDTH HALF-CIRCLE-HEIGHT (- vx) (- vy) s o-x o-y c)]   
    [(would-circ-go-past-bottom? y vy)
     (make-circ (+ x vx) (+ y (- CANVAS-HEIGHT y HALF-CIRCLE-HEIGHT)) vx (- vy)  s o-x o-y c)]
    [(would-circ-go-past-right? x vx)
     (make-circ (+ x (- CANVAS-WIDTH x HALF-CIRCLE-WIDTH))  (+ y vy) (- vx) vy  s o-x o-y c)]
    [(would-circ-go-past-top? y vy)
     (make-circ (+ x vx) HALF-CIRCLE-HEIGHT vx (- vy) s o-x o-y c)]
    [(would-circ-go-past-left? x vx)
     (make-circ HALF-CIRCLE-WIDTH (+ y vy) (- vx) vy s o-x o-y c)]
           [else (make-circ (+ x vx) (+ y vy) vx vy false 0 0 false)])))

(begin-for-test
  (check-equal? (new-circle 200 100 -12 20 false 0 0 false) (make-circ 188 120 -12 20 false 0 0 false)
                "(200,100) should be (-188,120) when applying velocity (-12,20)")
  (check-equal? (new-circle 45 80 -15 20 false 0 0 false) (make-circ 40 100 15 20 false 0 0 false)
                "(45,80) should be (40,100) when applying velocity (-15,20)")
  (check-equal? (new-circle 390 200 15 -20 false 0 0 false) (make-circ 360 180 -15 -20 false 0 0 false)
                "(390,200) should be (360,180) when applying velocity (15,-20)")
  (check-equal? (new-circle 300 299 15 -20 false 0 0 false) (make-circ 315 260 15 20 false 0 0 false)
                "(300,299) should be (315,260) when applying velocity (15,-20)")
  (check-equal? (new-circle 230 10 15 -20 false 0 0 false) (make-circ 245 40 15 20 false 0 0 false)
                "(230,10) should be (245,40) when applying velocity (15,-20)")
  (check-equal? (new-circle 230 10 15 -20 false 0 0 false) (make-circ 245 40 15 20 false 0 0 false)
                "(230,10) should be (245,40) when applying velocity (15,-20)")
  (check-equal? (new-circle 10 10 -15 -20 false 0 0 false) (make-circ 40 40 15 20 false 0 0 false)
                "(10,10) should be (40,40) when applying velocity (-15,-20)")
  (check-equal? (new-circle 390 10 15 -20 false 0 0 false) (make-circ 360 40 -15 20 false 0 0 false)
                "(390,10) should be (360,40) when applying velocity (15,-20)")
  (check-equal? (new-circle 10 290 -15 20 false 0 0 false) (make-circ 40 260 15 -20 false 0 0 false)
                "(10,290) should be (40,260) when applying velocity (-15,20)")
  (check-equal? (new-circle 10 290 -15 20 true 0 0 false) (make-circ 10 290 -15 20 true 0 0 false)
                "(10,290) should be (40,260) when applying velocity (-15,20)")
  (check-equal? (new-circle 380 260 45 40 false 0 0 false) (make-circ 360 260 -45 -40 false 0 0 false)
                "(380,260) should be (360,260) when applying velocity (45,40)"))

;; HELPER FUNCTION
;; would-circ-go-past-bottom?: NonNegInt Integer -> Boolean
;; GIVEN: y-coordinate and the velocity vy
;; RETURNS: true iff the circle goes below the canvas,
;;          else returns false
;; EXAMPLES:
;; (would-circ-go-past-bottom? 20 -20) -> true
;; (would-circ-go-past-bottom? 100 -15) -> false
;; DESIGN STRATEGY: combine simpler functions
(define (would-circ-go-past-bottom? y vy)
  (>=
   (+ y vy (/ (image-height BLUE-CIRCLE-IMAGE) 2))
   CANVAS-HEIGHT))

;; TESTS
(begin-for-test
  (check-equal? (would-circ-go-past-bottom? 299 20) #true
                "y-coordinate 299 should go past bottom when applying velocity 20
                 in y direction"))

;; HELPER FUNCTION
;; would-circ-go-past-right?: NonNegInt Integer -> Boolean
;; GIVEN: x-coordinate and the velocity vx
;; RETURNS: true iff the circle goes beyond the right boundary of the canvas
;;          else returns false
;; EXAMPLES:
;; (would-circ-go-past-right? 390 20) -> true
;; (would-circ-go-past-right? 300 -15) -> false
;; DESIGN STRATEGY: combine simpler functions
(define (would-circ-go-past-right? x vx)
  (>=
   (+ x vx (/ (image-width BLUE-CIRCLE-IMAGE) 2))
   CANVAS-WIDTH))

;; TESTS
(begin-for-test
  (check-equal? (would-circ-go-past-right? 370 40) #true
                "x-coordinate 370 should go past right boundary when applying velocity 40
                in x direction"))

;; HELPER FUNCTION
;; would-circ-go-past-top?: NonNegInt Integer -> Boolean
;; GIVEN: y-coordinate and the velocity vy
;; RETURNS: true iff the circle goes above the top boundary of the canvas,
;;          else returns false
;; EXAMPLES:
;; (would-circ-go-past-top? 44 -20) -> true
;; (would-circ-go-past-top? 121 -15) -> false
;; DESIGN STRATEGY: combine simpler functions
(define (would-circ-go-past-top? y vy)
  (<=
   (+ (- y (/ (image-height BLUE-CIRCLE-IMAGE) 2)) vy)
   0))

;; TESTS
(begin-for-test
  (check-equal? (would-circ-go-past-top? 20 -50) #true
                "y-coordinate 20 should go past top boundary when applying velocity -50 in
                 y direction"))

;; HELPER FUNCTION
;; would-circ-go-past-left?: NonNegInt Integer -> Boolean
;; GIVEN: x-coordinate and the velocity vx
;; RETURNS: true iff the circle goes beyond the left boundary of the canvas,
;;          else returns false
;; EXAMPLES:
;; (would-circ-go-past-left? 80 -90) -> true
;; (would-circ-go-past-left? 100 -15) -> false
;; DESIGN STRATEGY: combine simpler functions
(define (would-circ-go-past-left? x vx)
  (<=
   (+ (- x (/ (image-width BLUE-CIRCLE-IMAGE) 2)) vx)
   0))

;; TESTS
(begin-for-test
  (check-equal? (would-circ-go-past-left? 10 -40) #true
                "x-coordinate 10 should go past left boundary when applying velocity -40"))

;; world-to-scene : WorldState -> Scene
;; RETURNS: given a worldstate, it returns a scene that portrays the given world.
;; EXAMPLE:
;; (world-to-scene paused-world) should return a canvas with
;; two circcles, one at (100,180) and one at (120,340)
;; DESIGN STRATEGY: Use template for WorldState on w
(define (world-to-scene w)
  (cond
   [(and (circ-selected? (world-circ2 w)) (circ-selected? (world-circ1 w)))
     (place-redcirc (world-circ2 w) (place-redcirc (world-circ1 w) EMPTY-CANVAS))]
   [(circ-selected? (world-circ1 w))
   (place-redcirc (world-circ1 w) (place-bluecirc (world-circ2 w) EMPTY-CANVAS))] 
   [(circ-selected? (world-circ2 w))
     (place-redcirc (world-circ2 w) (place-bluecirc (world-circ1 w) EMPTY-CANVAS))]
   [(or (circ-clicked? (world-circ1 w)) (circ-clicked? (world-circ2 w)))
   (place-image RED-POINTER (circ-o-x (world-circ1 w)) (circ-o-y (world-circ1 w))
   (place-bluecirc (world-circ1 w) (place-bluecirc (world-circ2 w) EMPTY-CANVAS)))]
   [else (place-bluecirc (world-circ1 w) (place-bluecirc (world-circ2 w) EMPTY-CANVAS))]))

(begin-for-test
  
  (check-equal?
   (world-to-scene
    (make-world (make-circ 100 200 -12 20 true 0 0 false)
                (make-circ 200 200 23 -14 false 0 0 false) false))
                (place-redcirc (make-circ 100 200 -12 20 true 0 0 false)
                (place-bluecirc (make-circ 200 200 23 -14 false 0 0 false)
                                EMPTY-CANVAS))
                "Selecting the circle with centre as (100,200) should make
                 the outline of the circle as red but it is not!")
  
    (check-equal?
     (world-to-scene
      (make-world (make-circ 100 200 -12 20 false 0 0 false)
                  (make-circ 200 200 23 -14 true 0 0 false) false))
                (place-redcirc (make-circ 200 200 23 -14 true 0 0 false)
                (place-bluecirc (make-circ 100 200 -12 20 false 0 0 false)
                                EMPTY-CANVAS))
                 "Selecting the circle with centre as (200,200) should make
                 the outline of the circle as red but it is not!")
    
   (check-equal?
    (world-to-scene
     (make-world (make-circ 100 200 -12 20 true 0 0 false)
                 (make-circ 200 200 23 -14 true 0 0 false) false))
                (place-redcirc (make-circ 200 200 23 -14 true 0 0 false)
                (place-redcirc (make-circ 100 200 -12 20 true 0 0 false)
                               EMPTY-CANVAS))
                 "Selecting both the circles should make the outline of
                  both the circles as red but it is not!")
   
   (check-equal?
    (world-to-scene
     (make-world (make-circ 100 200 -12 20 false 20 30 true)
                 (make-circ 200 200 23 -14 false 0 0 false) false))
                 (place-image RED-POINTER 20 30
                 (place-bluecirc (make-circ 200 200 23 -14 false 20 30 true)
                 (place-bluecirc (make-circ 100 200 -12 20 true 0 0 false)
                                 EMPTY-CANVAS)))
                  "Selecting the circle with centre as (200,200) should make
                 the outline of the circle as red but it is not!")
   
   (check-equal?
    (world-to-scene
     (make-world (make-circ 100 200 23 -14 false 0 0 false)
                 (make-circ 200 200 -12 20 false 0 0 true) false))
                 (place-bluecirc (make-circ 200 200 -12 20 false 0 0 true)
                 (place-bluecirc (make-circ 100 200 23 -14 false 0 0 false)
                  (place-image RED-POINTER 0 0 EMPTY-CANVAS)))
                   "Not selecting any circle should not make
                   the outline of any circle as red but it is!")
   
    (check-equal?
     (world-to-scene
     (make-world (make-circ 100 200 23 -14 false 0 0 false)
                 (make-circ 200 200 -12 20 false 20 30 false) false))
                 (place-bluecirc (make-circ 200 200 -12 20 false 20 30 true)
                 (place-bluecirc (make-circ 100 200 23 -14 false 0 0 false)
                                 EMPTY-CANVAS))
                 "Not selecting any circle should not make
                   the outline of any circle as red but it is!")
  )

;; HELPER FUNCTION
;; place-redcirc : Circle Scene -> Scene
;; GIVEN: a circle and a scene
;; RETURNS: a scene like the given one, but with the given circle
;;          painted red on the outline and its coordinates painted on it.
;; EXAMPLE:
;; (place-redcirc (make-circc 200 100 -12 20) EMPTY-CANVAS) will
;; return the given circle with (-12,20) appended at the centre
;; DESIGN STRATEGY: Use template of Circle on c
(define (place-redcirc c s)
  (place-image
    RED-CIRCLE-IMAGE
    (circ-x c) (circ-y c)
    (place-image(text
    (string-append "(" (number->string (circ-vx c)) "," (number->string (circ-vy c)) ")") 14 "blue")
     (circ-x c) (circ-y c) (place-image RED-POINTER (circ-o-x c) (circ-o-y c) s))))

;; HELPER FUNCTION
;; place-bluecirc : Circle Scene -> Scene
;; GIVEN: a circle and a scene
;; RETURNS: a scene like the given one, but with the given circle
;;          painted blue on the outline and its coordinates painted on it.
;; EXAMPLE:
;; (place-bluecirc (make-circ 200 100 -12 20) EMPTY-CANVAS) will
;; return the given circle with (-12,20) appended at the centre
;; DESIGN STRATEGY: Use template of Circle on c
(define (place-bluecirc c s)
  (place-image
    BLUE-CIRCLE-IMAGE
    (circ-x c) (circ-y c) 
    (place-image(text
    (string-append "(" (number->string (circ-vx c)) "," (number->string (circ-vy c)) ")") 14 "blue")
    (circ-x c) (circ-y c) s)))

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

;; world-with-paused-toggled : WorldState -> WorldState
;; RETURNS: a world just like the given one, but with paused? toggled
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: use template for WorldState on w
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

;; world-after-mouse-event: WorldState Int Int MouseEvent -> WorldState
;; GIVEN: A World, the x- and y-coordinates of a mouse event, and the
;;        mouse event
;; RETURNS: the world that should follow the given world after the given mouse
;;          event.
;; DESIGN STRATEGY: use template for WorldState on w
(define (world-after-mouse-event w mx my mev)
  (make-world
    (circ-after-mouse-event (world-circ1 w) mx my mev)
    (circ-after-mouse-event (world-circ2 w) mx my mev)
    (world-paused? w)))

;; HELPER FUNCTION
;; circ-after-mouse-event :  Circle Int Int MouseEvent -> Circle
;; GIVEN: A circle, the x- and y-coordinates of a mouse event, and the
;;        mouse event
;; RETURNS: the circle that should follow the given circle after
;;          the given mouse event
;; DESIGN STRATEGY: Cases on MouseEvent
(define (circ-after-mouse-event c mx my mev)
  (cond
    [(mouse=? mev "button-down") (circ-after-button-down c mx my)]
    [(mouse=? mev "drag") (circ-after-drag c mx my)]
    [(mouse=? mev "button-up") (circ-after-button-up c mx my)]
    [else c]))

;;TESTS
(begin-for-test
  (check-equal? (circ-after-mouse-event (make-circ 10 20 -12 20 false 0 0 false) 32 33 "enter")
                (make-circ 10 20 -12 20 false 0 0 false))
)

;; HELPER FUNCTION
;; circ-after-button-down :  Circle Int Int-> Circle
;; GIVEN: A circle, the x- and y-coordinates of a mouse event, and the
;;        mouse event
;; RETURNS: the circle following the button-down event at the given location.
;; DESIGN STRATEGY: Use template for Circle on c
(define (circ-after-button-down c x y)
  (if (in-circ? c x y)
      (make-circ (circ-x c) (circ-y c) (circ-vx c) (circ-vy c) true x y true)
      (make-circ (circ-x c) (circ-y c) (circ-vx c) (circ-vy c) false x y true)
      ))

;;TESTS
(begin-for-test

  ;; button-down:

  ;; button-down inside circle1
  (check-equal?
    (world-after-mouse-event 
      (make-world
        (make-circ 100 180 -12 20 false 0 0 false)
        (make-circ 200 280 23 -14 false 0 0 false)
        false)
       90 200    ;; a coordinate inside circle1
      "button-down")
    (make-world
      (make-circ 100 180 -12 20 true 90 200 true)
      (make-circ 200 280 23 -14 false 90 200 true)
      false)
    "button down inside circle1 should select it but didn't")


  ;; button-down inside circcle2
  (check-equal?
    (world-after-mouse-event 
      (make-world
        (make-circ 100 180 -12 20 false 0 0 false)
        (make-circ 200 280 23 -14 false 0 0 false)
        false)
      229 260    ;; a coordinate inside circle2
      "button-down")
    (make-world
      (make-circ 100 180 -12 20 false 229 260 true)
      (make-circ 200 280 23 -14 true 229 260 true)
      false)
    "button down inside circle2 should select it but didn't")

  ;; button-down not inside any circle
  (check-equal?
    (world-after-mouse-event 
      (make-world
        (make-circ 100 180 -12 20 false 0 0 false)
        (make-circ 200 280 23 -14 false 0 0 false)
        false)
      10 50   ;; a coordinate not inside circle1 or circle2
      "button-down")
    (make-world
      (make-circ 100 180 -12 20 false 10 50 true)
      (make-circ 200 280 23 -14 false 10 50 true)
      false)
    "button down outside any circle should leave world unchanged, but didn't"))

;; HELPER FUNCTION
;; circ-after-drag : Circle Integer Integer -> Circle
;; GIVEN: A circle, the x- and y-coordinates of a mouse event, and the
;;        mouse event
;; RETURNS: the circle following a drag at the given location
;; STRATEGY: Use template for Circle on c
(define (circ-after-drag c x y)
(if (circ-selected? c)
     (make-circ (+ (circ-x c)(- x (circ-o-x c)))(+ (circ-y c)(- y (circ-o-y c))) (circ-vx c) (circ-vy c) true x y true)
     (make-circ (circ-x c) (circ-y c) (circ-vx c) (circ-vy c) false x y true) ))

;; TESTS
(begin-for-test
  (check-equal?
    (world-after-mouse-event 
      (make-world
        (make-circ 100 180 -12 20 false 0 0 false)
        (make-circ 200 280 23 -14 false 0 0 false)
        false)
       10 20   
      "drag")
    (make-world
      (make-circ 100 180 -12 20 false 10 20 true)
      (make-circ 200 280 23 -14 false 10 20 true)
      false)
    "drag with no cat selected didn't leave world unchanged")

  ;; button-down inside circle2
  (check-equal?
    (world-after-mouse-event 
      (make-world
        (make-circ 100 180 -12 20 true 0 0 true)
        (make-circ 200 280 23 -14 false 0 0 false)
        false)
      70 80   
      "drag")
    (make-world
      (make-circ 170 260 -12 20 true 70 80 true)
      (make-circ 200 280 23 -14 false 70 80 true)
      false)
    "drag when circle1 is selected, should just move circle1, but didn't!")

  ;; button-down not inside any circle
  (check-equal?
    (world-after-mouse-event 
      (make-world
        (make-circ 100 180 -12 20 false 0 0 false)
        (make-circ 200 280 23 -14 true 0 0 true)
        false)
      10 50   
      "drag")
    (make-world
      (make-circ 100 180 -12 20 false 10 50 true)
      (make-circ 210 330 23 -14 true 10 50 true)
      false)
    "drag when circle2 is selected should just move circle2, but didn't!"))

;; HELPER FUNCTION
;; new-circ-after-button-up : NonNegInt NonNegInteger Integer Integer -> Circle
;; GIVEN: x,y coordinates of the circle and their velocities in x and y direction
;; RETURNS: the position of the circle after clicking "button-up" event
;; EXAMPLES:
;; (new-circ-after-button-up 380 280 20 20) -> (make-circ 360 260 -20 -20) 
;; DESIGN STRATEGY: Cases on x and y coordinates of the circle
(define (new-circ-after-button-up x y vx vy)
   (cond
           [(and (would-circ-go-past-right? x vx) (would-circ-go-past-bottom? y vy))
           (make-circ 360 260 (- vx) (- vy) false 0 0 false)]
           [(and (would-circ-go-past-top? y vy) (would-circ-go-past-right? x vx))
           (make-circ 360 40 (- vx) (- vy) false 0 0 false)]
           [(and (would-circ-go-past-left? x vx) (would-circ-go-past-bottom? y vy))
           (make-circ 40 260 (- vx) (- vy) false 0 0 false)]
           [(and (would-circ-go-past-left? x vx) (would-circ-go-past-top? y vy))
           (make-circ 40 40 (- vx) (- vy) false 0 0 false)]        
           [(would-circ-go-past-bottom? y vy)
            (make-circ (+ x vx) (+ y (- CANVAS-HEIGHT y HALF-CIRCLE-HEIGHT)) vx (- vy) false 0 0 false)]
           [(would-circ-go-past-right? x vx)
            (make-circ (+ x (- CANVAS-WIDTH x HALF-CIRCLE-WIDTH))  (+ y vy) (- vx) vy false 0 0 false)]
           [(would-circ-go-past-top? y vy)
            (make-circ (+ x vx) 40 vx (- vy) false 0 0 false)]
           [(would-circ-go-past-left? x vx)
            (make-circ 40 (+ y vy) (- vx) vy false 0 0 false)]
           [else (make-circ x y vx vy false 0 0 false)])
 )

(begin-for-test
  (check-equal? (new-circ-after-button-up 200 100 -12 20) (make-circ 200 100 -12 20 false 0 0 false)
                "Circle at(200,100) should be (-188,120) when applying velocity (-12,20)")
  (check-equal? (new-circ-after-button-up 45 80 -15 20) (make-circ 40 100 15 20 false 0 0 false)
                "Circle at(45,80) should be (40,100) when applying velocity (-15,20)")
  (check-equal? (new-circ-after-button-up 390 200 15 -20) (make-circ 360 180 -15 -20 false 0 0 false)
                "Circle at(390,200) should be (360,180) when applying velocity (15,-20)")
  (check-equal? (new-circ-after-button-up 300 299 15 -20) (make-circ 315 260 15 20 false 0 0 false)
                "Circle at(300,299) should be (315,260) when applying velocity (15,-20)")
  (check-equal? (new-circ-after-button-up 230 10 15 -20) (make-circ 245 40 15 20 false 0 0 false)
                "Circle at (230,10) should be (245,40) when applying velocity (15,-20)")
  (check-equal? (new-circ-after-button-up 230 10 15 -20) (make-circ 245 40 15 20 false 0 0 false)
                "Circle at (230,10) should be (245,40) when applying velocity (15,-20)")
  (check-equal? (new-circ-after-button-up 10 10 -15 -20) (make-circ 40 40 15 20 false 0 0 false)
                "Circle at (10,10) should be (40,40) when applying velocity (-15,-20)")
  (check-equal? (new-circ-after-button-up 390 10 15 -20) (make-circ 360 40 -15 20 false 0 0 false)
                "Circle at (390,10) should be (360,40) when applying velocity (15,-20)")
  (check-equal? (new-circ-after-button-up 10 290 -15 20) (make-circ 40 260 15 -20 false 0 0 false)
                "Circle at (10,290) should be (40,260) when applying velocity (-15,20)")
  (check-equal? (new-circ-after-button-up 10 290 -15 20) (make-circ 40 260 15 -20 false 0 0 false)
                "Circle at (10,290) should be (40,260) when applying velocity (15,-20)")
  (check-equal? (new-circ-after-button-up 380 260 45 40) (make-circ 360 260 -45 -40 false 0 0 false)
                "Circle at (380,260) should be (360,260) when applying velocity (45,40)"))
           
(define (circ-after-button-up c x y)
;   (if (circ-selected? c)
;      (make-circ (+ (circ-x c)(- x (circ-o-x c))) (+ (circ-y c)(- y (circ-o-y c))) (circ-vx c) (circ-vy c) false x y false)
;      (make-circ (circ-x c) (circ-y c) (circ-vx c) (circ-vy c) false x y false)))
  (if (circ-selected? c)
      (new-circ-after-button-up
       (+ (circ-x c)(- x (circ-o-x c)))
       (+ (circ-y c)(- y (circ-o-y c)))
       (circ-vx c) (circ-vy c))
      (make-circ (circ-x c) (circ-y c) (circ-vx c) (circ-vy c) false x y false)))


;; TESTS
;; unselect circle1
(begin-for-test
  (check-equal?
    (world-after-mouse-event
      (make-world
        (make-circ 100 180 -12 20 true 0 0 false)
        (make-circ 200 280 23 -14 false 0 0 false)
        true)
      40 40    ;; arbitrary location
      "button-up")
    (make-world
        (make-circ 140 220 -12 20 false 0 0 false)
        (make-circ 200 280 23 -14 false 40 40 false)
        true)
    "button-up failed to unselect cat1")

;; unselect circle2
  (check-equal?
    (world-after-mouse-event
      (make-world
        (make-circ 100 180 -12 20 false 40 40 false)
        (make-circ 200 280 23 -14 true 40 40 false)
        true)
      40 40    ;; arbitrary location
      "button-up")
    (make-world
        (make-circ 100 180 -12 20 false 40 40 false)
        (make-circ 223 260 23 14 false 0 0 false)
        true)
    "button-up failed to unselect cat1"))

;; HELPER FUNCTION
;; in-circ? : Circle Integer Integer -> Circle
;; RETURNS: true iff the given coordinate is inside the given circle.
;; EXAMPLES: see tests below
;; STRATEGY: Use template for Circle on c
(define (in-circ? c x y)
  (and
    (<= 
      (- (circ-x c) 40)
      x
      (+ (circ-x c) 40))
    (<= 
      (- (circ-y c) 40)
      y
      (+ (circ-y c) 40))))

(begin-for-test
  
  ;; inside the cat
  (check-equal?
    (in-circ? unselected-circle1-at-100-180 140 220)
    true
    "test of in-cat? with nearby point")

  (check-equal?
    (in-circ? unselected-circle1-at-100-180  200 230) 
    false
    "test of in-cat? with distant point"))