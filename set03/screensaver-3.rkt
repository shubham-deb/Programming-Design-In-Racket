;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname screensaver-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; GOAL: to build a screensaver

;; start with (screensaver 0.5)

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(check-location "04" "screensaver-3.rkt")

(provide screensaver
         initial-world
         world-after-tick
         world-after-key-event
         world-circ1
         world-circ2
         world-paused?
         ;new-circle
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
(define-struct world (circ1 circ2 paused? clicked?))
;; A WorldState is a (make-world Circle Circle Boolean)
;; Interpretation: 
;; cir1 and cir2 are the two circles 
;; paused? describes whether or not the circle is paused.

;; TEMPLATE:
;; worldstate-fn : WorldState -> ??
;; (define (worldstate-fn w)
;; (... (world-cir1 w) (world-cir2 w) (world-paused? w)))

(define-struct circ (x y vx vy selected? o-x o-y)) 
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

;;;;;;;;;;;;;;;;;;;;;;;;
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
    (make-circ CIRCLE1-X-COORD CIRCLE1-Y-COORD  -12 20 false 0 0)
    (make-circ CIRCLE2-X-COORD CIRCLE2-Y-COORD 23 -14 false 0 0)
    false
    false
    ))

;; TESTS
;(begin-for-test
;  (check-equal? (initial-world 23)
;                (make-world (make-circ 200 100 -12 20 false 0 0 false)
;                            (make-circ 200 200 23 -14 false 0 0 false) true)
;                "Initial world should be initialized specified in problem set
;                  but it is not!"))

;; world-after-tick : WorldState -> WorldState
;; RETURNS: the world state that should follow the given world state
;;          after a tick.
;; STRATEGY: Use template for WorldState on w

(define (world-after-tick w)
  (if (world-paused? w)
      w
      ;(make-world (world-circ1 w) (world-circ2 w) (world-paused? w))
      (make-world
      (new-circ (world-circ1 w))
      (new-circ (world-circ2 w))
      (world-paused? w)
      (world-clicked? w))))

(define (hit-x? x vx)
(cond
  [(>= (+ x vx) 360) 359]
  [(<= (+ x vx) 40)  41]
  [else (+ x vx)]
  ))

(define (hit-y? y vy)
(cond
  [(>= (+ y vy) 260) 259]
  [(<= (+ y vy) 40)  41]
  [else (+ y vy)]
  ))

(define (hit-vx? x vx)
(cond
  [(>= (+ x vx) 360) (- vx)]
  [(<= (+ x vx) 40) (- vx)]
  [else vx]
  ))

(define (hit-vy? y vy)
(cond
  [(>= (+ y vy) 260) (- vy)]
  [(<= (+ y vy) 40) (- vy)]
  [else vy]
  ))

;; new-circle : NonNegInt NonNegInt Int Int -> Circle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: an unselected circle centered at (x,y), which will travel with
;;          velocity (vx, vy).
;; EXAMPLES:
;; (new-circle 40 40 -12 20 0 0 false) -> (make-circ 40 40 12 -20)
;; (new-circle 340 240 24 36) -> (make-circ 364 276 24 36)
;; DESIGN STRATEGY : Cases on x and y coordinates of the given circle
(define (new-circ c)
  (if (circ-selected? c)
      c
      (make-circ (hit-x? (circ-x c) (circ-vx c)) (hit-y? (circ-y c) (circ-vy c))  (hit-vx? (circ-x c) (circ-vx c))
                 (hit-vy? (circ-y c) (circ-vy c)) false 0 0)))

;(begin-for-test
;  (check-equal? (new-circle 200 100 -12 20) (make-circ 188 120 -12 20 false 0 0 false)
;                "(200,100) should be (-188,120) when applying velocity (-12,20)")
;  (check-equal? (new-circle 45 80 -15 20) (make-circ 40 100 15 20 false 0 0 false)
;                "(45,80) should be (40,100) when applying velocity (-15,20)")
;  (check-equal? (new-circle 390 200 15 -20) (make-circ 360 180 -15 -20 false 0 0 false)
;                "(390,200) should be (360,180) when applying velocity (15,-20)")
;  (check-equal? (new-circle 300 299 15 -20) (make-circ 315 260 15 20 false 0 0 false)
;                "(300,299) should be (315,260) when applying velocity (15,-20)")
;  (check-equal? (new-circle 230 10 15 -20) (make-circ 245 40 15 20 false 0 0 false)
;                "(230,10) should be (245,40) when applying velocity (15,-20)")
;  (check-equal? (new-circle 230 10 15 -20) (make-circ 245 40 15 20 false 0 0 false)
;                "(230,10) should be (245,40) when applying velocity (15,-20)")
;  (check-equal? (new-circle 10 10 -15 -20) (make-circ 40 40 15 20 false 0 0 false)
;                "(10,10) should be (40,40) when applying velocity (-15,-20)")
;  (check-equal? (new-circle 390 10 15 -20) (make-circ 360 40 -15 20 false 0 0 false)
;                "(390,10) should be (360,40) when applying velocity (15,-20)")
;  (check-equal? (new-circle 10 290 -15 20) (make-circ 40 260 15 -20 false 0 0 false)
;                "(10,290) should be (40,260) when applying velocity (-15,20)")
;  (check-equal? (new-circle 10 290 -15 20) (make-circ 40 260 15 -20 true 0 0 false)
;                "(10,290) should be (40,260) when applying velocity (-15,20)")
;  (check-equal? (new-circle 380 260 45 40) (make-circ 360 260 -45 -40 false 0 0 false)
;                "(380,260) should be (360,260) when applying velocity (45,40)"))

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
   [(world-clicked? w)
   (place-image RED-POINTER (circ-o-x (world-circ1 w)) (circ-o-y (world-circ1 w))
   (place-bluecirc (world-circ1 w) (place-bluecirc (world-circ2 w) EMPTY-CANVAS)))]
   [else (place-bluecirc (world-circ1 w) (place-bluecirc (world-circ2 w) EMPTY-CANVAS))]))

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
   (not (world-paused? w))
   (world-clicked? w)))

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
    (world-paused? w)
    (world-clicked? w)))

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

;(define (new-circle-after-button-down  x y vx vy s o-x o-y c)
;  (if s
;        (make-circ (hit-dx? x vx) (hit-dy? y vy)  (hit-vx? x vx) (hit-vy? y vy) true o-x o-y true)
;        (make-circ (hit-dx? x vx) (hit-dy? y vy)  (hit-vx? x vx) (hit-vy? y vy) false o-x o-y true)))
;; HELPER FUNCTION
;; circ-after-button-down :  Circle Int Int-> Circle
;; GIVEN: A circle, the x- and y-coordinates of a mouse event, and the
;;        mouse event
;; RETURNS: the circle following the button-down event at the given location.
;; DESIGN STRATEGY: Use template for Circle on c
(define (circ-after-button-down c x y)
  (if (in-circ? c x y)
      (make-circ (circ-x c) (circ-y c) (circ-vx c) (circ-vy c) true x y)
      (make-circ (circ-x c) (circ-y c) (circ-vx c) (circ-vy c) false x y)
      ))

;; HELPER FUNCTION
;; circ-after-drag : Circle Integer Integer -> Circle
;; GIVEN: A circle, the x- and y-coordinates of a mouse event, and the
;;        mouse event
;; RETURNS: the circle following a drag at the given location
;; STRATEGY: Use template for Circle on c
(define (circ-after-drag c x y)
(if (circ-selected? c)
     (make-circ (+ (circ-x c)(- x (circ-o-x c)))(+ (circ-y c)(- y (circ-o-y c))) (circ-vx c) (circ-vy c) true x y)
     (make-circ (circ-x c) (circ-y c) (circ-vx c) (circ-vy c) false x y)))

(define (new-circ-after-button-up c)
(make-circ (hit-x? (circ-x c) (circ-vx c)) (hit-y? (circ-y c) (circ-vy c))  (hit-vx? (circ-x c) (circ-vx c))
                 (hit-vy? (circ-y c) (circ-vy c)) false 0 0))

(define (circ-after-button-up c x y)
  (if (circ-selected? c)
      ;(make-circ
       ;(+ (circ-x c)(- x (circ-o-x c)))
       ;(+ (circ-y c)(- y (circ-o-y c)))
       ;(circ-vx c) (circ-vy c) false 0 0)
      (new-circ-after-button-up c)
      (make-circ (circ-x c) (circ-y c) (circ-vx c) (circ-vy c) false x y)))

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

