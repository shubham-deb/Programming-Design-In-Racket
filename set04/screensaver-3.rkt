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
         world-paused?
         new-circle
         circ-x
         circ-y
         circ-vx
         circ-vy
         circ-selected?
         world-after-mouse-event
         circ-after-mouse-event
         world-circles
         circle-after-key-event
         ;circle-pen-down?
         )
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
(define-struct world (circles paused? clicked? mx my))
;; A WorldState is a (make-world Circle Circle Boolean)
;; Interpretation: 
;; circles are the list of circles
;; paused? describes whether or not the world is paused.
;; clicked? describes whether or not the world is clicked.
;; mx and my are the mouse cursor positions

;; TEMPLATE:
;; worldstate-fn : WorldState -> ??
;; (define (worldstate-fn w)
;; (... (world-circles w) (world-paused? w) (world-clicked? w)
;;      (world-mx w) (world-my w)))

;; A List of Circles (LOC) is one of:
;; -- empty
;; -- (cons Circle LOS)

;; Template:
;; ;; loc-fn : LOC -> ??
;; (define (loc-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... (first lst)
;;                (loc-fn (rest lst)))]))

(define-struct circ (x y vx vy selected? o-x o-y))
;; A Circle is a
;;(make-circ NonNegInt NonNegInt Integer Integer Boolean Integer Integer Boolean)
;; Interpretation: 
;; x, y represents the x and y coordinates of the circle.
;; vx, vy give the velocities in x and y direction respectively
;; selected? represents whether the circle was clicked or not
;; o-x and o-y represent the offsets in x and y direction when the circle is dragged

;; Template:
;; circle-fn : Circle -> ??
;; (define (circle-fn c)
;; (... (circ-x c) (circ-y c) (circ-vx c) (circ-vy c)(circ-o-x c)(circ-o-y c)))

;; examples of circles, for testing

(define paused-circle1-at-100-180 (make-circ 100 180 -12 20 false 0 0))
(define unpaused-circle1-at-100-180 (make-circ 100 180 -12 20 false 0 0))

(define paused-circle2-at-120-340 (make-circ 120 340 23 -14 false 0 0))
(define unpaused-circle2-at-120-340 (make-circ 120 340 23 -14 false 0 0))

(define unpaused-circle1-at-88-200 (make-circ 88 200 -12 20 false 0 0 ))
(define unpaused-circle2-at-143-336 (make-circ 143 336 23 -14 false 0 0 ))

(define selected-circle1-at-100-180 (make-circ 100 180 -12 20 true 0 0 ))
(define unselected-circle1-at-100-180 (make-circ 100 180 -12 20 false 0 0 ))

(define selected-circle2-at-120-340 (make-circ 120 340 23 -14 true 0 0 ))
(define unselected-circle2-at-120-340 (make-circ 120 340 23 -14 false 0 0 ))
;; examples of worlds, for testing

(define paused-world
  (make-world
   paused-circle1-at-100-180
   paused-circle2-at-120-340
   true
   0
   0))

(define unpaused-world
  (make-world
   unpaused-circle1-at-100-180
   unpaused-circle2-at-120-340
   false
   0
   0))

;; in paused world, both the circles stop moving in the canvas
(define paused-world-after-tick
  (make-world
   paused-circle1-at-100-180
   paused-circle2-at-120-340
   true
   0
   0))

;; in an unpaused world, both the circles move in the canvas
(define unpaused-world-after-tick
  (make-world
   unpaused-circle1-at-88-200
   unpaused-circle2-at-143-336
   false
   0
   0))

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
            (on-mouse world-after-mouse-event)
            ))

;;TESTS: none

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set
;; DESIGN STRATEGY: combine simpler functions

(define (initial-world y)
  (make-world
   '()
   #false
   #false
   0
   0
   ))

;; world-after-tick : WorldState -> WorldState
;; RETURNS: the world state that should follow the given world state
;;          after a tick.
;; STRATEGY: Use template for WorldState on w
(define (world-after-tick w)
  (if (world-paused? w)
      w
      (cond
        [(empty? (world-circles w)) (if (world-clicked? w)
                                        (make-world (world-circles w) #false #true 0 0)
                                        (make-world (world-circles w) #false #false 0 0))]
        [(world-clicked? w)   (make-world (world-circles w) #false #true (world-mx w) (world-my w))]
        [else (make-world (new-circ (world-circles w)) #false #false 0 0)]
        )))

;; hit-x? NonnegInt Int -> Int
;; GIVEN: the x coordinate and the x-velocity of circle
;; RETURNS: x coordinate of circle
;; DESIGN STRATEGY: cases on x coordinate of circle
(define (hit-x? x vx)
  (cond
    [(>= (+ x vx) 360) 359]
    [(<= (+ x vx) 40)  41]
    [else (+ x vx)]
    ))

;; hit-y? NonnegInt Int -> Int
;; GIVEN: the y coordinate and the y-velocity of circle
;; RETURNS: y coordinate of circle
;; DESIGN STRATEGY: cases on y coordinate of circle
(define (hit-y? y vy)
  (cond
    [(>= (+ y vy) 260) 259]
    [(<= (+ y vy) 40)  41]
    [else (+ y vy)]
    ))

;; hit-vx? NonnegInt Int -> NonNegInt
;; GIVEN: the x coordinate and the x-velocity of circle
;; RETURNS: x velocity of circle
;; DESIGN STRATEGY: cases on x velocity of circle
(define (hit-vx? x vx)
  (cond
    [(>= (+ x vx) 360) (- vx)]
    [(<= (+ x vx) 40) (- vx)]
    [else vx]
    ))

;; hit-vy? NonnegInt Int -> NonNegInt
;; GIVEN: the y coordinate and the y-velocity of circle
;; RETURNS: y velocity of circle
;; DESIGN STRATEGY: cases on y velocity of circle
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
;; DESIGN STRATEGY : Combine simpler functions
(define (new-circle x y vx vy)
  (make-circ x y vx vy #false)
  )

;; new-circ : ListOfCircles -> ListOfCircles
;; GIVEN: a list of circles
;; RETURNS: updated values of list of circles
;; EXAMPLES:
;; (new-circ 40 40 -12 20 0 0 false) -> (make-circ 40 40 12 -20)
;; (new-circ 340 240 24 36) -> (make-circ 364 276 24 36)
;; DESIGN STRATEGY : Use template for LOC on c
(define (new-circ c)
  (cond
    [(empty? c) empty]
    [else
     (cons (make-circ (hit-x? (circ-x (first c)) (circ-vx (first c)))
                      (hit-y? (circ-y (first c)) (circ-vy (first c)))
                      (hit-vx? (circ-x (first c)) (circ-vx (first c)))
                      (hit-vy? (circ-y (first c)) (circ-vy (first c))) #false 0 0)
           (new-circ (rest c)))]
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; draw-circle: Circle -> Scene
;; GIVEN: a circle
;; RETURNS: the scene where circle is red if selected
;;          or blue if it is not
;; DESIGN STRATEGY: Use template for Circle on c 
(define (draw-circle f)
  (if (circ-selected? f)
      RED-CIRCLE-IMAGE
      BLUE-CIRCLE-IMAGE)
  )

;; place-circle: ListOfCircles -> Scene
;; GIVEN: a circle
;; RETURNS: a scene of circle
;; DESIGN STRATEGY: Use template for Circle on c 
(define (place-circle c s)
  (place-image (draw-circle c) (circ-x c) (circ-y c)
               (place-image(text
                            (string-append "(" (number->string (circ-vx c)) "," (number->string (circ-vy c)) ")" ) 14 "blue")
                           (circ-x c) (circ-y c)
                           s)))

;; place-circles: ListOfCircles Scene -> Scene
;; GIVEN: a circle
;; RETURNS: a scene of circles
;; DESIGN STRATEGY: Use template for LOC on c 
(define (place-circles c s)
  (if (empty? c)
      s
      (place-circles (rest c) (place-circle (first c) s)) ))

;; after-world-to-scene: World -> Scene
;; GIVEN: a world
;; RETURNS: a pointer if selected on the canvas otherwise
;;          returns the circles
;; DESIGN STRATEGY: Use template for WorldState on w
(define (after-world-to-scene w)
  (if (world-clicked? w)
      (place-image RED-POINTER (world-mx w) (world-my w)(place-circles (world-circles w) EMPTY-CANVAS ))
      (place-circles (world-circles w) EMPTY-CANVAS)))

;; world-to-scene : WorldState -> Scene
;; RETURNS: given a worldstate, it returns a scene that portrays the given world.
;; EXAMPLE:
;; (world-to-scene paused-world) should return a canvas with
;; two circcles, one at (100,180) and one at (120,340)
;; DESIGN STRATEGY: Use template for WorldState on w
(define (world-to-scene w)
  (cond
    [(empty? (world-circles w)) EMPTY-CANVAS]
    [else (after-world-to-scene w)]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-after-n-or-space : WorldState KeyEvent -> WorldState
;; RETURNS: the WorldState that should follow the given worldstate
;;          after the given keyevent.
;; on space, toggle paused?-- ignore all others
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: cases on KeyEvent kev
(define (world-after-n-or-space w kev)
  (if(equal? kev "n")
     (make-world (cons (make-circ 200 150 0 0 #false 0 0)(world-circles w)) #false #false 0 0)
     (world-with-paused-toggled w)))

;; world-after-key-event : WorldState KeyEvent -> WorldState
;; RETURNS: the WorldState that should follow the given worldstate
;;          after the given keyevent.
;; on space, toggle paused?-- ignore all others
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: cases on KeyEvent kev
(define (world-after-key-event w kev)
  (cond
    [(or (key=? kev " ") (key=? kev "n"))
     (world-after-n-or-space w kev)]
    [(or (key=? kev "left") (key=? kev "right") (key=? kev "up") (key=? kev "down"))
     (make-world (circle-after-key-event-recursion (world-circles w) kev) (world-paused? w) (world-clicked? w) 0 0)]
    [else w]))

;; circle-after-key-event-recursion : ListOfCircles KeyEvent -> ListOfCircles
;; RETURNS: the list of circles that should follow the given list of circles
;;          after the given keyevent.
;; on space, toggle paused?-- ignore all others
;; EXAMPLES: see tests below
;; DESIGN STRATEGY: Use template for LOC on c
(define (circle-after-key-event-recursion c kev)
(cond
  [(empty? c) empty]
  [else (cons (circle-after-key-event (first c) kev)
                          (circle-after-key-event-recursion (rest c) kev)) ]))

;; circle-after-key-event : Circle KeyEvent -> Circle
;; RETURNS: the state of the circle that should follow the given
;;          circle after the given key event
;; DESIGN STRATEGY: Use template for Circle on c
(define (circle-after-key-event c kev)
  (cond
    [(empty? c) c]
    [(key=? kev "left")
     (if (circ-selected? c)
         (make-circ (circ-x c) (circ-y c) (- (circ-vx c) 2) (circ-vy c) #true 0 0)
         c)]
    [(key=? kev "right")
     (if (circ-selected? c)
         (make-circ (circ-x c) (circ-y c) (+ (circ-vx c) 2) (circ-vy c) #true 0 0)
         c)]
     [(key=? kev "up")
      (if (circ-selected? c)
          (make-circ (circ-x c) (circ-y c) (circ-vx c) (- (circ-vy c) 2) #true 0 0)
          c)]
   [(key=? kev "down")
        (if (circ-selected? c)
         (make-circ (circ-x c) (circ-y c) (circ-vx c) (+ (circ-vy c) 2) #true 0 0)
         c)]
    [(key=? kev "d")
       (make-circ 0 0 0 0 #false 0 0)]
    [(key=? kev "e")
       (make-circ 0 0 0 0 #false 0 0)]
      [(key=? kev "u")
       (make-circ 0 0 0 0 #false 0 0)]
      ))

(begin-for-test
(check-equal? (circle-after-key-event (make-circ 200 100 0 0 #true 0 0) "up")
              (make-circ 200 100 0 -2 #true 0 0) "up")
(check-equal? (circle-after-key-event (make-circ 200 100 0 0 #true 0 0) "down")
              (make-circ 200 100 0 2 #true 0 0) "up")
(check-equal? (circle-after-key-event (make-circ 200 100 0 0 #true 0 0) "left")
              (make-circ 200 100 -2 0 #true 0 0) "up")
(check-equal? (circle-after-key-event (make-circ 200 100 0 0 #true 0 0) "right")
              (make-circ 200 100 2 0 #true 0 0) "up")
(check-equal? (circle-after-key-event (make-circ 0 0 0 0 #true 0 0) "d")
              (make-circ 0 0 0 0 #false 0 0) "d")
(check-equal? (circle-after-key-event (make-circ 0 0 0 0 #true 0 0) "e")
              (make-circ 0 0 0 0 #false 0 0) "e")
(check-equal? (circle-after-key-event (make-circ 0 0 0 0 #true 0 0) "u")
              (make-circ 0 0 0 0 #false 0 0) "u")
)

;; circle-pen-down? : Circle -> Boolean
;; RETURNS: true if the pen in the given circle is down
;; DESIGN STARTEGY: Use template for WorldState on w 
(define (world-with-paused-toggled w)
  (make-world
   (world-circles w)
   (not (world-paused? w))
   (world-clicked? w)
   (world-mx w)
   (world-my w)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event: WorldState Int Int MouseEvent -> WorldState
;; GIVEN: A World, the x- and y-coordinates of a mouse event, and the
;;        mouse event
;; RETURNS: the world that should follow the given world after the given mouse
;;          event.
;; DESIGN STRATEGY: use template for WorldState on w
(define (world-after-mouse-event w mx my mev)
  (cond
    [(empty? (world-circles w)) w]
    [else
     (if (or (mouse=? mev "button-down") (mouse=? mev "drag"))
     (make-world (circ-after-mouse-event-recursion (world-circles w) mx my mev)
                 (world-paused? w) #true mx my)
     (make-world (circ-after-mouse-event-recursion (world-circles w) mx my mev)
                 (world-paused? w) #false (world-mx w) (world-my w)))]))

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

;; HELPER FUNCTION
;; circ-after-mouse-event-recursion :  ListOfCircles Int Int MouseEvent -> ListOfCircles
;; GIVEN: A circle, the x- and y-coordinates of a mouse event, and the
;;        mouse event
;; RETURNS: the circle that should follow the given circle after
;;          the given mouse event
;; DESIGN STRATEGY: Use template for LOC on c
(define (circ-after-mouse-event-recursion c mx my mev)
  (cond
    [(empty? c) empty]
    [else (cons (circ-after-mouse-event (first c) mx my mev ) (circ-after-mouse-event-recursion (rest c) mx my mev))]))

;; HELPER FUNCTION
;; circ-after-button-down :  Circle Int Int-> Circle
;; GIVEN: A circle, the x- and y-coordinates of a mouse event, and the
;;        mouse event
;; RETURNS: the circle following the button-down event at the given location.
;; DESIGN STRATEGY: Use template for Circle on c
(define (circ-after-button-down c x y)
  (if (in-circ? c x y)
      (make-circ (circ-x c) (circ-y c) (circ-vx c) (circ-vy c) true x y)
      (make-circ (circ-x c) (circ-y c) (circ-vx c) (circ-vy c) false x y)))

;; HELPER FUNCTION
;; circ-after-drag : Circle Integer Integer -> Circle
;; GIVEN: A circle, the x- and y-coordinates of a mouse event, and the
;;        mouse event
;; RETURNS: the circle following a drag at the given location
;; STRATEGY: Use template for Circle on c
(define (circ-after-drag c x y)
  (if (circ-selected? c)
      (make-circ (+ (circ-x c)(- x (circ-o-x c)))(+ (circ-y c)(- y (circ-o-y c))) (circ-vx c) (circ-vy c) true x y)
      (make-circ (circ-x c) (circ-y c) (circ-vx c) (circ-vy c) false x y) ))

;; HELPER FUNCTION
;; new-circ-after-button-up : NonNegInt NonNegInteger Integer Integer -> Circle
;; GIVEN: x,y coordinates of the circle and their velocities in x and y direction
;; RETURNS: the position of the circle after clicking "button-up" event
;; EXAMPLES:
;; (new-circ-after-button-up 380 280 20 20) -> (make-circ 360 260 -20 -20) 
;; DESIGN STRATEGY: Cases on x and y coordinates of the circle
(define (new-circ-after-button-up c)
  (make-circ (hit-x? (circ-x c) (circ-vx c)) (hit-y? (circ-y c) (circ-vy c))  (hit-vx? (circ-x c) (circ-vx c))
             (hit-vy? (circ-y c) (circ-vy c)) false 0 0))

(define (circ-after-button-up c x y)
  (if (circ-selected? c)
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