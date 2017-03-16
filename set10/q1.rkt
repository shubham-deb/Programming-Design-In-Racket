#lang racket

;; GOAL: to build a marvelous toy, called the MetaToy.

(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "WidgetWorks.rkt")

(check-location "10" "q1.rkt")

;(provide make-metatoy)
;(provide run)
;(provide make-throbber)
;(provide make-clock)
;(provide make-politician)
;(provide Metatoy<%>)
;(provide Toy<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; canvas dimensions
(define CANVAS-WIDTH 500)         ;; in pixels
(define CANVAS-HEIGHT 600)        ;; in pixels
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; canvas constants
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2)) ;; in pixels
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2)) ;; in pixels

;; throbber position constants
(define INITIAL-X (/ CANVAS-WIDTH 2))  
(define INITIAL-Y (/ CANVAS-HEIGHT 2))

;; throbber dimensions
(define THROBBER-RADIUS 5)
(define THROBBER-OFFSET 5)

;; rectangle contsnats
(define RECTANGLE-WIDTH 60)
(define RECTANGLE-HEIGHT 30)
(define HALF-RECTANGLE-WIDTH (/ RECTANGLE-WIDTH 2))
(define HALF-RECTANGLE-HEIGHT (/ RECTANGLE-HEIGHT 2))
(define RECTANGLE (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT "outline" "blue"))

;; key event constants
(define NEW-THROBBER-EVENT "t")
(define NEW-CLOCK-EVENT "c")
(define NEW-POLITICIAN-EVENT "p")

;; Politician constants
(define SQUARE-IMAGE (square 60 "solid" "slateblue"))
(define IMAGE (bitmap "modi.jpg"))
(define POLITICIAN-IMAGE1 (overlay IMAGE SQUARE-IMAGE))
(define IMAGE2 (bitmap "trump.jpg"))
(define POLITICIAN-IMAGE2 (overlay IMAGE2 SQUARE-IMAGE))
(define KEEP-DISTANCE 75)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Data Definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Widget is an object whose class implements the Widget<%>
;; interface.

;; A Toy is an object whose class implements the Toy<%>
;; interface.

;; A ListOfToy (LOT) is either
;; -- empty
;; -- (cons Toy LOT)

;; lot-fn : LOT -> ??
;; (define (lot-fn LOT)
;;   (cond
;;     [(empty? LOT) ...]
;;     [else (...
;;             (send (first LOT) toy-fn)
;;             (lot-fn (rest LOT)))]))
;; Example:
;; (list (make-throbber 250 300) (make-clock 400 500))

;; A World is an object whose class implements the World<%>
;; interface.

;; A Metatoy is an object whose class implements the Metatoy<%>
;; interface.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INTERFACES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Metatoy<%> ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A Metatoy is an object of any class that implements Metatoy<%>.

;Interpretation:A Metatoy will consist of a Canvas which will have a ListOfToys.

(define Metatoy<%>
  (interface
      
      ;; the (World<%>) says that Metatoy<%> inherits from World<%>
      (SWidget<%>)
    
    ;; -> ListOfToy
    get-toys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Widget<%> ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Toy<%> ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Toy is an object of any class that implements Toy<%>

;; Interpretation : A Toy  can be used to get the position of Clock, Throbber
;; and a Politician or move them.

(define Toy<%> 
  (interface
      
      ;; The interface Toy<%> inherits from the interface Widget<%>.
      (SWidget<%>)
    
    ;; -> Int
    ;; RETURNS: the x or y position of the center of the toy
    toy-x
    toy-y
    
    ;; -> Int
    ;; RETURNS: some data related to the toy.  The interpretation of
    ;; this data depends on the class of the toy.
    ;; for a throbber, it is the current radius of the throbber
    ;; for the clock, it is the current value of the clock
    ;; for a politician, it is the current distance to the mouse
    toy-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ################### FUNCTIONS and  CLASS BEGIN ############################
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The Metatoy% class

;; Constructor template for Metatoy%:
;; (new Metatoy% [Toy ListOfToy])
;; Interpretation: A Toy  of class Metatoy% takes signals from
;; big-bang and distributes them to its listofToy.

(define Metatoy%
  (class* object% (Metatoy<%>)
    
    (init-field lot)
    ;  ListOfToy
    
    (super-new)
    
    ;; -> ListOfToy
    (define/public (get-toys) lot)
    
    ;; after-tick : -> Metatoy
    ;; Returns: Metatoy after tick
    ;; Strategy: Use HOF map on the ListOfToy in this Metatoy
    (define/public (after-tick)
       (for-each
        ;; Toy -> Toy
        ;; GIVEN: a toy
        ;; RETURNS: a Toy  like the given one but after a tick 
        (lambda (t) (send t after-tick))
        lot))
    
    ;; to-scene : -> Scene
    ;; Returns: A Scene with all the Toys in lot of Metatoy drawn
    ;; Use HOF foldr on the ListOfToy in this Metatoy  
    (define/public (add-to-scene scene)
      (foldr
       ;; Toy Scene -> Scene
       ;; GIVEN: a toy
       ;; RETURNS: a Scene like the given one but with the Toy  in it
       (lambda (t scene)
         (send t add-to-scene scene)) EMPTY-CANVAS lot))
    
    ;; after-key-event : KeyEvent -> Metatoy
    ;; STRATEGY: Cases on kev
    (define/public (after-key-event kev)
      (cond
        [(key=? kev NEW-THROBBER-EVENT)
          (set! lot (cons (new-throbber) lot))]
        [(key=? kev NEW-CLOCK-EVENT)
          (set! lot (cons (new-clock) lot))]
        [(key=? kev NEW-POLITICIAN-EVENT)
          (set! lot (cons (new-politician) lot))]
        [else
          (for-each
           ;; Toy -> Toy
           ;; GIVEN: a toy
           ;; RETURNS: a Toy  like the given one but after another key event
           (lambda (t) (send t after-key-event kev))
           lot)]))
    
    ;; world-after-mouse-event : Nat Nat MouseEvent -> Metatoy
    ;; STRATEGY: Cases on mev
    (define (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev "button-down")
         (after-button-down mx my)]
        [(mouse=? mev "drag")
         (after-drag mx my)]
        [(mouse=? mev "button-up")
         (after-button-up mx my)]
        [(mouse=? mev "move")
         (after-move mx my)]
        [else this]))
    
    ;; world-after-button-down: Nat Nat -> Metatoy
    ;; STRATEGY: Use HOF map on ListOfToy
    (define/public (after-button-down mx my)
       (for-each
        ;; Toy -> Toy
        ;; GIVEN: a toy
        ;; RETURNS: a Toy  like the given one but after button-down event
        (lambda (t) (send t after-button-down mx my))
        lot))
    
    ;; world-after-button-up: Nat Nat -> Metatoy
    ;; STRATEGY: Use HOF map on ListOfToy
    (define/public (after-button-up mx my)
       (for-each
        ;; Toy -> Toy
        ;; GIVEN: a toy
        ;; RETURNS: a Toy  like the given one but after button-up event
        (lambda (t) (send t after-button-up mx my))
        lot))
    
    ;; world-after-drag: Nat Nat -> Metatoy
    ;; STRATEGY: Use HOF map on ListOfToy
    (define/public (after-drag mx my)
       (for-each
        ;; Toy -> Toy
        ;; GIVEN: a toy
        ;; RETURNS: a Toy  like the given one but after drag event
        (lambda (t) (send t after-drag mx my))
        lot))
    
    ;; world-after-move: Nat Nat -> Metatoy
    ;; STRATEGY: Use HOF map on ListOfToy
    (define/public (after-move mx my)
       (for-each
        ;; Toy -> Toy
        ;; GIVEN: a toy
        ;; RETURNS: a Toy  like the given one but after move event
        (lambda (t) (send t after-move mx my))
        lot))))

;; make-metatoy : ListOfToys -> Metatoy
;; RETURNS: a Metatoy with the given list of toys.
(define (make-metatoy lot)
  (new Metatoy% [lot lot]))

(define (make-container)
  (local ((define EMPTY-CONTAINER (container-init 500 600)))
    (begin
      (send EMPTY-CONTAINER add-stateful-widget (make-metatoy empty))
      (send EMPTY-CONTAINER run .5))))

;;; We have three classes of Widget<%>:  Clock%, Throbber% and Politicians% 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Clocks start at the center of the canvas and display the number
;; of ticks since it was created at the center.
;; They are draggable.

;; Constructor template for Clock%:
;;(new Clock% [x Integer][y Integer]
;;            [mx Integer][my Integer]
;;            [ticks-since-creation NonNegInt])
;; Interpretation: An object of class Clock% represents a clock.

;; new-clock: -> Clock
;; RETURNS: a clock displayed at the center of the canvas

(define (new-clock)
  (new Clock% [x INITIAL-X][y INITIAL-Y]))

;;make-clock : PosInt PosInt -> Toy
;;GIVEN: an x and a y position
;;RETURNS: an object representing a clock at the given position.
(define (make-clock x y)
  (new Clock% [x x][y y]))

(define Clock%
  (class* object% (Toy<%>)
    
    ; the x and y position of the center of the clock
    (init-field x y)   
    
    ;; if the Clock is selected, the position of
    ;; the last button-down event inside the Clock, relative to the
    ;; Clock's center.
    ;; By default, saved-mx and saved-my will be 0
    (init-field [saved-mx 0] [saved-my 0])
    
    ; the number of ticks since the clock was created
    (init-field [ticks-since-creation 0])
    
    ; image for displaying the clock
    (field [CLOCK-IMG
            (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT "outline" "blue")])
    
    ;; color of the clock
    (field [CLOCK-COLOR "blue"])
    
    ;; font size and color of the tick inside the clock
    (field [TICK-FONT 10])
    (field [TICK-COLOR "blue"])
    
    (super-new)
    
    ;; after-tick : -> Toy
    ;; RETURNS: A Toy like this one, but as it should be after a tick
    (define/public (after-tick)
      ;      (new Clock%
      ;           [x x]
      ;           [y y]
      ;           [saved-mx saved-mx]
      ;           [saved-my saved-my]
      (set! ticks-since-creation (+ ticks-since-creation 1)))
    
    ;; after-key-event : KeyEvent -> Toy
    ;; RETURNS: A Toy like this one, but as it should be after the
    ;; given key event.
    ;; DETAILS: clock ignores key events
    (define/public (after-key-event kev)
      this)      
    
    ; after-button-down : Integer Integer -> Toy
    ; GIVEN: the location of a button-down event
    ; RETURNS: A Toy after button-down event
    ; STRATEGY: Cases on whether the event is in the clock
    (define/public (after-button-down mx my)
      (if (in-clock? x y mx my)
          (begin
            (set! saved-mx (- mx x))
            (set! saved-my (- my y)))
          this))
    
    ; after-button-up : Integer Integer -> Toy
    ; GIVEN: the location of a button-up event
    ; RETURNS: A Toy after button-up event
    ; STRATEGY: Returns the same Clock
    ; DETAILS: Ignores button-up event
    (define/public (after-button-up mx my)
      this)   
    
    ; after-drag : Integer Integer -> Toy
    ; GIVEN: the location of a drag event
    ; RETURNS: A Toy after drag event
    ; STRATEGY: Cases on whether the clock is selected.
    (define/public (after-drag mx my)
      (if (in-clock? x y mx my)
          (begin
            (set! x (- mx saved-mx))
            (set! y (- my saved-my)))
          this))
    
    ;;  after-move: Int Int -> Toy
    ;;  RETURNS: the state of this Toy  that should follow a mouse-move
    ;;  at the given coordinates
    ;; DETAILS : Clock Toy ignores "move" event
    (define/public (after-move mx my)
      this)
    
    ;; -> Int
    ;; RETURNS: the x position of the center of the toy
    (define/public (toy-x)
      x)
    
    ;; -> Int
    ;; RETURNS: the y position of the center of the toy
    (define/public (toy-y)
      y)
    
    ;; toy-data: -> Int
    ;; RETURNS: current value of the clock
    (define/public (toy-data)
      ticks-since-creation)
    
    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this clock painted
    ;; on it.
    (define/public (add-to-scene scene)
      (local
        (;; represents the number of ticks since creation
         (define TICKS-SINCE-CREATION (number->string ticks-since-creation)))
        (place-image
         (text TICKS-SINCE-CREATION TICK-FONT TICK-COLOR) x y
         (place-image CLOCK-IMG x y scene))))
    
    ;; in-clock? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this clock.
    ;; STRATEGY : Combine Simpler Functions
    (define (in-clock? x y mx my)
      (and
       (<= 
        (- x HALF-RECTANGLE-WIDTH)
        mx
        (+ x HALF-RECTANGLE-WIDTH))
       (<= 
        (- y HALF-RECTANGLE-HEIGHT)
        my
        (+ y HALF-RECTANGLE-HEIGHT))))))

;; Throbbers start as a green solid circle on the center of the canvas and 
;; can expand and contract.
;; It can be selected and draggable.

;; Constructor template for Throbber%:
;; (new Throbber% [x Integer][y Integer]
;;                           [radius NonNegInt][selected? Boolean]
;;                           [expand? Boolean]
;;                           [saved-mx Integer][saved-my Integer])
;; Interpretation: An object of class Throbber% represents a throbber.

(define Throbber%
  (class* object% (Toy<%>)
    
    ; the x and y position of the center of the throbber
    ; while radius is the radius of the throbber
    (init-field x y)   
    
    ; represents if the throbber is selected or not
    (init-field [selected? false])
    
    ;; represents the radius of the throbber as pixels
    (init-field [radius 5])
    
    ; represents if the throbber is expanding or contracting
    (init-field [expand? true])
    
    ;; if the throbber is selected, the x and y position of
    ;; the last button-down event inside the throbber, relative to the
    ;; throbber's center.  Else any value.
    (init-field [saved-mx 0] [saved-my 0])
    
    (super-new)
    
    ;; after-tick : -> Toy
    ;; RETURNS: A Toy like this one, but as it should be after a tick
    (define/public (after-tick)
      (set! radius (get-radius))
      (set! expand?(expanding?)))
    
    ;; get-radius: -> NonNegInt
    ;; RETURNS: the new radius of the throbber
    ;; EXAMPLES: radius = 5 and expand? = true; (get-radius) -> 10
    ;;           radius = 10 and expand? = false; (get-radius) -> 5
    ;;           radius = 20 and expand? = false; (get-radius) -> 15
    ;; DESIGN STRATEGY: Cases on expand? and radius for max and min radius 
    (define (get-radius)
      (local ((define MAX-RADIUS 20)  ;; in pixels
              (define MIN-RADIUS 5)   ;; in pixels
              (define OFFSET 5))      ;; in pixels
        (cond
          [(and expand? (<= (+ radius OFFSET) MAX-RADIUS))
           (+ radius OFFSET)]
          [(and (not expand?) (>= (- radius OFFSET) MIN-RADIUS))
           (- radius OFFSET)])))
    
    ;; expanding?: -> Boolean
    ;; RETURNS: returns false iff the radius is above
    ;;          MAX-RADIUS else return false
    ;; EXAMPLES: radius = 20; (expanding?) -> false
    ;;           radius = 5; (expand?) -> true
    ;; DESIGN STRATEGY: Cases on radius whether it is above MAX-RADIUS
    ;;                  or below MIN-RADIUS
    (define (expanding?)
      (local ((define MAX-RADIUS 20) ;; in pixels
              (define MIN-RADIUS 5)  ;; in pixels
              (define OFFSET 5))     ;; in pixels
        (cond
          [(> (+ radius OFFSET) MAX-RADIUS) false]
          [(< (- radius OFFSET) MIN-RADIUS) true]
          [else expand?])))
    
    ;; after-key-event : KeyEvent -> Widget
    ;; RETURNS: A Toy like this one, but as it should be after the
    ;; given key event.
    ;; DETAILS: throbber ignores key events
    (define/public (after-key-event kev)
      this)      
    
    ;; -> Int
    ;; RETURNS: the x position of the center of the toy
    (define/public (toy-x)
      x)
    
    ;; -> Int
    ;; RETURNS: the y position of the center of the toy
    (define/public (toy-y)
      y)
    
    ;; -> Int
    ;; RETURNS: the current radius of the throbber
    (define/public (toy-data)
      radius)
    
    ; after-button-down : Integer Integer -> Toy
    ; GIVEN: the location of a button-down event
    ; RETURNS : Toy after button-down event
    ; STRATEGY: Cases on whether the event is in the Throbber
    (define/public (after-button-down mx my)
      (if (in-throbber? mx my)
          (begin
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y)))
          this))
    
    ; after-button-up : Integer Integer -> Toy
    ; GIVEN: the location of a button-up event
    ; RETURNS: Toy on button-up event
    ; STRATEGY: Cases on whether the event is in the throbber.
    ; If the throbber is selected, then unselect it.
    (define/public (after-button-up mx my)
      (if selected?
          (set! selected? false)
          this))   
    
    ; after-drag : Integer Integer -> Toy
    ; GIVEN: the location of a drag event
    ; RETURNS: A Toy after the drag event
    ; STRATEGY: Cases on whether the throbber is selected.
    ; If it is selected, move it so that the vector from the center to
    ; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx saved-mx))
            (set! y (- my saved-my)))
          this))
    
    ;;  Int Int -> Toy
    ;;  RETURNS: the state of this Toy  that should follow a mouse-move
    ;;  at the given coordinates
    (define/public (after-move mx my)
      this)
    
    ;; get-shape: Boolean -> String
    ;; RETURNS: a string specifying mode of the circle
    ;; Example : selected? = true ; (get-shape) -> outline
    ;;           selected? = false ; (get-shape) -> solid
    ;; DESIGN STRATEGY: Cases on selected? 
    (define (get-shape)
      (if selected?
          "outline"
          "solid"))
    
    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this throbber painted
    ;; on it.
    (define/public (add-to-scene scene)
      (local ((define CIRCLE (circle radius (get-shape) "green")))
        
        (place-image CIRCLE x y scene)))
    
    ;; in-throbber? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this throbber.
    ;; DESIGN STRATEGY: Combine simpler functions
    (define (in-throbber? other-x other-y)
      (local ((define X-OFFSET (- x other-x))
              (define Y-OFFSET (- y other-y)))
        
        (<= (+ (sqr X-OFFSET) (sqr Y-OFFSET))
            (sqr radius))))))

;; new-throbber: -> Toy
;; RETURNS: a throbber at the center of canvas with initial radius as 5 pixels
(define (new-throbber)
  (new Throbber% [x INITIAL-X][y INITIAL-Y]))

;;make-throbber: PosInt PosInt -> Toy
;;GIVEN: an x and a y position
;;RETURNS: an object representing a throbber at the given position.
(define (make-throbber x y)
  (new Throbber% [x x][y y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Politicians start at the center of the canvas and move around the canvas.
;; POlitician will start moving towards the mouse pointer and once it is within
;; 75 pixel of mouse pointer, the politician moves out and comes back with a
;; different image

;; Constructor template for Politician%:
;;(new Politician% [x Integer][y Integer]
;;                 [saved-mx Integer][saved-my Integer]
;;                 [politician Image])
;; Interpretation: An object of class Politician% represents a politician.

(define Politician%
  (class* object% (Toy<%>)
    
    ; the x and y position of the center of the politician
    (init-field x y)
    
    ;; if the politician is selected, the x and y position of
    ;; the last button-down event inside the politician, relative to the
    ;; politician's center.
    (init-field [saved-mx (/ CANVAS-WIDTH 2)] [saved-my 0])
    
    ;; the initial image of the politician
    (init-field [politician POLITICIAN-IMAGE1])

    
    ;; get-distance :NonNegInt -> NonNegInt
    ;; RETURNS: the cartesian distance between mouse pointer and the politician
    ;; EXAMPLES: (get-disance 250 300 250 0) -> 150
    ;; DESIGN STARTEGY: Combine simpler functions
    (define/public (get-distance x y saved-mx saved-my)
      (local (;;distance between x coordinate of mouse pointer and politician
              (define X-OFFSET (- x saved-mx))
              ;;distance between y coordinate of mouse pointer and politician
              (define Y-OFFSET (- y saved-my)))
        (sqrt (+ (sqr X-OFFSET) (sqr Y-OFFSET)))))
    
    (init-field [distance
            (get-distance x y saved-mx saved-my)])
    
    (super-new)
    
    ;; mid-point of x-coordinate of the mouse and the politician center
    ;;(define mid-x (/ (- saved-mx x) 2))
    
    ;; mid-point of y-coordinate of the mouse and the politician center
    ;;(define mid-y (/ (- saved-my y) 2))
    
    ;; -> Int
    ;; RETURNS: the x position of the center of the toy
    (define/public (toy-x)
      x)
    
    ;; -> Int
    ;; RETURNS: the y position of the center of the toy
    (define/public (toy-y)
      y)
    
    ;; after-tick : -> Toy
    ;; RETURNS: A Toy like this one, but as it should be after a tick
    (define/public (after-tick)
      (begin
        (set! distance (get-distance x y saved-mx saved-my))
        (set! x (get-x x y saved-mx saved-my))
        (set! y (get-y x y saved-mx saved-my))
        (set! politician (get-politician x y saved-mx saved-my))))

    ;; get-x: NonNegInt NonNegInt NonNegInt NonNegInt -> NonNegInt
    ;; GIVEN: Initial position of the politician and current mouse-pointer
    ;; RETURNS: new x-coordinate of the politician based on the distance
    ;;          between the politician and the mouse
    ;; EXAMPLES:(get-x 250 300 250 0) -> 250
    ;;          (get-x 250 300 250 300) -> 240
    ;; DESIGN STRATEGY:Cases on x-distance between the politician and the mouse,
    ;;                  whether it has crossed 75 pixels
    (define (get-x x y mx my)
        (cond
          ([< distance KEEP-DISTANCE] (new-x x))
          (else (+ x (/ (- saved-mx x) 2)))))
    
    ;; new-x : NonNegInt -> NonNegInt
    ;; GIVEN: current x-position of the politician
    ;; RETURNS: new x-coordinate of the politician after being repelled
    ;; DESIGN STRATEGY; Cases on x-coordinate of the politician if it is going
    ;;                  out of the canvas
    (define (new-x x)
      (- x (* 10 (/ (- saved-mx x) 2))))
    
    ;; get-y:NonNegInt NonNegInt NonNegInt NonNegInt -> NonNegInt
    ;; GIVEN: current position of the politician and mouse-pointer
    ;; RETURNS: new y-coordinate of the politician based on the distance
    ;;          between the politician and the mouse
    ;;EXAMPLES:(get-y 250 300 250 0) -> 150
    ;;          (get-y 250 300 250 300) -> 140
    ;; DESIGN STRATEGY:Cases on y-distance between the politician and the mouse,
    ;;                  whether it has crossed 75 pixels
    (define (get-y x y mx my)
        (cond
          ([< distance KEEP-DISTANCE] (new-y y))
          (else (+ y (/ (- saved-my y) 2)))))
    
    ;; new-y : NonNegInt -> NonNegInt
    ;; GIVEN: current y-position of politician
    ;; RETURNS: new y-coordinate of the politician after being repelled
    ;; DESIGN STRATEGY; Cases on y-coordinate of the politician if it is going
    ;;                  out of the canvas
    (define (new-y y)
      (- y (* 10 (/ (- saved-my y) 2))))
    
    ;; get-politician: NonNegInt NonNegInt NonNegInt NonNegInt Image -> Image
    ;; GIVEN: current image of politician and position of the politician and
    ;;        mouse pointer
    ;; RETURNS: new image of the politician after it gets repelled
    ;; EXAMPLES:(get-politician 250 300 270 330 POLITICIAN-IMAGE1)
    ;;                     -> POLITICIAN-IMAGE2
    ;; DESIGN STRATEGY: Cases on whether the distance between the politician
    ;;                  and the mouse pointer is less than 75 pixels
    (define (get-politician x y mx my)
      (if (< distance KEEP-DISTANCE)
          (cond
            ([equal? politician POLITICIAN-IMAGE1] POLITICIAN-IMAGE2)
            ([equal? politician POLITICIAN-IMAGE2] POLITICIAN-IMAGE1))
          politician))
    
    ;; -> Int
    ;; RETURNS: the current distance to the mouse
    (define/public (toy-data)
      (inexact->exact (round (get-distance))))
    
    ;; after-key-event : KeyEvent -> Toy
    ;; RETURNS: A Toy like this one, but as it should be after the
    ;; given key event.
    (define/public (after-key-event kev)
      this)      
    
    ; after-button-down : Integer Integer -> Toy
    ; RETURNS: A Toy like this one, but as it should be after the
    ;         given button-down event.
    (define/public (after-button-down mx my)
      (begin
        (set! saved-mx mx)
        (set! saved-my my)
        ))
    
    ; after-button-up : Integer Integer -> Toy
    ; RETURNS: Toy like this one, but as it should be after the
    ;         given button-up event.
    (define/public (after-button-up mx my)
      (begin
        (set! saved-mx mx)
        (set! saved-my my)
        ))   
    
    ; after-drag : Integer Integer -> Toy
    ; RETURNS: A Toy like this one, but as it should be after the
    ;         given after-drag event.
    (define/public (after-drag mx my)
      (begin
        (set! saved-mx mx)
        (set! saved-my my)
        ))
    
    ; after-move : Integer Integer -> Toy
    ; RETURNS: A Toy like this one, but as it should be after the
    ;         given after-move event.
    (define/public (after-move mx my)
      (begin
        (set! saved-mx mx)
        (set! saved-my my)
        ))
    
    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this Toy painted
    ;; on it.
    (define/public (add-to-scene scene)
      (place-image politician x y
                   (place-image (text (number->string distance) 14 "red")
                   200 100
                   (place-image (text (number->string (round y)) 14 "red")
                   200 200 scene))
    
    ))))

;; new-politician: -> Toy
;; RETURNS: a politician at the center of the canvas
(define (new-politician)
  (new Politician% [x INITIAL-X] [y INITIAL-Y]))

;;make-politician : PosInt PosInt -> Toy
;;GIVEN: an x and a y position
;;RETURNS: an object representing a politician at the given position.
(define (make-politician x y)
  (new Politician% [x x][y y]))