#lang racket

;; GOAL: to build a marvelous toy, called the MetaToy.

(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")

;(check-location "09" "q1.rkt")

(provide make-metatoy)
(provide run)
(provide make-throbber)
(provide make-clock)
(provide make-politician)
(provide Metatoy<%>)
(provide Toy<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

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

;; rectangle contsnats
(define RECTANGLE-WIDTH 60)
<<<<<<< HEAD
(define RECTANGLE-HEIGHT 60)
(define HALF-RECTANGLE-WIDTH (/ RECTANGLE-WIDTH 2))
(define HALF-RECTANGLE-HEIGHT (/ RECTANGLE-HEIGHT 2))
=======
(define RECTANGLE-HEIGHT 30)
(define HALF-RECTANGLE-WIDTH (/ RECTANGLE-WIDTH 2))
(define HALF-RECTANGLE-HEIGHT (/ RECTANGLE-HEIGHT 2))
(define RECTANGLE (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT "outline" "blue"))
>>>>>>> 4e39a75a800581731e4bae65556680534e1b0686

;; key event constants
(define NEW-THROBBER-EVENT "t")
(define NEW-CLOCK-EVENT "c")
(define NEW-POLITICIAN-EVENT "p")

;; image constants
(define SQUARE-IMAGE (square 60 "solid" "slateblue"))
(define IMAGE (bitmap "modi.jpg"))
(define politician-image (overlay IMAGE SQUARE-IMAGE))
(define IMAGE2 (bitmap "trump.jpg"))
(define politician-image2 (overlay IMAGE2 SQUARE-IMAGE))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Data Definitions

;; A Widget is an object whose class implements the Widget<%>
;; interface. 

;; A World is an object whose class implements the World<%>
;; interface. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INTERFACES 

;; A World is an object of any class that implements Metatoy<%>.
<<<<<<< HEAD

(define World<%>
  (interface ()
    
    ; -> World
    ; GIVEN: no arguments
    ; RETURNS: the state of the world at the next tick
    after-tick          
    
    ; Integer Integer MouseEvent-> World
    ; GIVEN: a location
    ; RETURNS: the state of the world that should follow the
    ; given mouse event at the given location.
    after-mouse-event
    
    ; KeyEvent : KeyEvent -> World
    ; GIVEN: a key event
    ; RETURNS: the state of the world that should follow the
    ; given key event
    after-key-event     
    
    ; -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene that depicts this World
    to-scene
    ))

=======

(define World<%>
  (interface ()
    
    ; -> World
    ; GIVEN: no arguments
    ; RETURNS: the state of the world at the next tick
    after-tick          
    
    ; Integer Integer MouseEvent-> World
    ; GIVEN: a location
    ; RETURNS: the state of the world that should follow the
    ; given mouse event at the given location.
    after-mouse-event
    
    ; KeyEvent : KeyEvent -> World
    ; GIVEN: a key event
    ; RETURNS: the state of the world that should follow the
    ; given key event
    after-key-event     
    
    ; -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene that depicts this World
    to-scene
    ))

>>>>>>> 4e39a75a800581731e4bae65556680534e1b0686
;; A Metatoy is an object of any class that implements Metatoy<%>.

(define Metatoy<%>
  (interface
      
      ;; the (World<%>) says that Metatoy<%> inherits from World<%>
      (World<%>)
    
    ;; -> ListOfToy
    get-toys))

;; Every object that lives in the world must implement the Widget<%>
;; interface.

(define Widget<%>
  (interface ()
    
    ; -> Widget
    ; GIVEN: no arguments
    ; RETURNS: the state of this object that should follow at time t+1.
    after-tick          
    
    ; Integer Integer -> Widget
    ; GIVEN: a location
    ; RETURNS: the state of this object that should follow the
    ; specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag
    
    ; KeyEvent : KeyEvent -> Widget
    ; GIVEN: a key event and a time
    ; RETURNS: the state of this object that should follow the
    ; given key event
    after-key-event     
    
    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    add-to-scene
    ))

;; A Toy is an object of any class that implements Toy<%> 

(define Toy<%> 
  (interface
      
      ;; The interface Toy<%> inherits from the interface Widget<%>.
      (Widget<%>)
    
    ;;  Int Int -> Toy
    ;;  RETURNS: the state of this toy that should follow a mouse-move
    ;;  at the given coordinates
    after-move
    
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
    toy-data
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initial world will not contain any toys

;; initial-world : -> Metatoy
;; RETURNS: a world with a helicopter and no throbbers
(define (initial-world)
  (make-metatoy
   empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start with (run framerate).  Typically: (run 0.25)

;; run : PosNum -> Metatoy
;; GIVEN: a frame rate (in seconds/tick)
;; EFFECT: creates a MetaToy with no toys in it, and runs it using big-bang
;; at the given frame rate.  Returns the final state of the Metatoy.
;; DESIGN STRATEGY: Deliver events to the event handler functions
(define (run rate)
  (big-bang (initial-world)
            (on-tick (lambda (w) (send w after-tick)) rate)
            (on-draw (lambda (w) (send w to-scene)))
            (on-key (lambda (w kev) (send w after-key-event kev)))
            (on-mouse (lambda (w mx my mev)
                        (send w after-mouse-event mx my mev)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The Metatoy% class

;; Constructor template for Metatoy%:
;; (new Metatoy% [Toy ListOfToy])
;; Interpretation: A toy of class Metatoy% takes signals from
;; big-bang and distributes them to its list of toy as appropriate.

(define Metatoy%
  (class* object% (Metatoy<%>)
    
    (init-field lot)
    ;  ListOfToy
    
    (super-new)
    
    ;; -> ListOfToy
    (define/public (get-toys) lot)
    
    ;; after-tick : -> Metatoy
    ;; Use HOF map on the ListOfToy in this Metatoy
    (define/public (after-tick)
      (make-metatoy
       (map
        ;; Toy -> Toy
        ;; GIVEN: a toy
        ;; RETURNS: a toy like the given one but after a tick 
        (lambda (t) (send t after-tick))
        lot)))
    
    ;; to-scene : -> Scene
    ;; Use HOF foldr on the ListOfToy in this Metatoy  
    (define/public (to-scene)
      (foldr
       ;; Toy Scene -> Scene
       ;; GIVEN: a toy
       ;; RETURNS: a Scene like the given one but with the toy in it
       (lambda (t scene)
         (send t add-to-scene scene)) EMPTY-CANVAS
                                      lot))
    
    
    ;; after-key-event : KeyEvent -> Metatoy
    ;; STRATEGY: Cases on kev
    (define/public (after-key-event kev)
      (cond
        [(key=? kev NEW-THROBBER-EVENT)
         (make-metatoy
          (cons (new-throbber) lot)
          )]
        [(key=? kev NEW-CLOCK-EVENT)
         (make-metatoy
          (cons (new-clock) lot)
          )]
        [(key=? kev NEW-POLITICIAN-EVENT)
         (make-metatoy
          (cons (new-politician) lot)
          )]
        [else
         (make-metatoy
          (map
           ;; Toy -> Toy
           ;; GIVEN: a toy
           ;; RETURNS: a toy like the given one but after another key event
           (lambda (t) (send t after-key-event kev))
           lot))]))
    
    ;; world-after-mouse-event : Nat Nat MouseEvent -> Metatoy
    ;; STRATEGY: Cases on mev
    (define/public (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev "button-down")
         (world-after-button-down mx my)]
        [(mouse=? mev "drag")
         (world-after-drag mx my)]
        [(mouse=? mev "button-up")
         (world-after-button-up mx my)]
        [(mouse=? mev "move")
         (world-after-move mx my)]
        [else this]))
    
    ;; world-after-button-down: Nat Nat -> Metatoy
    ;; STRATEGY: Use HOF map on ListOfToy
    (define (world-after-button-down mx my)
      (make-metatoy
       (map
        ;; Toy -> Toy
        ;; GIVEN: a toy
        ;; RETURNS: a toy like the given one but after button-down event
        (lambda (t) (send t after-button-down mx my))
        lot)))
    
    ;; world-after-button-up: Nat Nat -> Metatoy
    ;; STRATEGY: Use HOF map on ListOfToy
    (define (world-after-button-up mx my)
      (make-metatoy
       (map
        ;; Toy -> Toy
        ;; GIVEN: a toy
        ;; RETURNS: a toy like the given one but after button-up event
        (lambda (t) (send t after-button-up mx my))
        lot)))
    
    ;; world-after-drag: Nat Nat -> Metatoy
    ;; STRATEGY: Use HOF map on ListOfToy
    (define (world-after-drag mx my)
      (make-metatoy
       (map
        ;; Toy -> Toy
        ;; GIVEN: a toy
        ;; RETURNS: a toy like the given one but after drag event
        (lambda (t) (send t after-drag mx my))
        lot)))
    
    ;; world-after-move: Nat Nat -> Metatoy
    ;; STRATEGY: Use HOF map on ListOfToy
    (define (world-after-move mx my)
      (make-metatoy
       (map
        ;; Toy -> Toy
        ;; GIVEN: a toy
        ;; RETURNS: a toy like the given one but after move event
        (lambda (t) (send t after-move mx my))
        lot)))
    ))

;; make-metatoy : ListOfToys -> Metatoy
;; RETURNS: a Metatoy with the given list of toys.
(define (make-metatoy lot)
  (new Metatoy% [lot lot]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We have three classes of Widget<%>:  Clocks, Throbbers and Politicians 

;; Clocks start at the center of the canvas and display the number
;; of ticks since it was created.
;; They are draggable.

;; Constructor template for Clock%:
;;(new Clock% [x Integer][y Integer]
;;                     [mx Integer][my Integer][ticks-since-creation NonNegInt])
;; last three arguments are optional
;; Interpretation: An object of class Clock% represents a clock.
<<<<<<< HEAD
=======

;; new-clock: -> Clock
;; RETURNS: a clock displayed at the center of the canvas
(define (new-clock)
  (new Clock% [x INITIAL-X][y INITIAL-Y]))

;;make-clock : PosInt PosInt -> Toy
;;GIVEN: an x and a y position
;;RETURNS: an object representing a clock at the given position.
(define (make-clock x y)
  (new Clock% [x x][y y]))

>>>>>>> 4e39a75a800581731e4bae65556680534e1b0686
(define Clock%
  (class* object% (Toy<%>)
    
    ; the x and y position of the center of the clock
    (init-field x y)   
    
    ;; if the heli is selected, the position of
    ;; the last button-down event inside the heli, relative to the
    ;; heli's center.  Else any value.
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
    
    ;; after-tick : -> Clock
    ;; RETURNS: A clock like this one, but as it should be after a tick
    (define/public (after-tick)
      (new Clock%
           [x x]
           [y y]
           [saved-mx saved-mx]
           [saved-my saved-my]
           [ticks-since-creation (+ ticks-since-creation 1)]))
    
    ;; after-key-event : KeyEvent -> Clock
    ;; RETURNS: A world like this one, but as it should be after the
    ;; given key event.
    ;; DETAILS: clock ignores key events
    (define/public (after-key-event kev)
      this)      
    
    ; after-button-down : Integer Integer -> Clock
    ; GIVEN: the location of a button-down event
    ; STRATEGY: Cases on whether the event is in the clock
    (define/public (after-button-down mx my)
      (if (in-clock? x y mx my)
          (new Clock%
               [x x][y y]
               [saved-mx (- mx x)]
               [saved-my (- my y)]
               [ticks-since-creation ticks-since-creation])
          this))
    
    ; after-button-up : Integer Integer -> Clock
    ; GIVEN: the location of a button-up event
    ; STRATEGY: Cases on whether the event is in the clock.
    (define/public (after-button-up mx my)
<<<<<<< HEAD
      (new Clock%
           [x x][y y]
           [saved-mx saved-mx]
           [saved-my saved-my]
           [ticks-since-creation ticks-since-creation]))   
=======
      this)   
>>>>>>> 4e39a75a800581731e4bae65556680534e1b0686
    
    ; after-drag : Integer Integer -> Clock
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether the clock is selected.
    (define/public (after-drag mx my)
      (if (in-clock? x y mx my)
          (new Clock%
               [x (- mx saved-mx)]
               [y (- my saved-my)]
               [saved-mx saved-mx]
               [saved-my saved-my]
               [ticks-since-creation ticks-since-creation])
          this))
    
    ;;  after-move: Int Int -> Toy
    ;;  RETURNS: the state of this toy that should follow a mouse-move
    ;;  at the given coordinates
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
    (define (in-clock? x y mx my)
      (and
       (<= 
        (- x HALF-RECTANGLE-WIDTH)
        mx
        (+ x HALF-RECTANGLE-WIDTH))
       (<= 
        (- y HALF-RECTANGLE-HEIGHT)
        my
        (+ y HALF-RECTANGLE-HEIGHT))))
<<<<<<< HEAD
    ))

;; new-clock: -> Clock
;; RETURNS: a clock displayed at the center of the canvas
(define (new-clock)
  (new Clock% [x INITIAL-X][y INITIAL-Y]))
=======

    ))

;; Some constants declaration for Testing
(define clock-t0 (make-clock 250 300))
(define clock-t1 (send clock-t0 after-tick))
(define clock-t1-selected (send clock-t1 after-button-down 250 300))
(define clock-t1-dragged (send clock-t1-selected after-drag 260 310))
(define clock-t0-unselected (send clock-t0 after-button-down 300 300))
(define clock-t0-unselected-drag (send clock-t0-unselected after-drag 350 350))

(begin-for-test
  (check-equal? (send clock-t0 toy-x)
                250
                " Initial X-Pos not proper")
  
  (check-equal? (send clock-t0 toy-y)
                300
                " Initial Y-Pos not proper")
  
  (check-equal? (send clock-t0 toy-data)
                0
                " Counter not starting from 0")

  (check-equal? (send clock-t1 toy-data)
                1
                " Counter not getting incremented")

  (check-equal? (and (equal? (send clock-t1-dragged toy-x)
                             260)
                     (equal? (send clock-t1-dragged toy-y)
                             310))
                true
                "Smooth drag not working for Clock")

  (check-equal? (send clock-t1 after-key-event "t")
                clock-t1
                "Clock is getting altered after throbber creation")

  (check-equal? (send clock-t1 after-move 300 400)
                clock-t1
                "Clock responding to move event !!")

  (check-equal? (and (equal? (send clock-t0-unselected-drag toy-x)
                             250)
                     (equal? (send clock-t0-unselected-drag toy-y)
                             300))
                true
                "Unselected Clock being dragged")

  (check-equal? (send clock-t1-selected after-button-up 260 310)
                clock-t1-selected
                "Clock not getting deselected")

  (check-equal? (send clock-t0 add-to-scene EMPTY-CANVAS)
                (place-image
                 (text (number->string (send clock-t0 toy-data)) 10 "blue")
                 (send clock-t0 toy-x)
                 (send clock-t0 toy-y)
                 (place-image RECTANGLE
                              (send clock-t0 toy-x)
                              (send clock-t0 toy-y)
                              EMPTY-CANVAS))
                "Draw failed for Clock"))

>>>>>>> 4e39a75a800581731e4bae65556680534e1b0686

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Throbbers start as a green circle on the center of the canvas and expand
;; and contract.
;; It can be selected and draggable.

;; Constructor template for Throbber%:
;; (new Throbber% [x Integer][y Integer]
;;                           [radius NonNegInt][selected? Boolean]
;;                           [saved-mx Integer][saved-my Integer])
;; Interpretation: An object of class Throbber% represents a throbber.

(define Throbber%
  (class* object% (Toy<%>)
    
    ; the x and y position of the center of the throbber
    ; while radius is the radius of the throbber
    (init-field x y radius)   
    
    ; represents if the throbber is selected or not
    (init-field [selected? false])
    
    ; represents if the throbber is expanding or contracting
    (init-field [expand? true])
    
    ;; if the throbber is selected, the x and y position of
    ;; the last button-down event inside the throbber, relative to the
    ;; helicopter's center.  Else any value.
    (init-field [saved-mx 0] [saved-my 0])
    
    (super-new)
    
    ;; after-tick : -> Toy
    ;; RETURNS: A throbber like this one, but as it should be after a tick
    (define/public (after-tick)
      (new Throbber%
           [x x][y y]
<<<<<<< HEAD
           [radius (get-radius radius)]
           [selected? selected?]
           [expand? (expanding? radius)]
=======
           [radius (get-radius radius expand?)]
           [selected? selected?]
           [expand? (expanding? radius expand?)]
>>>>>>> 4e39a75a800581731e4bae65556680534e1b0686
           [saved-mx saved-mx]
           [saved-my saved-my]))
    
    ;; get-radius: NonNegInt -> NonNegInt
    ;; GIVEN: radius of the throbber
    ;; RETURNS: the new radius of the throbber
    ;; EXAMPLES:..
    ;; DESIGN STRATEGY: Cases on expand? and radius on some boundary conditions
<<<<<<< HEAD
    (define (get-radius radius)
      (local ((define MAX-RADIUS 20)  ;; in pixels
              (define MIN-RADIUS 5)   ;; in pixels
              (define OFFSET 5))      ;; in pixels
        (cond
          [(and expand? (<= (+ radius OFFSET) MAX-RADIUS)) (+ radius OFFSET)]
          [(and expand? (>= (+ radius OFFSET) MAX-RADIUS)) (- radius OFFSET)]
          [(and (not expand?) (>= (- radius OFFSET) MIN-RADIUS)) (- radius OFFSET)]
=======
    (define (get-radius radius expand?)
      (local ((define MAX-RADIUS 20)  ;; in pixels
              (define MIN-RADIUS 5)   ;; in pixels
              (define OFFSET 2))      ;; in pixels
        (cond
          [(and expand? (< (+ radius OFFSET) MAX-RADIUS)) (+ radius OFFSET)]
          [(and expand? (> (+ radius OFFSET) MAX-RADIUS)) (- radius OFFSET)]
          [(and (not expand?) (> (- radius OFFSET) MIN-RADIUS)) (- radius OFFSET)]
>>>>>>> 4e39a75a800581731e4bae65556680534e1b0686
          [(and (not expand?) (<= (- radius OFFSET) MIN-RADIUS)) (+ radius OFFSET)]
          )))
    
    ;; expanding?: NonNegInt -> Boolean
    ;; GIVEN: radius of the throbber
    ;; RETURNS: returns false iff the radius is above
    ;;          MAX-RADIUS else return false
    ;; EXAMPLES:...
    ;; DESIGN STRATEGY: Cases on radius whether it is above MAX-RADIUS
    ;;                  or below MIN-RADIUS
<<<<<<< HEAD
    (define (expanding? radius)
      (local ((define MAX-RADIUS 20) ;; in pixels
              (define MIN-RADIUS 5)  ;; in pixels
              (define OFFSET 1))     ;; in pixels
        (cond
          [(>= (+ radius OFFSET) MAX-RADIUS) false]
=======
    (define (expanding? radius expand?)
      (local ((define MAX-RADIUS 20) ;; in pixels
              (define MIN-RADIUS 5)  ;; in pixels
              (define OFFSET 2))     ;; in pixels
        (cond
          [(> (+ radius OFFSET) MAX-RADIUS) false]
>>>>>>> 4e39a75a800581731e4bae65556680534e1b0686
          [(<= (- radius OFFSET) MIN-RADIUS) true]
          [else expand?])))
    
    ;; after-key-event : KeyEvent -> Widget
    ;; RETURNS: A world like this one, but as it should be after the
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
    ; STRATEGY: Cases on whether the event is in the helicopter
    (define/public (after-button-down mx my)
      (if (in-throbber? mx my)
          (new Throbber%
               [x x][y y]
               [radius radius]
               [selected? true]
<<<<<<< HEAD
               [expand? expand?]
=======
>>>>>>> 4e39a75a800581731e4bae65556680534e1b0686
               [saved-mx (- mx x)]
               [saved-my (- my y)])
          this))
    
    ; after-button-up : Integer Integer -> Toy
    ; GIVEN: the location of a button-up event
    ; STRATEGY: Cases on whether the event is in the throbber.
    ; If the throbber is selected, then unselect it.
    (define/public (after-button-up mx my)
      (if (in-throbber? mx my)
          (new Throbber%
               [x x][y y]
               [radius radius]
               [selected? false]
<<<<<<< HEAD
               [expand? expand?]
=======
>>>>>>> 4e39a75a800581731e4bae65556680534e1b0686
               [saved-mx saved-mx]
               [saved-my saved-my])
          this))   
    
    ; after-drag : Integer Integer -> Toy
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether the throbber is selected.
    ; If it is selected, move it so that the vector from the center to
    ; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
<<<<<<< HEAD
      (if (in-throbber? mx my)
=======
      (if selected?
>>>>>>> 4e39a75a800581731e4bae65556680534e1b0686
          (new Throbber%
               [x (- mx saved-mx)]
               [y (- my saved-my)]
               [radius radius]
               [selected? true]
<<<<<<< HEAD
               [expand? expand?]
=======
>>>>>>> 4e39a75a800581731e4bae65556680534e1b0686
               [saved-mx saved-mx]
               [saved-my saved-my])
          this))
    
    ;;  Int Int -> Toy
    ;;  RETURNS: the state of this toy that should follow a mouse-move
    ;;  at the given coordinates
    (define/public (after-move mx my)
      this)

    ;; get-shape: -> String
    ;; RETURNS: a string
    ;; DESIGN STRATEGY: Cases on selected? 
    (define/public (get-shape)
      (if selected?
          "outline"
          "solid"))
    
    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this helicopter painted
    ;; on it.
    (define/public (add-to-scene scene)
      (local ((define CIRCLE (circle radius (send this get-shape) "green")))
        
        (place-image CIRCLE x y scene)))
    
    ;; in-throbber? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this throbber.
    ;; DESIGN STRATEGY: Combine simpler functions
    (define (in-throbber? other-x other-y)
      (local ((define X-OFFSET (- x other-x))
              (define Y-OFFSET (- y other-y)))
        
        (<= (+ (sqr X-OFFSET) (sqr Y-OFFSET))
            (sqr radius)))
      )))

;; new-throbber: -> Toy
;; RETURNS: a throbber at the center of canvas with initial radius as 5 pixels
(define (new-throbber)
  (new Throbber% [x INITIAL-X][y INITIAL-Y] [radius THROBBER-RADIUS]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Politicians start at the center of the canvas and move around the canvas.


;; Constructor template for Politician%:
;;(new Politician% [x Integer][y Integer]
;;                 [saved-mx Integer][saved-my Integer][politician Image])
;; last three arguments are optional
;; Interpretation: An object of class Politician% represents a politician.

(define Politician%
  (class* object% (Toy<%>)
    
    ; the x and y position of the center of the politician
    (init-field x y)
    
    ;; if the politician is selected, the x and y position of
    ;; the last button-down event inside the politician, relative to the
    ;; politician's center.  Else any value.
    (init-field [saved-mx 0] [saved-my 0])
    
    ;; the initial image of the politician
    (init-field [politician politician-image])
    
    (super-new)
    
    ;; mid-point of x-coordinate of the mouse and the politician 
    (define mid-x (/ (- saved-mx x) 2))
    
    ;; mid-point of y-coordinate of the mouse and the politician 
    (define mid-y (/ (- saved-my y) 2))
    
    ;; -> Int
    ;; RETURNS: the x position of the center of the toy
    (define/public (toy-x)
      x)
    
    ;; -> Int
    ;; RETURNS: the y position of the center of the toy
    (define/public (toy-y)
      y)
    
    ;; after-tick : -> Toy
    ;; RETURNS: A politician like this one, but as it should be after a tick
    (define/public (after-tick)
      (new Politician%
<<<<<<< HEAD
           [x (get-x)]
=======
           [x (get-x x y saved-mx saved-my)]
>>>>>>> 4e39a75a800581731e4bae65556680534e1b0686
           [y (get-y x y saved-mx saved-my)]
           [saved-mx saved-mx]
           [saved-my saved-my]
           [politician (get-politician x y saved-mx saved-my politician)]
           ))

    ;; -> Int
    ;; RETURNS: the current distance to the mouse
    (define/public (toy-data)
      (round (get-distance)))
 
    ;; after-key-event : KeyEvent -> Toy
    ;; RETURNS: A world like this one, but as it should be after the
    ;; given key event.
    (define/public (after-key-event kev)
      this)      
    
    ; after-button-down : Integer Integer -> Toy
    ; RETURNS: A politician like this one, but as it should be after the
    ;         given button-down event.
    (define/public (after-button-down mx my)
      (new Politician%
           [x x]
           [y y]
           [saved-mx mx]
           [saved-my my]
           [politician politician]))
    
    ; after-button-up : Integer Integer -> Toy
    ; RETURNS: A politician like this one, but as it should be after the
    ;         given button-up event.
    (define/public (after-button-up mx my)
      (new Politician%
           [x x]
           [y y]
           [saved-mx mx]
           [saved-my my]
           [politician politician]))   
    
    ; after-drag : Integer Integer -> Toy
    ; RETURNS: A politician like this one, but as it should be after the
    ;         given after-drag event.
    (define/public (after-drag mx my)
      (new Politician%
           [x x]
           [y y]
           [saved-mx mx]
           [saved-my my]
           [politician politician]))

    ; after-move : Integer Integer -> Toy
    ; RETURNS: A politician like this one, but as it should be after the
    ;         given after-move event.
    (define/public (after-move mx my)
      (new Politician%
           [x x]
           [y y]
           [saved-mx mx]
           [saved-my my]
           [politician politician]))
    
    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this helicopter painted
    ;; on it.
    (define/public (add-to-scene scene)
      (place-image politician x y scene))

<<<<<<< HEAD
    ;; get-x: -> NonNegInt
=======
    ;; get-x: NonNegInt NonNegInt NonNegInt NonNegInt -> NonNegInt
>>>>>>> 4e39a75a800581731e4bae65556680534e1b0686
    ;; GIVEN: Initial position of the politician and current mouse-pointer
    ;; RETURNS: new x-coordinate of the politician based on the distance
    ;;          between the politician and the mouse
    ;; EXAMPLES:...
    ;; DESIGN STRATEGY:Cases on x-distance between the politician and the mouse,
    ;;                  whether it has crossed 75 pixels
<<<<<<< HEAD
    (define (get-x)
      (local ((define MIN-DISTANCE 75))
        (cond
          ([<= (get-distance x y saved-mx saved-my) MIN-DISTANCE] (new-x x))
=======
    (define (get-x x y mx my)
      (local ((define MIN-DISTANCE 75))
        (cond
          ([< (get-distance x y mx my) MIN-DISTANCE] (new-x x))
>>>>>>> 4e39a75a800581731e4bae65556680534e1b0686
          (else (+ x mid-x)))))
    
    ;; new-x : NonNegInt -> NonNegInt
    ;; GIVEN: current x-position of the politician
    ;; RETURNS: new x-coordinate of the politician after being repelled
    ;; EXAMPLES:...
    ;; DESIGN STRATEGY; Cases on x-coordinate of the politician if it is going
    ;;                  out of the canvas
    (define (new-x x)
<<<<<<< HEAD
      (- x (* mid-x 10)))
=======
      (local ((define OFFSET 60))
        (if (> x HALF-CANVAS-WIDTH)
            (- x HALF-CANVAS-WIDTH)
            (+ x (- HALF-CANVAS-WIDTH OFFSET)))))
>>>>>>> 4e39a75a800581731e4bae65556680534e1b0686
    
    ;; get-y:NonNegInt NonNegInt NonNegInt NonNegInt -> NonNegInt
    ;; GIVEN: current position of the politician and mouse-pointer
    ;; RETURNS: new y-coordinate of the politician based on the distance
    ;;          between the politician and the mouse
    ;; EXAMPLES:...
    ;; DESIGN STRATEGY:Cases on y-distance between the politician and the mouse,
    ;;                  whether it has crossed 75 pixels
    (define (get-y x y mx my)
      (local ((define MIN-DISTANCE 75))
        (cond
          ([< (get-distance x y mx my) MIN-DISTANCE] (new-y y))
          (else (+ y mid-y)))))
    
    ;; new-y : NonNegInt -> NonNegInt
    ;; GIVEN: current y-position of politician
    ;; RETURNS: new y-coordinate of the politician after being repelled
    ;; EXAMPLES:...
    ;; DESIGN STRATEGY; Cases on y-coordinate of the politician if it is going
    ;;                  out of the canvas
    (define (new-y y)
<<<<<<< HEAD
      (- y (* mid-y 10)))
=======
      (local ((define OFFSET 60))
        (if (> y HALF-CANVAS-HEIGHT)
            (- y HALF-CANVAS-HEIGHT)
            (+ y (- HALF-CANVAS-HEIGHT OFFSET)))))
>>>>>>> 4e39a75a800581731e4bae65556680534e1b0686
    
    ;; get-distance :NonNegInt -> NonNegInt
    ;; RETURNS: the cartesian distance between mouse pointer and the politician
    ;; EXAMPLES:...
    ;; DESIGN STARTEGY: Combine simpler functions
    (define (get-distance x y saved-mx saved-my)
      (local (;;distance between x coordinate of mouse pointer and politician
              (define X-OFFSET (- x saved-mx))
              ;;distance between y coordinate of mouse pointer and politician
              (define Y-OFFSET (- y saved-my)))
        (sqrt (+ (sqr X-OFFSET) (sqr Y-OFFSET)))))
    
    ;; get-politician: NonNegInt NonNegInt NonNegInt NonNegInt Image -> Image
    ;; GIVEN: current image of politician and position of the politician and mouse pointer
    ;; RETURNS: new image of the politician after it gets repelled
    ;; EXAMPLES:...
    ;; DESIGN STRATEGY: Cases on whether the distance between the politician
    ;;                  and the mouse pointer is less than 75 pixels
    (define (get-politician x y mx my politician)
      (local ((define MIN-DISTANCE 75))
      (if (< (get-distance x y mx my) MIN-DISTANCE)
          (cond
            ([equal? politician politician-image] politician-image2)
            ([equal? politician politician-image2] politician-image))
          politician)))
    ))

;; new-politician: -> Toy
;; RETURNS: a politician at the center of the canvas
(define (new-politician)
  (new Politician% [x INITIAL-X] [y INITIAL-Y]))

;;make-throbber: PosInt PosInt -> Toy
;;GIVEN: an x and a y position
;;RETURNS: an object representing a throbber at the given position.
(define (make-throbber x y)
  (new Throbber% [x x][y y]))

<<<<<<< HEAD
;;make-clock : PosInt PosInt -> Toy
;;GIVEN: an x and a y position
;;RETURNS: an object representing a clock at the given position.
(define (make-clock x y)
  (new Clock% [x x][y y]))

=======
>>>>>>> 4e39a75a800581731e4bae65556680534e1b0686
;;make-politician : PosInt PosInt -> Toy
;;GIVEN: an x and a y position
;;RETURNS: an object representing a politician at the given position.
(define (make-politician x y)
  (new Politician% [x x][y y]))