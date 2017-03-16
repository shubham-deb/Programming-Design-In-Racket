#lang racket

;; Program to simulate picking up team members by the leader cublet
;; Enter (run 0.5) to start the simulation

(require 2htdp/universe)
(require 2htdp/image)
(require "WidgetWorks.rkt")
(require "extras.rkt")
(require rackunit)
(require "sets.rkt")

(check-location "10" "q2.rkt")

(provide cubelets-init)
(provide make-block)
(provide SBlock<%>)
(provide run)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS

;; canvas dimensions
(define CANVAS-WIDTH 600)         ;; in pixels
(define SQUARE-SIDE 20)           ;; in pixels
(define HALF-SQUARE-SIDE 10)      ;; in pixels
(define HALF-CANVAS-WIDTH 300)    ;; in pixels
(define CANVAS-HEIGHT 500)        ;; in pixels
(define HALF-CANVAS-HEIGHT 250)   ;; in pixels
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; key event constants
(define NEW-SQUARE-BLOCK-EVENT "b")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Data Definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A SBlock is an object whose class implements the SWidget<%>
;; interface.

;; A Metablock is an object whose class implements the SWidget<%>
;; interface.

;; A ListOfSBlock (LOB) is either
;; -- empty
;; -- (cons SBlock LOB)

;; lob-fn : LOB -> ??
;; (define (lob-fn LOB)
;;   (cond
;;     [(empty? LOB) ...]
;;     [else (...
;;             (send (first LOB) block-fn)
;;             (lob-fn (rest LOB)))]))
;; Example:
;; (list (make-block 100 200 empty) (make-block 250 300 empty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interfaces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define SBlock<%>
  (interface (SWidget<%>)
    
    ;; RETURNS: the teammates of this sblock
    ;; get-team : -> ListOfSBlock
    get-team
    
    ;; EFFECT: adds the given sblock to this block's team
    ;; add-teammate: SBlock -> Void
    add-teammate
    
    ;; intersects?: -> Boolean
    ;; Returns : True, iff this square intersect the other-b
    intersects?
    
    ;; intersect-responder: Integer^2 -> Boolean
    ;; Returns : True, iff a square with the given other-x,other-y intersects
    ;;           this square
    intersect-responder
    
    ;; updated-list-of-objects : ListOfSBlock -> Void
    ;; EFFECT : get the updated list of lob's from Metablock
    updated-list-of-objects
    
    ;; new-mouse-location-teammate : Integer Integer -> Void
    ;; GIVEN : Mouse location
    ;; EFFECT : returns the updated mouse location of the teammates.
    ;;          It gets the mouse position of it's leader and saves it
    ;;          in it's fields
    new-mouse-location-teammate
    
    ;; new-drag-position-teammate : Integer Integer -> Void
    ;; GIVEN : Mouse location
    ;; EFFECT : returns the new dragged postion of the teammate and
    ;;          saves the dragged position to saved-mx and saved-my
    new-drag-position-teammate
    
    ;;RETURNS: the x or y coordinates of this sblock
    ;; sblock-x : -> Integer
    ;; sblock-y : -> Integer
    sblock-x
    sblock-y ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cubelets-init : -> Container
;; GIVEN: no arguments
;; RETURNS: a Container, initially with no blocks, which when run, will
;; run in a 600x500 canvas and process the events in the description above.

(define (cubelets-init)
  (container-init CANVAS-WIDTH CANVAS-HEIGHT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-block : NonNegInt NonNegInt ListOfSBlock -> SBlock
;; GIVEN: an x and y position, and a list of blocks
;; WHERE: the list of blocks is the list of blocks already on the playground.
;; RETURNS: a new block, at the given position, with no teammates

(define (make-block x y lob)
  (new SBlock% [x x] [y y] [lob lob]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The Metablock% class

;; Constructor template for Metablock%:
;; (new Metablock% [lob ListOfSBlock])
;; Interpretation: Metablock% object will hold the
;;                 List of all the SBlocks created

(define Metablock%
  (class* object% (SWidget<%>)
    
    ;; ListOfSBlock indicating all the SBlocks created in Canvas
    (init-field lob)
    
    ;; The Last know location of Mouse Pointer in the Canvas.
    ;; By default, it will be referring to the Centre of the Canvas
    (init-field [saved-mx HALF-CANVAS-WIDTH] [saved-my HALF-CANVAS-HEIGHT])
    
    (super-new)
    
    ; after-tick : -> Void
    ; GIVEN: no arguments
    ; EFFECT: updates this widget to the state it should have
    ; following a tick.
    (define/public (after-tick)
      (for-each
       ;; SBlock -> SBlock
       ;; GIVEN: a SBlock
       ;; RETURNS: a SBlock  like the given one but after a tick 
       (lambda (b) (send b after-tick))
       lob))
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    (define/public (add-to-scene scene)
      (foldr
       ;; SBlock Scene -> Scene
       ;; GIVEN: a SBlock
       ;; RETURNS: a Scene like the given one but with the SBlock  in it
       (lambda (b scene)
         (send b add-to-scene scene)) EMPTY-CANVAS lob))
    
    ; after-key-event : KeyEvent -> Void
    ; GIVEN: a key event kev
    ; EFFECT: updates this widget to the state it should have
    ;         following the given key event
    (define/public (after-key-event kev)
      (cond
        [(key=? kev NEW-SQUARE-BLOCK-EVENT)
         (set! lob
               (cons (new SBlock% [x saved-mx] [y saved-my]) lob))
         (for-each (lambda (b) (send b updated-list-of-objects lob)) lob)]
        [else
         (for-each
          ;; SBlock -> SBlock
          ;; GIVEN: a SBlock
          ;; RETURNS: a SBlock  like the given one but after another key event
          (lambda (b) (send b after-key-event kev))
          lob)]))
    
    ; after-button-down : Integer Integer -> Void
    ; GIVEN: The location of button-down-event
    ; EFFECT: updates this widget to the state it should have
    ;        following the button-down event at the given location.
    (define/public (after-button-down mx my)
      (begin
        (set! saved-mx mx)
        (set! saved-my my))
      (for-each
       ;; SBlock -> SBlock
       ;; GIVEN: a SBlock
       ;; RETURNS: a SBlock  like the given one but after button-down event
       (lambda (b)
         (send b after-button-down mx my))
       lob))
    
    ; after-button-up : Integer Integer -> Void
    ; GIVEN: a location of button-up-event
    ; EFFECT: updates this widget to the state it should have
    ;         following the button-up event at the given location.
    (define/public (after-button-up mx my)
      (begin
        (set! saved-mx mx)
        (set! saved-my my))
      (for-each
       ;; SBlock -> SBlock
       ;; GIVEN: a SBlock
       ;; RETURNS: a SBlock  like the given one but after button-up event
       (lambda (b) (send b after-button-up mx my))
       lob))
    
    ; after-button-drag : Integer Integer -> Void
    ; GIVEN: a location of drag-event
    ; EFFECT: updates this widget to the state it should have
    ;         following the drag event at the given location.
    (define/public (after-drag mx my)
      (for-each
       ;; SBlock -> SBlock
       ;; GIVEN: a SBlock
       ;; RETURNS: a SBlock  like the given one but after drag event
       (lambda (b) (send b after-drag mx my))
       lob))
    
    ; after-move : Integer Integer -> Void
    ; GIVEN: a location of move-event
    ; EFFECT: updates this widget to the state it should have
    ;         following the move-event event at the given location.
    (define/public (after-move mx my)
      (for-each
       ;; SBlock -> SBlock
       ;; GIVEN: a SBlock
       ;; RETURNS: a SBlock  like the given one but after move event
       (lambda (b) (send b after-move mx my))
       lob))
    
    ;; Test Helper Functions
    ;; Returns : The ListOfSBlock present in the Canvas
    (define/public (for-test:lot)
      lob)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world: ListOfSBlock -> Metablock
;; RETURNS: a Metablock with the given list of SBlocks.
(define (initial-world lob)
  (new Metablock% [lob lob]))

;; run : PosNum -> Void
;; GIVEN: a frame rate (in seconds/tick)
;; EFFECT: This function creates a Container, and places a MetaBlock with
;; no SBlocks in that Container.  The function may or may not put other
;; Widgets and SWidgets in the container, depending on the
;; implementation. The function then runs the Container at the given
;; frame rate using WidgetWorks.

(define (run r)
  (local ((define EMPTY-CONTAINER (cubelets-init)))
    (begin
      (send EMPTY-CONTAINER add-stateful-widget (initial-world empty))
      (send EMPTY-CONTAINER run r))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The SBlock% class

;; Constructor template for SBlock%:
;; (new SBlock% [x Integer] [y Integer]
;;              [saved-mx Integer] [saved-my Integer]
;;              [lob ListOfSBlock] [selected? Boolean])
;; Interpretation: SBlock% object is the fundamental object on which a MetaBlock
;;                 can be built. SBlock% object represents a cublet in Canvas

(define SBlock%
  (class* object% (SBlock<%>)
    
    ; the x and y position of the center of the SBlock
    (init-field x y)
    
    ;; if the SBlock is selected, the x and y position of
    ;; the last button-down event inside the SBlock, relative to the
    ;; SBlocks's center.
    (init-field [saved-mx 0] [saved-my 0])
    
    ;; Holds the ListOfSBlock which will represent all other SBlocks in the
    ;; Canvas expect this SBlock
    (init-field [lob empty])
    
    ;; True, iff the SBlock is selected
    (init-field [selected? false])
    
    ;; Holds the ListOfSBlock which are teammates of this SBlock
    (field [lot empty])
    
    (super-new)
    
    ; after-tick : -> Void
    ; GIVEN: no arguments
    ; EFFECT: SBlock will have no effect after tick
    (define/public (after-tick)
      void)
    
    ; KeyEvent -> Void
    ; GIVEN: a keyevent kev
    ; EFFECT: SBlock will have no effect after key event
    (define/public (after-key-event kev)
      void)      
    
    ;; new-mouse-location-teammate : Integer Integer -> Void
    ;; GIVEN : Mouse location
    ;; EFFECT : returns the updated mouse location of the teammates.
    ;;          It gets the mouse position of it's leader and saves it
    ;;          in it's fields
    (define/public (new-mouse-location-teammate mx my)
      (begin
        (set! saved-mx mx)
        (set! saved-my my)))
    
    ;; new-drag-position-teammate : Integer Integer -> Void
    ;; GIVEN : Mouse location
    ;; EFFECT : returns the new dragged postion of the teammate and
    ;;          saves the dragged position to saved-mx and saved-my
    (define/public (new-drag-position-teammate mx my)
      (begin
        (set! x (+ x (- mx saved-mx)))
        (set! y (+ y (- my saved-my))) 
        (new-mouse-location-teammate mx my)
        ))
    
    ; after-button-down : Integer Integer -> Void
    ; GIVEN: a Mouse location
    ; EFFECT: updates this widget to the state it should have
    ;         following the button-down event the given location.
    (define/public (after-button-down mx my)
      (if (in-square? x y mx my)
          (begin
            (set! selected? true)
            (set! saved-mx mx)
            (set! saved-my my)
            (make-teammates lob mx my)
            (for-each 
             (lambda(t)
               (send t new-mouse-location-teammate mx my))
             lot))
          void))
    
    ;; in-square? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this square.
    ;; STRATEGY : Combine Simpler Functions
    (define (in-square? x y mx my)
      (and
       (<= 
        (- x HALF-SQUARE-SIDE)
        mx
        (+ x HALF-SQUARE-SIDE))
       (<= 
        (- y HALF-SQUARE-SIDE)
        my
        (+ y HALF-SQUARE-SIDE))))
    
    ; after-button-up : Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: updates this widget to the state it should have
    ;         following the bitton-up at the given location.
    (define/public (after-button-up mx my)
      (if selected?
          (begin
            (set! selected? false)
            (set! saved-mx mx)
            (set! saved-my my)
            (for-each 
             (lambda(t)
               (send t new-mouse-location-teammate mx my))
             lot)
            )
          void))   
    
    ;; intersects?: -> Boolean
    ;; Returns : True, iff this square intersect the other-b
    (define/public (intersects? other-b)
      (send other-b intersect-responder x y))
    
    ;; intersect-responder: Integer^2 -> Boolean
    ;; Returns : True, iff a square with the given other-x,other-y intersects
    ;;           this square
    (define/public (intersect-responder other-x other-y)
      (and (<= (abs (- x other-x)) SQUARE-SIDE)
           (<= (abs (- y other-y)) SQUARE-SIDE)))
    
    
    ;; get-team : -> ListOfSBlock
    ;; RETURNS: the teammates of this sblock
    (define/public (get-team)
      (begin
        (set! lot (set-diff lot (list this)))
        lot))
    
    ; sblock-x : -> Integer
    ; RETURNS: the x coordinates of this sblock
    (define/public (sblock-x)
      x)
    
    ; sblock-y : -> Integer
    ; RETURNS: the y coordinates of this sblock
    (define/public (sblock-y)
      y)
    
    ;; add-teammate: SBlock -> Void
    ;; EFFECT: adds the given sblock to this block's team
    (define/public (add-teammate other-block)
      (set! lot (set-cons other-block lot)))
    
    ;; make-teammates : ListOfSBlock Integer Interger -> Void
    ;; Effect : It will make various pairs of each teammate.
    ;;          Teammates are added so that they are symmetric and transitive
    ;; Let Block A intersect Block B
    (define (make-teammates lob mx my)
      (for-each
       (lambda (b)
         (if (send this intersects? b)
             (begin
               
               ;; add B in teammates of A
               (set! lot (set-cons b lot))
               
               ;; add A in B's teammates
               (send b add-teammate this)
               
               ;; add B's teammates in A
               ;; add A to teammates of B
               (for-each
                (lambda (b)
                  (begin
                    (set! lot (set-cons b lot))
                    (send b add-teammate this)
                    ))
                (send b get-team))
               
               ;; add A's teammates in B
               ;; add B to teammates of A
               (for-each
                (lambda (t)
                  (begin 
                    (send b add-teammate t)
                    (send t add-teammate b))
                  )
                lot)
               
               ;; add A's teammates to B's teammates
               (for-each
                (lambda (b)
                  (for-each
                   (lambda (t)
                     (send b add-teammate t))
                   lot)) 
                (send b get-team))
               
               ;; add B's teammates to A's teammates
               (for-each
                (lambda (t)
                  (for-each
                   (lambda (b)
                     (send t add-teammate b))
                   (send b get-team))) 
                lot)
               
               ;update the mouse-x,y diff of the teammates.
               (for-each 
                (lambda(t)
                  (send t new-mouse-location-teammate mx my))
                lot)
               )
             void)) lob))
    
    ; after-drag : Integer Integer -> Void
    ; EFFECT: State of SBlock is updated after the
    ;         given after-drag event.
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (+ x (- mx saved-mx)))
            (set! y (+ y (- my saved-my)))
            (set! saved-mx mx)
            (set! saved-my my)
            (set! lob (set-diff lob (set-union lot (list this))))
            ;; for checking if the square intersects with the other square
            ;; and then addin them to lot if they intersect
            (make-teammates lob mx my)
            (for-each
             (lambda (t) (send t new-drag-position-teammate mx my)) lot)
            )
          void))
    
    ;; updated-list-of-objects : ListOfSBlock -> Void
    ;; EFFECT : get the updated list of lob's from Metablock
    (define/public (updated-list-of-objects updated-lob)
      (set! lob updated-lob))
    
    ; after-move : Integer Integer -> Void
    ; GIVEN: a Mouse location
    ; EFFECT: No effect on SBlock for move event
    (define/public (after-move mx my)
      void)
    
    ; add-to-scene : Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    (define/public (add-to-scene scene)
      (if selected?
          (place-image (square SQUARE-SIDE "outline" "red") x y scene)    
          (place-image (square SQUARE-SIDE "outline" "green") x y scene)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
(begin-for-test
  (local
    ((define mblock1 (initial-world empty)))
    
    (send mblock1 after-key-event "b")
    (check-equal? (length (send mblock1 for-test:lot)) 1
                  "SBlock is not getting created on b")
    
    (send mblock1 after-tick)
    (check-equal? (and (equal? (send (first (send mblock1 for-test:lot))
                                     sblock-x)
                               300)
                       (equal? (send (first (send mblock1 for-test:lot))
                                     sblock-y)
                               250))
                  true
                  "SBlock changing after tick")
    
    (send mblock1 after-key-event "c")
    (check-equal? (length (send mblock1 for-test:lot)) 1
                  "SBlock is getting created for unwanted keys")
    
    ;; add-to-scene test
    (check-equal? (send mblock1 add-to-scene EMPTY-CANVAS)
                  (place-image (square 20 "outline" "green")
                               300 250 EMPTY-CANVAS)
                  "Failed to draw a SBlock")
    
    (send mblock1 after-button-down 300 250)
    
    (check-equal? (send mblock1 add-to-scene EMPTY-CANVAS)
                  (place-image (square 20 "outline" "red")
                               300 250 EMPTY-CANVAS)
                  "Failed to draw a SBlock")
    
    (send mblock1 after-button-up 300 250)
    
    ;; Test for Mouse Events
    (send mblock1 after-button-down 300 250)
    (send mblock1 after-drag 350 300)
    (send mblock1 after-button-up 350 300)
    (send mblock1 after-move 400 350)
    
    (check-equal? (and (equal? (send (first (send mblock1 for-test:lot))
                                     sblock-x)
                               350)
                       (equal? (send (first (send mblock1 for-test:lot))
                                     sblock-y)
                               300))
                  true
                  "SBlock not getting dragged")
    ))

(begin-for-test
  (local
    ((define block1 (make-block 300 250 empty))
     (define block2 (make-block 350 250 (list block1)))
     (define block3 (make-block 250 250 (list block1 block2))))
    
    (send block1 after-button-down 350 350)
    (send block1 after-drag 370 370)
    (send block1 after-button-up 370 370)
    (send block1 after-move 400 400)
    
    (check-equal? (and (equal? (send block1 sblock-x) 300)
                       (equal? (send block1 sblock-y) 250))
                  true
                  "Unselected block is moving")
    
    ;; Add block1 and block2 friends using Mouse movement
    
    (send block2 after-button-down 350 250)
    (send block2 after-drag 320 250)
    (send block2 after-button-up 320 250)
    
    (check-equal?
     (and (equal? (send (first (send block2 get-team)) sblock-x) 300)
          (equal? (send (first (send block2 get-team)) sblock-y) 250))
     true
     "block1 not present in block2 friend list")
    
    (check-equal?
     (and (equal? (send (first (send block1 get-team)) sblock-x) 320)
          (equal? (send (first (send block1 get-team)) sblock-y) 250))
     true
     "block2 not present in block1 friend list")
    
    (check-equal?
     (length (send block1 get-team)) 1 "Block1 failed to add teammate")
    
    (check-equal?
     (length (send block2 get-team)) 1 "Block2 failed to add teammate")
    
    ;; Move the cublets once they are team memebers with block2 as leader
    (send block2 after-button-down 320 250)
    (send block2 after-drag 350 250)
    (send block2 after-button-up 350 250)
    
    ;; Check for positions after drag
    (check-equal? (and (equal? (send block1 sblock-x) 330)
                       (equal? (send block1 sblock-y) 250))
                  true
                  "Teammate of leader not moving after drag")
    
    (check-equal? (and (equal? (send block2 sblock-x) 350)
                       (equal? (send block2 sblock-y) 250))
                  true
                  "Leader not moving after drag")
    
    ;; Add Block3 to the team of Block1 and Block2
    (send block1 after-button-down 330 250)
    (send block1 after-drag 270 250)
    ;; Picked up Block3 as teammate
    (send block1 after-button-up 270 250)
    
    (check-equal? (send block1 sblock-x) 270
                  "Leader of new team failed for drag")
    
    (check-equal? (send block2 sblock-x) 290
                  "Follower is not following leader")
    
    (send block1 add-teammate block3)
    (send block2 add-teammate block3)
    (send block3 add-teammate block1)
    (send block3 add-teammate block2)
    
    
    
    (send block2 after-button-down 290 250)
    (send block2 after-drag 300 250)
    (send block2 after-button-up 300 250)
    
    ;; Check for positions after drag
    (check-equal? (and (equal? (send block1 sblock-x) 280)
                       (equal? (send block1 sblock-y) 250))
                  true
                  "Teammate of leader not moving after drag")
    
    (check-equal? (and (equal? (send block2 sblock-x) 300)
                       (equal? (send block2 sblock-y) 250))
                  true
                  "Leader not moving after drag")
    
    (check-equal? (and (equal? (send block3 sblock-x) 260)
                       (equal? (send block3 sblock-y) 250))
                  true
                  "New Teammate not following leader")
    
    (check-equal? (length (send block3 get-team)) 2
                  "block3 failed to add new teammate")
    
    (check-equal? (length (send block2 get-team)) 2
                  "block2 failed to add new teammate")
    
    (check-equal? (length (send block1 get-team)) 2
                  "block1 failed to add new teammate")
    
    ))


