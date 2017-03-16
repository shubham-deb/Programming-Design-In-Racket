;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Goal: To design and implement a system for a graphical interface for trees.
;;       The system allows to create and manipulate trees on a canvas.

;; start with (run 5)

(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "sets.rkt")
(require "extras.rkt")

(check-location "06" "q1.rkt")

(provide initial-world)
(provide run)
(provide world-after-mouse-event)
(provide world-after-key-event)
(provide world-to-trees)
(provide tree-to-root)
(provide tree-to-sons)
(provide node-to-center)
(provide node-to-selected?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAIN FUNCTION.

;; run :  Any -> World
;; GIVEN: any value
;; EFFECT: runs a copy of an initial world
;; RETURNS: the final state of the world.  The given value is ignored.
;; DESIGN STRATEGY: Combine simpler functions
(define (run val)
  (big-bang (initial-world val)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)
            (to-draw world-to-scene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;; canvas dimensions
(define CANVAS-WIDTH 500)         ;; in pixels
(define CANVAS-HEIGHT 400)        ;; in pixels
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; node and line color constants
(define LINE-COLOR "blue")
(define NODE-COLOR "green")

;; node mode constants
(define SELECTED-NODE-MODE "solid")
(define UNSELECTED-NODE-MODE "outline")

;; node dimension constants
(define CIRCLE-RADIUS 20)                      ;; in pixels
(define SQUARE-SIDE (* 2 CIRCLE-RADIUS))       ;; in pixels
(define HALF-SQUARE-SIDE (/ SQUARE-SIDE 2))    ;; in pixels

;; circle node image constants
(define SELECTED-CIRCLE
  (circle CIRCLE-RADIUS SELECTED-NODE-MODE NODE-COLOR))
(define UNSELECTED-CIRCLE
  (circle CIRCLE-RADIUS UNSELECTED-NODE-MODE NODE-COLOR))

;; square node image constants
(define SELECTED-SQUARE (square SQUARE-SIDE SELECTED-NODE-MODE NODE-COLOR))
(define UNSELECTED-SQUARE (square SQUARE-SIDE UNSELECTED-NODE-MODE NODE-COLOR))

;; new node coordinate constants
(define NEW-NODE-X (/ CANVAS-WIDTH 2))                     ;; in pixels
(define NEW-NODE-Y CIRCLE-RADIUS)                          ;; in pixels
(define NEW-SON-NODE-X-COORD-OFFSET (* 3 CIRCLE-RADIUS))   ;; in pixels
(define NEW-SON-NODE-Y-COORD-OFFSET (* 3 CIRCLE-RADIUS))   ;; in pixels

;; node selection constants
(define SELECTED #true)
(define UNSELECTED #false)

;; key event constants
(define C "c")
(define S "s")
(define D "d")

;; mouse event constant
(define BUTTON-DN "button-down")
(define BUTTON-UP "button-up")
(define DRAG "drag")
(define ENTER "enter")

;; node type constants
(define NODE-CIRCLE "circle")
(define NODE-SQUARE "square")

;; world mouse pressed constants
(define PRESSED #true)
(define RELEASED #false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

;; A MouseButton is one of:
;; -- "button-down"
;; -- "button-up"
;; INTERP: represents the mouse button down or button up event.

;; TEMPLATE:
;; mouse-button-fn: MouseButton -> ??
#; (define (mouse-button-fn mb)
     (cond
       [(string=? mb BUTTON-DN) ...]
       [(string=? mb BUTTON-UP) ...]))

;; A NodeType is one of:
;; -- "circle"
;; -- "square"
;; INTERP: represents the way in which the node is rendered.

;; TEMPLATE: 
;; nodetype-fn: NodeType -> ??
#; (define (nodetype-fn nt)
     (cond
       [(string=? nt NODE-CIRCLE) ...]
       [(string=? nt NODE-SQUARE) ...]))

(define-struct node (pos-x pos-y selected? type
                           drag-x drag-y sons))
;; A Node is a (make-node NonNegInt NonNegInt Boolean NodeType
;;                        Int Int ListOfNodes)
;; INTERPRETATION: 
;; pos-x gives the position of x-coordinate of the center of the node, in pixels
;; pos-y gives the position of y-coordinate of the center of the node, in pixels
;; selected? describes whether or not the node is selected
;; type represents the way in which the node is rendered
;; drag-x gives the position of x-coordinate of the drag source, in pixels
;; drag-y gives the position of y-coordinate of the drag source, in pixels
;; sons represents the son (or child) nodes of the node

;; TEMPLATE:
;; node-fn : Node -> ??
#; (define (node-fn n) 
     (... (node-pos-x n)
          (node-pos-y n)
          (node-selected? n)
          (node-type n)
          (node-drag-x n)
          (node-drag-y n)
          (node-sons n)))

;; EXAMPLES:
;; a new circle node, unselected
(define CIRCLE-NEW-NODE
  (make-node NEW-NODE-X NEW-NODE-Y UNSELECTED NODE-CIRCLE 0 0 '()))

;; a new square node, unselected
(define SQUARE-NEW-NODE
  (make-node NEW-NODE-X NEW-NODE-Y UNSELECTED NODE-SQUARE 0 0 '()))

;; a new selected circle node
(define SEL-CIRCLE-NEW-NODE
  (make-node NEW-NODE-X NEW-NODE-Y SELECTED NODE-CIRCLE 0 0 '()))

;; a new selected square node
(define SEL-SQUARE-NEW-NODE
  (make-node NEW-NODE-X NEW-NODE-Y SELECTED NODE-SQUARE 0 0 '()))

;; a square son node, first son, unselected
(define SQUARE-FIRST-SON-NODE
  (make-node NEW-NODE-X (+ NEW-NODE-Y NEW-SON-NODE-Y-COORD-OFFSET)
             UNSELECTED NODE-SQUARE 0 0 '()))

;; a circle son node, first son, selected
(define CIRCLE-FIRST-SON-SEL-NODE
  (make-node NEW-NODE-X (+ NEW-NODE-Y NEW-SON-NODE-Y-COORD-OFFSET)
             SELECTED NODE-CIRCLE 0 0 '()))

;; a circle node, unselected, with one circle selected son
(define CIRCLE-NODE-WITH-SEL-CIRCLE-SON
  (make-node NEW-NODE-X NEW-NODE-Y UNSELECTED NODE-CIRCLE 0 0
             (list CIRCLE-FIRST-SON-SEL-NODE)))

;; a circle node, unselected, with one square unselected son 
(define CIRCLE-NODE-WITH-SQUARE-SON
  (make-node NEW-NODE-X NEW-NODE-Y UNSELECTED NODE-CIRCLE 0 0
             (list SQUARE-FIRST-SON-NODE)))

;; a circle node, unselected, with an unselected circle son
;; which has one circle selected son, therefore, total depth two
(define CIRCLE-NODE-WITH-DEPTH-TWO
  (make-node NEW-NODE-X NEW-NODE-Y UNSELECTED NODE-CIRCLE 0 0
             (list CIRCLE-NODE-WITH-SEL-CIRCLE-SON)))

;; a circle node, unselected, with an unselected new circle son node
(define UNSELECTED-CIRCLE-WITH-DEPTH-ONE
  (make-node NEW-NODE-X NEW-NODE-Y UNSELECTED NODE-CIRCLE 0 0
             (list CIRCLE-NEW-NODE)))

;; a square node, selected, with an unselected first square son node
(define SEL-SQUARE-WITH-SQUARE-NODE
  (make-node NEW-NODE-X NEW-NODE-Y SELECTED NODE-SQUARE 0 0
             (list SQUARE-FIRST-SON-NODE)))

;; a circle node, unselected, with two depth, where, the node
;; at depth one is selected and the leaf node is unselected
(define SEL-CIRCLE-WITH-CIRCLE-NODE-DEPTH-TWO
  (make-node NEW-NODE-X NEW-NODE-Y UNSELECTED NODE-CIRCLE 0 0
             (list (make-node NEW-NODE-X
                              (+ NEW-NODE-Y NEW-SON-NODE-Y-COORD-OFFSET)
                              SELECTED NODE-CIRCLE 0 0
                              (list (make-node NEW-NODE-X 140 UNSELECTED
                                               NODE-CIRCLE 0 0 '()))))))

;; A ListOfNodes (LON) is one of:
;; -- empty
;;    interp: a sequence of Node with no elements
;; -- (cons Node LON)
;;    interp: (cons Node LON) represents a sequence of Node
;;            whose first element is a Node and whose
;;            other elements are represented by a LON.
;; INTERP: is a list of nodes. 

;; TEMPLATE:
;; lon-fn : LON -> ??
;; HALTING MEASURE: (length nodes)
#; (define (lon-fn nodes)
     (cond
       [(empty? nodes) ...]
       [else (... (node-fn (first nodes))
                  (lon-fn (rest nodes)))]))

;; A Tree is a Node.
;; INTERP: self-evident

;; A ListOfTree is a ListOfNode.
;; INTERP: self-evident

;; A ListOfNonNegInt (LONNI) is one of:
;; -- empty
;;    interp: a sequence of NonNegInt with no elements
;; -- (cons NonNegInt LONNI)
;;    interp: (cons NonNegInt LONNI) represents a sequence of NonNegInt
;;            whose first element is a NonNegInt and whose
;;            other elements are represented by a LONNI.
;; INTERP: is a list of NonNegInt.

;; TEMPLATE:
;; lonni-fn : LONNI -> ??
;; HALTING MEASURE: (length loni)
#; (define (lonni-fn loni)
     (cond
       [(empty? loni) ...]
       [else (... (first loni)
                  (lonni-fn (rest loni)))]))

;; EXAMPLES:
(define X-COORDINATES (list 53 104 50 28 230))
(define Y-COORDINATES (list 30 420 25 92 720))

(define-struct world (nodes mx my pressed?))
;; A World is a (make-world ListOfNodes Int Int Boolean)
;; nodes represents the list of nodes in the world
;; mx and my are the x- and y-coordinates of a mouse event, in pixels
;; pressed? describes whether or not the mouse button down or drag is pressed
;; INTERP: represents the state of the world that helps simulate the system

;; TEMPLATE:
;; world-fn : WorldState -> ??
#; (define (world-fn w)
     (... (world-nodes w)
          (world-mx w)
          (world-my w)
          (world-pressed? w)))

;; EXAMPLES:
;; initial world with no nodes
(define INITIAL-WORLD
  (make-world empty 0 0 RELEASED))

;; initial world with a circle node
(define INITIAL-WORLD-WITH-CIRCLE
  (make-world (list CIRCLE-NEW-NODE) 0 0 RELEASED))

;; initial world with a square node
(define INITIAL-WORLD-WITH-SQUARE
  (make-world (list SQUARE-NEW-NODE) 0 0 RELEASED))

;; initial world with a selected square node
(define INITIAL-WORLD-WITH-SEL-SQUARE
  (make-world (list SEL-SQUARE-NEW-NODE) 0 0 RELEASED))

;; initial world with a selected square node
(define INITIAL-WORLD-WITH-MOUSE-DOWN
  (make-world empty 0 0 PRESSED))

;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> World
;; GIVEN: any value
;; RETURNS: an initial world.  The given value is ignored.
;; EXAMPLES:
;; (initial-world 5) = INITIAL-WORLD
;; DESIGN STRATEGY: Use template for World
(define (initial-world val)
  INITIAL-WORLD)
;; TESTS
(begin-for-test
  (check-equal? (initial-world 0) INITIAL-WORLD
                "initial world should have been an initial world with value 0")
  (check-equal? (initial-world 5) INITIAL-WORLD
                "initial world should have been an initial world with value 5"))

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a World, a location (x- and y- coordinates), and a MouseEvent
;; RETURNS: the state of the world as it should be
;; following the given mouse event at that location.
;; EXAMPLES:
;; (world-after-mouse-event INITIAL-WORLD 0 0 BUTTON-DN)
;; = INITIAL-WORLD-WITH-MOUSE-DOWN
;; DESIGN STRATEGY: Use template for World on w
(define (world-after-mouse-event w mx my mev)
  (make-world (nodes-after-mouse-event (world-nodes w) mx my mev)
              (world-mx w)
              (world-my w)
              (world-after-mouse-event-pressed? mev)))
;; TESTS
(begin-for-test
  (check-equal? (world-after-mouse-event INITIAL-WORLD 0 0 BUTTON-DN)
                INITIAL-WORLD-WITH-MOUSE-DOWN
                "initial world after mouse button down event")
  (check-equal? (world-after-mouse-event INITIAL-WORLD-WITH-MOUSE-DOWN 0 0
                                         BUTTON-UP)
                INITIAL-WORLD
                "initial world after mouse button up event"))

;; nodes-after-mouse-event : LON Integer Integer MouseEvent -> LON
;; GIVEN: a LON, a location (x- and y- coordinates), and a MouseEvent
;; RETURNS: the list of nodes as it should be following
;; the given mouse event at that location.
;; EXAMPLES:
;; (nodes-after-mouse-event empty 0 0 BUTTON-DN)  = empty
;; (nodes-after-mouse-event (list SQUARE-NEW-NODE) 0 0 BUTTON-UP)
;; = (list SQUARE-NEW-NODE)
;; HALTING MEASURE: (length nodes)
;; DESIGN STRATEGY: Use HOF map on nodes
(define (nodes-after-mouse-event nodes mx my mev)
  (local
    (;; node/after-mev/not-in-sons : Node MouseEvent -> Node
     ;; GIVEN: a Node and a MouseEvent
     ;; RETURNS: a Node that follows the given mouse event
     ;; DESIGN STRATEGY: Cases on MouseEvent mev for varying mouse events
     (define (node/after-mev/not-in-sons n mev)
       (cond
         [(mouse=? mev BUTTON-DN) (node/after-mouse-button n mx my BUTTON-DN)]
         [(mouse=? mev BUTTON-UP) (node/after-mouse-button n mx my BUTTON-UP)]
         [(mouse=? mev DRAG) (node/after-drag n mx my)]
         [else n]))
     
     ;; node-selected?/after-mev/in-node : Node MouseEvent -> Boolean
     ;; GIVEN: a Node and a MouseEvent
     ;; RETURNS: a Boolean depending on the given MouseEvent mev
     ;; DESIGN STRATEGY: Use template for MouseEvent on mev
     (define (node-selected?/after-mev/in-node n mev)
       (cond 
         [(mouse=? mev BUTTON-DN) SELECTED]
         [(mouse=? mev BUTTON-UP) UNSELECTED]
         [else (node-selected? n)]))
     
     ;; node/after-mev/selected? : Node -> Boolean
     ;; GIVEN: a Node 
     ;; RETURNS: true iff the mouse coordinates are within the node
     ;; DESIGN STRATEGY: Use template for Node on n
     (define (node/after-mev/selected? n)
       (if (in-node? n mx my)
           (node-selected?/after-mev/in-node n mev)
           (node-selected? n)))

     ;; node/after-mev/pos : Boolean NonNegInt Int Int -> NonNegInt
     ;; GIVEN: a Boolean representing whether or not the node is selected
     ;; a x- (or y-) coordinate of a node, the mouse x- (or y-)
     ;; coordinate, and the x- (or y-) of the drag source of the node
     ;; RETURNS: the x- or y- coordinate that follows the mouse drag event
     ;; DESIGN STRATEGY: Cases on selected? depending on whether or not
     ;; the node is selected
     (define (node/after-mev/pos selected? pos mpos dragsrc)
       (if selected? (node-drag-pos pos mpos dragsrc) pos))
     
     ;; node/after-mev/in-any-son : Node LON -> Node
     ;; GIVEN: a Node and a LON that represents the sons of the given Node
     ;; RETURNS: a Node that follows the mouse event
     ;; DESIGN STRATEGY: Use template for Node on n
     (define (node/after-mev/in-any-son n sons)
       (make-node (node/after-mev/pos
                   (node-selected? n) (node-pos-x n) mx (node-drag-x n))
                  (node/after-mev/pos
                   (node-selected? n) (node-pos-y n) my (node-drag-y n))
                  (node/after-mev/selected? n)
                  (node-type n) mx my
                  (nodes-after-mouse-event sons mx my mev)))
     
     ;; node/after-mev : Node LON MouseEvent -> Node
     ;; GIVEN: a Node, a LON that represents the sons of the given Node
     ;; and a MouseEvent
     ;; RETURNS: a Node that follows the given mouse event
     ;; DESIGN STRATEGY: Cases on sons depending on whether or not the mouse
     ;; event occurred in any of the sons of the node at that location
     (define (node/after-mev n sons mev)
       (if (in-any-node? sons mx my)
           (node/after-mev/in-any-son n sons)
           (node/after-mev/not-in-sons n mev))))
    
    (map
     ;; Node -> Node
     ;; GIVEN: a Node
     ;; RETURNS: a Node that follows the mouse event
     ;; DESIGN STRATEGY: Use template for Node on n
     (lambda (n) (node/after-mev n (node-sons n) mev)) nodes)))
;; TESTS
(begin-for-test
  (check-equal? (nodes-after-mouse-event empty 0 0 BUTTON-DN) empty
                "nodes after button down for empty list of nodes")
  (check-equal? (nodes-after-mouse-event
                 (list SQUARE-NEW-NODE) NEW-NODE-X NEW-NODE-Y BUTTON-DN)
                (list (make-node NEW-NODE-X NEW-NODE-Y SELECTED
                                 NODE-SQUARE NEW-NODE-X NEW-NODE-Y '()))
                "nodes after button down for a list with one square node")
  (check-equal? (nodes-after-mouse-event (list SQUARE-NEW-NODE) 0 0 BUTTON-UP)
                (list SQUARE-NEW-NODE)
                "nodes after button up for a list with one square node")
  (check-equal? (nodes-after-mouse-event (list SEL-SQUARE-NEW-NODE) 230 1 DRAG)
                (list (make-node (+ NEW-NODE-X 230) (add1 CIRCLE-RADIUS)
                                 SELECTED NODE-SQUARE 230 1 '()))
                "nodes after drag for a list with one selected square node")
  (check-equal? (nodes-after-mouse-event (list SEL-SQUARE-NEW-NODE) 230 1 ENTER)
                (list SEL-SQUARE-NEW-NODE)
                "nodes after any arbitrary mouse event")
  (check-equal? (nodes-after-mouse-event
                 (list (make-node NEW-NODE-X NEW-NODE-Y
                                  UNSELECTED NODE-SQUARE 0 0
                                  (list (make-node NEW-NODE-X NEW-NODE-Y
                                                   UNSELECTED NODE-SQUARE 0 0
                                                   '()))))
                 NEW-NODE-X NEW-NODE-Y BUTTON-UP)
                (list (make-node NEW-NODE-X NEW-NODE-Y
                                 UNSELECTED NODE-SQUARE
                                 NEW-NODE-X NEW-NODE-Y
                                 (list (make-node NEW-NODE-X NEW-NODE-Y
                                                  UNSELECTED NODE-SQUARE
                                                  NEW-NODE-X NEW-NODE-Y
                                                  '()))))
                "nodes after button up event when son and node overlap")
  (check-equal? (nodes-after-mouse-event
                 (list (make-node NEW-NODE-X NEW-NODE-Y
                                  UNSELECTED NODE-SQUARE 0 0
                                  (list (make-node NEW-NODE-X NEW-NODE-Y
                                                   UNSELECTED NODE-SQUARE 0 0
                                                   '()))))
                 NEW-NODE-X NEW-NODE-Y BUTTON-DN)
                (list (make-node NEW-NODE-X NEW-NODE-Y
                                 SELECTED NODE-SQUARE
                                 NEW-NODE-X NEW-NODE-Y
                                 (list (make-node NEW-NODE-X NEW-NODE-Y
                                                  SELECTED NODE-SQUARE
                                                  NEW-NODE-X NEW-NODE-Y
                                                  '()))))
                "nodes after button down event when son and node overlap")
  (check-equal? (nodes-after-mouse-event
                 (list (make-node NEW-NODE-X NEW-NODE-Y
                                  SELECTED NODE-SQUARE 0 0
                                  (list (make-node NEW-NODE-X NEW-NODE-Y
                                                   SELECTED NODE-SQUARE 0 0
                                                   '()))))
                 NEW-NODE-X NEW-NODE-Y ENTER)
                (list (make-node (* 2 NEW-NODE-X) (* 2 NEW-NODE-Y)
                                 SELECTED NODE-SQUARE
                                 NEW-NODE-X NEW-NODE-Y
                                 (list (make-node NEW-NODE-X NEW-NODE-Y
                                                  SELECTED NODE-SQUARE 0 0
                                                  '()))))
                "nodes after any arbitrary event when son and node overlap")
  (check-equal? (nodes-after-mouse-event
                 (list CIRCLE-NODE-WITH-SEL-CIRCLE-SON)
                 NEW-NODE-X (+ NEW-NODE-Y NEW-SON-NODE-Y-COORD-OFFSET)
                 BUTTON-UP)
                (list
                 (make-node NEW-NODE-X
                            NEW-NODE-Y UNSELECTED NODE-CIRCLE
                            NEW-NODE-X
                            (+ NEW-NODE-Y NEW-SON-NODE-Y-COORD-OFFSET)
                            (list (make-node
                                   NEW-NODE-X
                                   (+ NEW-NODE-Y NEW-SON-NODE-Y-COORD-OFFSET)
                                   UNSELECTED NODE-CIRCLE
                                   NEW-NODE-X
                                   (+ NEW-NODE-Y NEW-SON-NODE-Y-COORD-OFFSET)
                                   '()))))
                "nodes after button up, son of a circle node selected"))

;; node/after-mouse-button : Node Integer Integer MouseButton -> Node
;; GIVEN: a Node, a location (x- and y- coordinates), and a MouseButton
;; RETURNS: the Node that follows the given mouse button at that location.
;; EXAMPLES:
;; (node/after-mouse-button SEL-SQUARE-NEW-NODE 240 15 BUTTON-UP)
;; = (make-node NEW-NODE-X NEW-NODE-Y UNSELECTED NODE-SQUARE 240 15 '())
;; DESIGN STRATEGY: Cases on whether or not the mouse 
;; button occurred in the node at that location
(define (node/after-mouse-button n mx my mb)
  (local
    (;; node-selected?/after-mouse-button : MouseButton -> Boolean
     ;; GIVEN: a MouseButton
     ;; RETURNS: the updated node-selected? depending on whether or not
     ;; the mouse button is a button down or a button up
     ;; DESIGN STRATEGY: Use template for MouseButton on mb
     (define (node-selected?/after-mouse-button mb)
       (cond
         [(string=? mb BUTTON-DN) SELECTED]
         [(string=? mb BUTTON-UP) UNSELECTED]))
     
     ;; node/after-mouse-button/in-node : Node -> Node
     ;; GIVEN: a Node
     ;; RETURNS: a Node that follows when the mouse button
     ;; occurred in the node at the given location
     ;; DESIGN STRATEGY: Use template for Node on n
     (define (node/after-mouse-button/in-node n)
       (make-node (node-pos-x n) (node-pos-y n)
                  (node-selected?/after-mouse-button mb)
                  (node-type n) mx my (node-sons n))))
    
    (if (in-node? n mx my) (node/after-mouse-button/in-node n) n)))
;; TESTS
(begin-for-test
  (check-equal? (node/after-mouse-button SEL-SQUARE-NEW-NODE
                                         NEW-NODE-X NEW-NODE-Y BUTTON-UP)
                (make-node NEW-NODE-X NEW-NODE-Y UNSELECTED
                           NODE-SQUARE NEW-NODE-X NEW-NODE-Y '())
                "new square node after button-up")
  (check-equal? (node/after-mouse-button SQUARE-NEW-NODE
                                         NEW-NODE-X NEW-NODE-Y BUTTON-DN)
                (make-node NEW-NODE-X NEW-NODE-Y SELECTED
                           NODE-SQUARE NEW-NODE-X NEW-NODE-Y '())
                "new square node after button-down"))

;; node-drag-pos : NonNegInt Int Int -> NonNegInt
;; GIVEN: a x- (or y-) coordinate of a node, the mouse x- (or y-)
;; coordinate, and the x- (or y-) of the drag source of the node
;; RETURNS: an updated x- (or y-) coordinate of the node after drag
;; EXAMPLES:
;; (node-drag-pos 50 100 40) = 110
;; (node-drag-pos 40 20 80) = -20
;; DESIGN STRATEGY: Combine simpler functions
(define (node-drag-pos pos mpos dragsrc)
  (+ pos (- mpos dragsrc)))
;; TESTS
(begin-for-test
  (check-equal? (node-drag-pos 50 100 40) 110
                "get node drag pos x-coordinate")
  (check-equal? (node-drag-pos 40 20 80) -20
                "get node drag pos y-coordinate"))

;; node/after-drag : LON Integer Integer -> LON
;; GIVEN: a LON, a location (x- and y- coordinates) that follows
;; the mouse drag event at that location.
;; EXAMPLES:
;; (node/after-drag CIRCLE-NEW-NODE CANVAS-WIDTH CANVAS-HEIGHT)
;; = CIRCLE-NEW-NODE
;; DESIGN STRATEGY: Cases on whether or not the mouse 
;; drag occurred when the node is selected at that location
(define (node/after-drag n mx my)
  (local
    (;; node-drag-sons : LON Int Int Int Int -> LON
     ;; GIVEN: a LON that represents the sons of a Node, 
     ;; (x- and y- coordinates) of the parent Node
     ;; (x- and y- coordinates) of the drag source of the parent Node
     ;; RETURNS: a LON that follows the mouse drag event
     ;; for all the given sons with respect to the parent Node
     ;; HALTING MEASURE: (length sons)
     ;; DESIGN STRATEGY: Use HOF map on sons
     (define (node-drag-sons sons px py ndx ndy)
       (map
        ;; Node -> Node
        ;; GIVEN: a Node
        ;; RETURNS: a Node that follows the mouse drag event
        ;; with respect to the parent Node
        ;; DESIGN STRATEGY: Combine simpler functions
        (lambda (son) (selected-node/with-drag-pos son ndx ndy px py)) sons))
     
     ;; selected-node/with-drag-pos : Node Int Int Int Int -> Node
     ;; GIVEN: a Node, a mouse location (x- and y- coordinates)
     ;; and a drag source location (x- and y- coordinates)
     ;; RETURNS: a Node that follows the mouse drag event to the mouse locations
     ;; based on the node's drag source location.
     ;; DESIGN STRATEGY: Use template for Node on n
     (define (selected-node/with-drag-pos n mx my ndx ndy)
       (local
         (;; x- and y- coordinates of the node
          (define NODE-X (node-pos-x n))
          (define NODE-Y (node-pos-y n))
          
          ;; x- and y- coordinates of the node after the drag event
          (define NODE-X-AFTER-DRAG (node-drag-pos NODE-X mx ndx))
          (define NODE-Y-AFTER-DRAG (node-drag-pos NODE-Y my ndy)))
         
         (make-node NODE-X-AFTER-DRAG
                    NODE-Y-AFTER-DRAG
                    (node-selected? n) (node-type n) mx my
                    (node-drag-sons (node-sons n)
                                    NODE-X
                                    NODE-Y
                                    NODE-X-AFTER-DRAG
                                    NODE-Y-AFTER-DRAG))))
     
     ;; selected-node/after-drag : Node -> Node
     ;; GIVEN: a Node
     ;; RETURNS: a Node that follows after the mouse drag event at that location
     ;; DESIGN STRATEGY: Use template for Node on n
     (define (selected-node/after-drag n)
       (selected-node/with-drag-pos n mx my (node-drag-x n) (node-drag-y n))))
    
    (if (node-selected? n) (selected-node/after-drag n) n)))
;; TESTS
(begin-for-test
  (check-equal? (node/after-drag CIRCLE-NEW-NODE CANVAS-WIDTH CANVAS-HEIGHT)
                CIRCLE-NEW-NODE "new circle node not dragged")
  (check-equal? (node/after-drag SEL-SQUARE-WITH-SQUARE-NODE 20 20)
                (make-node (+ NEW-NODE-X (- 20 0))
                           (+ NEW-NODE-Y (- 20 0))
                           SELECTED NODE-SQUARE 20 20
                           (list
                            (make-node
                             (+ NEW-NODE-X (- 20 0))
                             (+ (+ NEW-NODE-Y NEW-SON-NODE-Y-COORD-OFFSET)
                                (- 20 0))
                             UNSELECTED NODE-SQUARE
                             (+ NEW-NODE-X (- 20 0))
                             (+ NEW-NODE-Y (- 20 0))
                             empty)))
                "selected square node with square node dragged by (20, 20)"))

;; in-any-node? : LON Integer Integer -> Boolean
;; GIVEN: a LON, the x- and y- coordinates of the mouse
;; RETURNS: true iff the given x- and y- coordinates are within any of the nodes
;; EXAMPLES:
;; (in-any-node? '() CANVAS-WIDTH CANVAS-HEIGHT) = #false
;; HALTING MEASURE: (length lon)
;; DESIGN STRATEGY: Use HOF ormap on lon
(define (in-any-node? lon mx my)
  (ormap
   ;; Node -> Boolean
   ;; GIVEN: a Node
   ;; RETURNS: true iff the given mouse coordinates are within the node
   ;; DESIGN STRATEGY: Use template for Node on n
   (lambda (n) (or (in-node? n mx my) (in-any-node? (node-sons n) mx my))) lon))
;; TESTS
(begin-for-test
  (check-equal? (in-any-node? '() CANVAS-WIDTH CANVAS-HEIGHT) #false
                "mouse coordinates not in an empty list of nodes")
  (check-equal? (in-any-node? (list CIRCLE-NEW-NODE) 230 30)
                (in-node? CIRCLE-NEW-NODE 230 30)
                "mouse coordinates not in the given list of nodes"))

;; in-node? : Node Integer Integer -> Boolean
;; GIVEN: a Node, the x- and y- coordinates of the mouse
;; RETURNS: true iff the given mouse coordinates are within the given node
;; EXAMPLES:
;; (in-node? SQUARE-NEW-NODE NEW-NODE-X NEW-NODE-Y) = #true
;; (in-node? CIRCLE-NEW-NODE CANVAS-WIDTH CANVAS-HEIGHT) = #false
;; DESIGN STRATEGY: Use template for Node on n
(define (in-node? n mx my)
  (local
    (;; distance-to-node-center : NonNegInt NonNegInt Int Int -> Number
     ;; GIVEN: the x- and y- coordinates of the center of the node and the
     ;; x- and y- coordinates of the mouse
     ;; RETURNS: the distance of the center of the node to the coordinates
     ;; of the mouse 
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (distance-to-node-center cx cy mx my)
       (sqrt (+ (sqr (- mx cx)) (sqr (- my cy)))))
     
     ;; in-circle? : NonNegInt NonNegInt Int Int -> Boolean
     ;; GIVEN: x- and y- coordinates of a Node and the
     ;; x- and y- coordinates of the mouse
     ;; RETURNS: true iff the given mouse coordinates are inside the circle
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (in-circle? x y mx my)
       (<= (distance-to-node-center x y mx my) CIRCLE-RADIUS))
     
     ;; in-square? : NonNegInt NonNegInt Int Int -> Boolean
     ;; GIVEN: x- and y- coordinates of a Node and the
     ;; x- and y- coordinates of the mouse
     ;; RETURNS: true iff the given mouse coordinates are inside the square
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (in-square? x y mx my)
       (and (<= (- x HALF-SQUARE-SIDE) mx (+ x HALF-SQUARE-SIDE))
            (<= (- y HALF-SQUARE-SIDE) my (+ y HALF-SQUARE-SIDE))))
     
     ;; in-node?/type : NonNegInt NonNegInt NodeType -> Boolean
     ;; GIVEN: x- and y- coordinates of a Node, and a NodeType
     ;; RETURNS: true iff the mouse coordinates fall within
     ;; the coordinate of the node specified by nt
     ;; DESIGN STRATEGY: Use template for NodeType on nt
     (define (in-node?/type x y nt)
       (cond
         [(string=? nt NODE-CIRCLE) (in-circle? x y mx my)]
         [(string=? nt NODE-SQUARE) (in-square? x y mx my)])))
    
    (in-node?/type (node-pos-x n) (node-pos-y n) (node-type n))))
;; TESTS
(begin-for-test
  (check-equal? (in-node? CIRCLE-NEW-NODE CANVAS-WIDTH CANVAS-HEIGHT) #false
                "mouse coordinates not in the given circle node")
  (check-equal? (in-node? SQUARE-NEW-NODE NEW-NODE-X NEW-NODE-Y) #true
                "mouse coordinates in the given square node"))

;; world-after-mouse-event-pressed? : MouseEvent -> Boolean
;; GIVEN: a MouseEvent
;; RETURNS: whether or not the mouse was pressed
;; EXAMPLES:
;; (world-after-mouse-event-pressed? BUTTON-DN) = #true
;; (world-after-mouse-event-pressed? BUTTON-UP) = #false
;; DESIGN STRATEGY: Cases on MouseEvent mev
(define (world-after-mouse-event-pressed? mev)
  (cond
    [(mouse=? mev BUTTON-DN) PRESSED]
    [(mouse=? mev BUTTON-UP) RELEASED]
    [(mouse=? mev DRAG) PRESSED]
    [else RELEASED]))
;; TESTS
(begin-for-test
  (check-equal? (world-after-mouse-event-pressed? BUTTON-DN) #true
                "mouse event pressed, button-down")
  (check-equal? (world-after-mouse-event-pressed? BUTTON-UP) #false
                "mouse event not pressed, button-down")
  (check-equal? (world-after-mouse-event-pressed? DRAG) #true
                "mouse event pressed, drag")
  (check-equal? (world-after-mouse-event-pressed? ENTER) #false
                "mouse event not pressed, enter"))

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a World and a key event
;; RETURNS: the state of the world as it should be following the given key event
;; EXAMPLES:
;; (world-after-key-event INITIAL-WORLD C) = INITIAL-WORLD-WITH-CIRCLE
;; (world-after-key-event INITIAL-WORLD-WITH-SEL-SQUARE D) = INITIAL-WORLD
;; DESIGN STRATEGY: Use template for World on w
(define (world-after-key-event w ke)
  (make-world (nodes-after-key-event (world-nodes w) ke)
              (world-mx w)
              (world-my w)
              (world-pressed? w)))
;; TESTS
(begin-for-test
  (check-equal? (world-after-key-event INITIAL-WORLD C)
                INITIAL-WORLD-WITH-CIRCLE "initial world after 'c' is pressed")
  (check-equal? (world-after-key-event INITIAL-WORLD S)
                INITIAL-WORLD-WITH-SQUARE "initial world after 's' is pressed")
  (check-equal? (world-after-key-event INITIAL-WORLD-WITH-SEL-SQUARE D)
                INITIAL-WORLD "initial world with square after 'd' is pressed")
  (check-equal? (world-after-key-event INITIAL-WORLD "a")
                INITIAL-WORLD "initial world after arbitrary key is pressed"))

;; nodes-after-key-event : LON KeyEvent -> LON
;; GIVEN: a LON and a KeyEvent
;; RETURNS: a LON similar to the given one but updated
;; depending on the given KeyEvent ke
;; EXAMPLES:
;; (nodes-after-key-event empty C) = (list CIRCLE-NEW-NODE)
;; (nodes-after-key-event (list CIRCLE-NEW-NODE) S)
;; = (list SQUARE-NEW-NODE CIRCLE-NEW-NODE)
;; DESIGN STRATEGY: Cases on KeyEvent ke for varying key events
(define (nodes-after-key-event nodes ke)
  (cond
    [(key=? ke C) (update-world-nodes nodes NODE-CIRCLE)]
    [(key=? ke S) (update-world-nodes nodes NODE-SQUARE)]
    [(key=? ke D) (delete-nodes nodes)]
    [else nodes]))
;; TESTS
(begin-for-test
  (check-equal? (nodes-after-key-event empty C) (list CIRCLE-NEW-NODE)
                "circle nodes after 'c' key event")
  (check-equal? (nodes-after-key-event (list CIRCLE-NEW-NODE) S)
                (list SQUARE-NEW-NODE CIRCLE-NEW-NODE)
                "square node after 's' key event"))

;; update-world-nodes : LON NodeType -> LON
;; GIVEN: a LON and a NodeType
;; RETURNS: a LON similar to the given one but with a new Node of NodeType nt
;; EXAMPLES:
;; (update-world-nodes (list SEL-SQUARE-NEW-NODE) NODE-SQUARE)
;; = (list SEL-SQUARE-WITH-SQUARE-NODE)
;; DESIGN STRATEGY: Cases on nodes depending on whether or not
;; any of the world nodes are selected
(define (update-world-nodes nodes nt)
  (if (any-node-selected? nodes)
      (add-son-to-selected-nodes nodes nt)
      (add-new-node-to-world nodes nt)))
;; TESTS
(begin-for-test
  (check-equal? (update-world-nodes (list SEL-SQUARE-NEW-NODE) NODE-SQUARE)
                (list SEL-SQUARE-WITH-SQUARE-NODE)
                "updated the list of nodes by adding a square node")
  (check-equal? (update-world-nodes '() NODE-CIRCLE)
                (list CIRCLE-NEW-NODE)
                "add new circle node to empty list of nodes"))

;; add-new-node-to-world : LON NodeType -> LON
;; GIVEN: a LON and a NodeType
;; RETURNS: a LON similar to the given one but with a new node of NodeType nt
;; EXAMPLES:
;; (add-new-node-to-world '() NODE-CIRCLE) = (list CIRCLE-NEW-NODE)
;; (add-new-node-to-world '() NODE-SQUARE) = (list SQUARE-NEW-NODE)
;; DESIGN STRATEGY: Combine simpler functions
(define (add-new-node-to-world nodes nt)
  (local
    (;; new-node/type : NodeType -> Node
     ;; GIVEN: a NodeType
     ;; RETURNS: a new Node depending on the given NodeType nt
     ;; DESIGN STRATEGY: Use template for NodeType on nt
     (define (new-node/type nt)
       (cond
         [(string=? nt NODE-CIRCLE) CIRCLE-NEW-NODE]
         [(string=? nt NODE-SQUARE) SQUARE-NEW-NODE])))
    
    (cons (new-node/type nt) nodes)))
;; TESTS
(begin-for-test
  (check-equal? (add-new-node-to-world '() NODE-CIRCLE) (list CIRCLE-NEW-NODE)
                "add new circle node to world nodes")
  (check-equal? (add-new-node-to-world '() NODE-SQUARE) (list SQUARE-NEW-NODE)
                "add new circle node to world nodes"))

;; add-son-to-selected-nodes : LON NodeType -> LON
;; GIVEN: a LON and a NodeType
;; RETURNS: a LON similar to the given one but with
;; a new son of NodeType nt added to the selected nodes
;; EXAMPLES:
;; (add-son-to-selected-nodes
;;    (list UNSELECTED-CIRCLE-WITH-DEPTH-TWO) NODE-CIRCLE)
;; = (list UNSELECTED-CIRCLE-WITH-DEPTH-TWO)
;; HALTING MEASURE: (length nodes)
;; DESIGN STRATEGY: Use HOF map on nodes
(define (add-son-to-selected-nodes nodes nt)
  (local
    (;; add-son-to-selected-node : Node -> Node
     ;; GIVEN: a Node
     ;; RETURNS: a Node with a new son
     ;; DESIGN STRATEGY: Use template for Node on n
     (define (add-son-to-selected-node n)
       (make-node (node-pos-x n) (node-pos-y n)
                  (node-selected? n) (node-type n)
                  (node-drag-x n) (node-drag-y n)
                  (add-son (node-pos-x n) (node-pos-y n)
                           (node-sons n) n nt)))
     
     ;; update-node : Node -> Node
     ;; GIVEN: a Node
     ;; RETURNS: a Node similar to the given one but with a son
     ;; added to it iff the node is selected
     ;; DESIGN STRATEGY: Use template for Node on n
     (define (update-node n)
       (if (node-selected? n) (add-son-to-selected-node n) n))
     
     ;; update-sons : Node -> Node
     ;; GIVEN: a Node
     ;; RETURNS: a Node similar to the given one but with a new son
     ;; DESIGN STRATEGY: Use template for Node on n
     (define (update-sons n)
       (make-node (node-pos-x n) (node-pos-y n)
                  (node-selected? n) (node-type n)
                  (node-drag-x n) (node-drag-y n)
                  (add-son-to-selected-nodes (node-sons n) nt)))
     
     ;; update-sons-of-node : Node -> Node
     ;; GIVEN: a Node
     ;; RETURNS: a Node whose sons are updated with a new node of type nt
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (update-sons-of-node n)
       (update-sons (update-node n))))
    
    (map
     ;; Node -> Node
     ;; GIVEN: a Node
     ;; RETURNS: a Node whose sons are updated with a new node of type nt
     ;; DESIGN STRATEGY: Combine simpler functions
     (lambda (n) (update-sons-of-node n)) nodes)))
;; TESTS
(begin-for-test
  (check-equal? (add-son-to-selected-nodes
                 (list SEL-SQUARE-NEW-NODE) NODE-SQUARE)
                (list SEL-SQUARE-WITH-SQUARE-NODE)
                "add square node as son to list having selected square node")
  (check-equal? (add-son-to-selected-nodes
                 (list CIRCLE-NODE-WITH-SEL-CIRCLE-SON) NODE-CIRCLE)
                (list (make-node
                       NEW-NODE-X NEW-NODE-Y UNSELECTED NODE-CIRCLE 0 0
                       (list (make-node
                              NEW-NODE-X
                              (+ NEW-NODE-Y
                                 NEW-SON-NODE-Y-COORD-OFFSET)
                              SELECTED NODE-CIRCLE 0 0
                              (list (make-node
                                     NEW-NODE-X
                                     (+ (+ NEW-NODE-Y
                                           NEW-SON-NODE-Y-COORD-OFFSET)
                                        NEW-SON-NODE-Y-COORD-OFFSET)
                                     UNSELECTED
                                     NODE-CIRCLE 0 0 '()))))))
                "add circle node as son to a selected circle node")
  (check-equal? (add-son-to-selected-nodes
                 (list UNSELECTED-CIRCLE-WITH-DEPTH-ONE) NODE-CIRCLE)
                (list UNSELECTED-CIRCLE-WITH-DEPTH-ONE)
                "no node is selected, no son added"))

;; any-node-selected? : LON -> Boolean
;; GIVEN: a LON
;; RETURNS: true iff any of the given nodes is selected
;; EXAMPLES:
;; (any-node-selected? '()) = #false
;; (any-node-selected? (list SEL-SQUARE-NEW-NODE)) = #true
;; HALTING MEASURE: (length nodes)
;; DESIGN STRATEGY: Use HOF ormap on nodes
(define (any-node-selected? nodes)
  (ormap
   ;; Node -> Boolean
   ;; GIVEN: a Node
   ;; RETURNS: true iff the node or any of its sons are selected
   ;; DESIGN STRATEGY: Use template for Node on n
   (lambda (n) (or (node-selected? n)
                   (any-node-selected? (node-sons n)))) nodes))
;; TESTS
(begin-for-test
  (check-equal? (any-node-selected? '()) #false
                "no nodes selected in an empty list of nodes")
  (check-equal? (any-node-selected? (list CIRCLE-NEW-NODE)) #false
                "no nodes selected in the given list of nodes")
  (check-equal? (any-node-selected? (list SEL-SQUARE-NEW-NODE)) #true
                "one node selected in the given list of nodes"))

;; add-son : NonNegInt NonNegInt LON Node NodeType -> LON
;; GIVEN: x- and y- coordinates of a Node n, a LON that represents
;; the sons of the Node n, a Node n and the type of the Node n
;; RETURNS: a LON that contains a new son Node added to the given sons
;; EXAMPLES:
;; (add-son NEW-NODE-X NEW-NODE-Y '() CIRCLE-NEW-NODE NODE-SQUARE)
;; = (list SQUARE-FIRST-SON-NODE)
;; DESIGN STRATEGY: Use template for LON on sons
(define (add-son x y sons n nt)
  (local
    (;; add-first-son : NonNegInt NonNegInt Node NodeType -> LON
     ;; GIVEN: x- and y- coordinates, a Node n and the type of Node n
     ;; RETURNS: a LON with a new Node of type nt added to sons of Node n
     ;; DESIGN STRATEGY: Use template for Node
     (define (add-first-son x y n nt)
       (list (make-node x (update-pos-y y) UNSELECTED nt 0 0 empty)))
     
     ;; add-other-sons : NonNegInt NonNegInt Node NodeType LON -> LON
     ;; GIVEN: x- and y- coordinates, a Node n and the type of Node n,
     ;; and the a LON that represents the sons of Node n
     ;; RETURNS: a LON with a new Node of type nt added to sons of Node n
     ;; DESIGN STRATEGY: Use template for Node
     (define (add-other-sons x y n nt sons)
       (cons (make-node (update-pos-x x sons)
                        (update-pos-y y) UNSELECTED nt 0 0 empty)
             sons)))
    
    (cond
      [(empty? sons) (add-first-son x y n nt)]
      [else (add-other-sons x y n nt sons)])))
;; TESTS
(begin-for-test
  (check-equal? (add-son NEW-NODE-X NEW-NODE-Y '() CIRCLE-NEW-NODE NODE-SQUARE)
                (list SQUARE-FIRST-SON-NODE)
                "add first square son to a circle node")
  (check-equal? (add-son NEW-NODE-X NEW-NODE-Y (list SQUARE-NEW-NODE)
                         SQUARE-NEW-NODE NODE-SQUARE)
                (list (make-node
                       (update-pos-x NEW-NODE-X (list SQUARE-NEW-NODE))
                       (update-pos-y NEW-NODE-Y) UNSELECTED NODE-SQUARE 0 0 '())
                      SQUARE-NEW-NODE)
                "add a square son to a list of nodes that has a square node"))

;; update-pos-x : NonNegInt LON -> NonNegInt
;; GIVEN: a x-coordinate and a LON
;; RETURNS: an updated x- coordinate for the sons of a node
;; EXAMPLES:
;; (update-pos-x NEW-NODE-X (list CIRCLE-NEW-NODE))
;; = (- NEW-NODE-X NEW-SON-NODE-X-COORD-OFFSET)
;; HALTING MEASURE: (length sons)
;; DESIGN STRATEGY: Use HOF foldr on (x-pos-of-sons sons)
(define (update-pos-x x sons)
  (local
    (;; x-pos-of-sons : LON -> LONNI
     ;; GIVEN: a LON
     ;; RETURNS: a list of non-negative integers that represent the
     ;; x-coordinates of each of the sons
     ;; HALTING MEASURE: (length sons)
     ;; DESIGN STRATEGY: Use HOF map on sons
     (define (x-pos-of-sons sons)
       (map
        ;; Node -> NonNegInt
        ;; GIVEN: a Node
        ;; RETURNS: the x-coordinate of the node
        ;; DESIGN STRATEGY: Use template for Node on n
        (lambda (n) (node-pos-x n)) sons)))
    
    (- (foldr min CANVAS-WIDTH (x-pos-of-sons sons))
       NEW-SON-NODE-X-COORD-OFFSET)))
;; TESTS
(begin-for-test
  (check-equal? (update-pos-x NEW-NODE-X (list CIRCLE-NEW-NODE))
                (- NEW-NODE-X NEW-SON-NODE-X-COORD-OFFSET)
                "updated x-coordinate of the new circle node"))

;; update-pos-y : NonNegInt -> NonNegInt
;; GIVEN: a y-coordinate
;; RETURNS: an updated y-coordinate
;; EXAMPLES:
;; (update-pos-y NEW-NODE-Y) = 80
;; DESIGN STRATEGY: Combine simpler functions
(define (update-pos-y y)
  (+ y NEW-SON-NODE-Y-COORD-OFFSET))
;; TESTS
(begin-for-test
  (check-equal? (update-pos-y NEW-NODE-Y)
                (+ NEW-NODE-Y NEW-SON-NODE-Y-COORD-OFFSET)
                "updated y-coordinate of the given y-coordinate node"))

;; delete-nodes : LON -> LON
;; GIVEN: a LON
;; RETURNS: a LON with the selected nodes deleted
;; EXAMPLES:
;; (delete-nodes (list CIRCLE-NODE-WITH-SEL-CIRCLE-SON))
;; = (list CIRCLE-NEW-NODE)
;; DESIGN STRATEGY: Cases on nodes depending on whether or not
;; any of the given world nodes is selected
(define (delete-nodes nodes)
  (local
    (;; world-nodes-selected? : LON -> Boolean
     ;; GIVEN: a LON
     ;; RETURNS: true iff any of the given nodes is selected
     ;; HALTING MEASURE: (length nodes)
     ;; DESIGN STRATEGY: Use HOF ormap on nodes
     (define (world-nodes-selected? nodes)
       (ormap
        ;; Node -> Boolean
        ;; GIVEN: a Node
        ;; RETURNS: true iff the given node is selected
        ;; DESIGN STRATEGY: Use template for Node on n
        (lambda (n) (node-selected? n)) nodes)))
    
    (if (world-nodes-selected? nodes)
        (delete-world-nodes nodes)
        (delete-nodes/node-with-parent nodes))))
;; TESTS
(begin-for-test
  (check-equal? (delete-nodes (list CIRCLE-NODE-WITH-SEL-CIRCLE-SON))
                (list CIRCLE-NEW-NODE)
                "delete the selected circle son node of a circle node"))

;; delete-world-nodes : LON -> LON
;; GIVEN: a LON
;; RETURNS: a LON with the selected nodes deleted
;; such that their sons become world nodes
;; EXAMPLES:
;; (delete-world-nodes (list SEL-CIRCLE-NEW-NODE SQUARE-NEW-NODE))
;; = (list SQUARE-NEW-NODE)
;; HALTING MEASURE: (length (selected-nodes/sons nodes))
;; DESIGN STRATEGY: Use HOF foldr on (selected-nodes/sons nodes)
(define (delete-world-nodes nodes)
  (local
    (;; selected-node/sons : Node -> LON
     ;; GIVEN: a Node
     ;; RETURNS: a LON that represents either the node sons or
     ;; the node itself as a list depending on whether or not
     ;; the node is selected
     ;; DESIGN STRATEGY: Use template for Node on n
     (define (selected-node/sons n)
       (if (node-selected? n) (node-sons n) (list n)))
     
     ;; selected-nodes/sons : LON -> LON
     ;; GIVEN: a LON
     ;; RETURNS: a LON that represents the nodes
     ;; after deleting the selected node
     ;; HALTING MEASURE: (length nodes)
     ;; DESIGN STRATEGY: Use HOF foldr on nodes
     (define (selected-nodes/sons nodes)
       (foldr
        ;; Node LON -> LON
        ;; GIVEN: a Node and a LON
        ;; RETURNS: a LON that represents the nodes
        ;; after deleting the selected node
        ;; DESIGN STRATEGY: Combine simpler functions
        (lambda (n lst) (append (selected-node/sons n) lst)) '() nodes)))
    
    (foldr
     ;; Node LON -> LON
     ;; GIVEN: a Node and a LON
     ;; RETURNS: a LON after deletion of selected world nodes
     ;; DESIGN STRATEGY: Combine simpler functions
     (lambda (n lst) (append (list n) lst)) '() (selected-nodes/sons nodes))))
;; TESTS
(begin-for-test
  (check-equal? (delete-world-nodes empty) empty
                "delete no nodes from an empty list of nodes")
  (check-equal? (delete-world-nodes (list SEL-CIRCLE-NEW-NODE SQUARE-NEW-NODE))
                (list SQUARE-NEW-NODE)
                "delete two nodes from a list of nodes"))

;; delete-nodes/node-with-parent : LON -> LON
;; GIVEN: a LON
;; RETURNS: a LON after deletion of the selected node that has a parent
;; EXAMPLES:
;; (delete-nodes/node-with-parent (list CIRCLE-NODE-WITH-SEL-CIRCLE-SON))
;; = (list CIRCLE-NEW-NODE)
;; HALTING MEASURE: (length nodes)
;; DESIGN STRATEGY: Use HOF map on nodes
(define (delete-nodes/node-with-parent nodes)
  (local
    (;; delete-node/with-parent : Node -> Node
     ;; GIVEN: a Node
     ;; RETURNS: a Node after the selected son is deleted
     ;; DESIGN STRATEGY: Use template for Node on n
     (define (delete-node/with-parent n)
       (make-node (node-pos-x n) (node-pos-y n)
                  (node-selected? n) (node-type n)
                  (node-drag-x n) (node-drag-y n)
                  (delete-sons (node-sons n))))
    
    ;; delete-sons : LON -> LON
    ;; GIVEN: a LON
    ;; RETURNS: a LON where the selected son is deleted
    ;; HALTING MEASURE: (length lon)
    ;; DESIGN STRATEGY: Use HOF foldr on lon
    (define (delete-sons lon)
      (foldr
       ;; Node LON -> LON
       ;; GIVEN: a Node and a LON
       ;; RETURNS: a LON after deletion of selected node
       ;; DESIGN STRATEGY: Combine simpler functions
       (lambda (n lst) (append (delete-node n) lst)) '() lon))
    
    ;; delete-node : Node -> LON
    ;; GIVEN: a Node
    ;; RETURNS: a LON that represents either the node sons or
    ;; the node as a list after deletion depending on whether or not
    ;; the node is selected
    ;; DESIGN STRATEGY: Use template for Node on n
    (define (delete-node n)
      (if (node-selected? n)
          (delete-sons (node-sons n))
          (list (delete-node/with-parent n)))))
    
    (foldr
     ;; Node LON -> LON
     ;; GIVEN: a Node and a LON
     ;; RETURNS: a LON after deletion of selected node
     ;; DESIGN STRATEGY: Combine simpler functions
     (lambda (n lst) (append (delete-node n) lst)) '() nodes)))
;; TESTS
(begin-for-test
  (check-equal? (delete-nodes/node-with-parent
                 (list CIRCLE-NODE-WITH-SEL-CIRCLE-SON))
                (list CIRCLE-NEW-NODE)
                "delete the selected circle son node of a circle node")
  (check-equal? (delete-nodes/node-with-parent
                 (list CIRCLE-NODE-WITH-DEPTH-TWO))
                (list UNSELECTED-CIRCLE-WITH-DEPTH-ONE)
                "delete the last leaf node of a circle node with depth two"))

;; world-to-scene : World -> Scene
;; GIVEN: a World
;; RETURNS: a Scene that portrays the given world
;; depending on whether or not the mouse button down or drag is pressed
;; EXAMPLES:
;; (world-to-scene INITIAL-WORLD) = EMPTY-CANVAS
;; DESIGN STRATEGY: Use template for World on w
(define (world-to-scene w)
  (nodes-to-scene (world-nodes w) EMPTY-CANVAS))
;; TESTS
(begin-for-test
  (check-equal? (world-to-scene INITIAL-WORLD) EMPTY-CANVAS
                "render initial world as empty canvas")
  (check-equal? (world-to-scene INITIAL-WORLD-WITH-CIRCLE)
                (render-shape UNSELECTED-CIRCLE CIRCLE-NEW-NODE EMPTY-CANVAS)
                "render initial world with new circle on an empty canvas"))

;; nodes-to-scene : LON Scene -> Scene
;; GIVEN: a LON and a Scene
;; RETURNS: a Scene that portrays the given LON on the given Scene
;; EXAMPLES:
;; (nodes-to-scene '() EMPTY-CANVAS) = EMPTY-CANVAS
;; (nodes-to-scene (list CIRCLE-NEW-NODE) EMPTY-CANVAS)
;; = (render-shape UNSELECTED-CIRCLE CIRCLE-NEW-NODE EMPTY-CANVAS)
;; HALTING MEASURE: (length nodes)
;; DESIGN STRATEGY: Use HOF foldr on nodes
(define (nodes-to-scene nodes scene)
  (foldr
   ;; Node Scene -> Scene
   ;; GIVEN: a Node and a Scene
   ;; RETURNS: a Scene that portrays the given Node
   ;; DESIGN STRATEGY: Combine simpler functions
   (lambda (n scene) (render-node n scene)) scene nodes))
;; TESTS
(begin-for-test
  (check-equal? (nodes-to-scene '() EMPTY-CANVAS) EMPTY-CANVAS
                "render no nodes to empty canvas")
  (check-equal? (nodes-to-scene (list CIRCLE-NEW-NODE) EMPTY-CANVAS)
                (render-shape UNSELECTED-CIRCLE CIRCLE-NEW-NODE EMPTY-CANVAS)
                "render a new circle node on an empty canvas")
  (check-equal? (nodes-to-scene (list SEL-SQUARE-NEW-NODE) EMPTY-CANVAS)
                (render-shape SELECTED-SQUARE SEL-SQUARE-NEW-NODE EMPTY-CANVAS)
                "render a selected square node on an empty canvas"))

;; render-node : Node Scene -> Scene
;; GIVEN: a Node and a Scene
;; RETURNS: a Scene that portrays the given Node
;; EXAMPLES:
;; (render-node SEL-CIRCLE-NEW-NODE EMPTY-CANVAS)
;; = (render-shape SELECTED-CIRCLE SEL-CIRCLE-NEW-NODE EMPTY-CANVAS)
;; DESIGN STRATEGY: Cases on whether or not the node is selected
(define (render-node n scene)
  (local
    (;; type of the given node
     (define NODE-TYPE (node-type n))
     
     ;; render-selected-node : Node Scene NodeType -> Scene
     ;; GIVEN: a Node, a Scene and a NodeType
     ;; RETURNS: a Scene similar to the given one
     ;; but with the given selected node portrayed on it
     ;; DESIGN STRATEGY: Use template for NodeType on nt
     (define (render-selected-node n scene nt)
       (cond
         [(string=? nt NODE-CIRCLE) (render-shape SELECTED-CIRCLE n scene)]
         [(string=? nt NODE-SQUARE) (render-shape SELECTED-SQUARE n scene)]))
     
     ;; render-unselected-node : Node Scene NodeType -> Scene
     ;; GIVEN: a Node, a Scene and a NodeType
     ;; RETURNS: a Scene similar to the given one
     ;; but with the given unselected node portrayed on it
     ;; DESIGN STRATEGY: Use template for NodeType on nt
     (define (render-unselected-node n scene nt)
       (cond
         [(string=? nt NODE-CIRCLE) (render-shape UNSELECTED-CIRCLE n scene)]
         [(string=? nt NODE-SQUARE) (render-shape UNSELECTED-SQUARE n scene)])))
    
    (if (node-selected? n)
        (render-selected-node n scene NODE-TYPE)
        (render-unselected-node n scene NODE-TYPE))))
;; TESTS
(begin-for-test
  (check-equal? (render-node SEL-CIRCLE-NEW-NODE EMPTY-CANVAS)
                (render-shape SELECTED-CIRCLE SEL-CIRCLE-NEW-NODE EMPTY-CANVAS)
                "render a selected circle node on an empty canvas")
  (check-equal? (render-node SQUARE-NEW-NODE EMPTY-CANVAS)
                (render-shape UNSELECTED-SQUARE SQUARE-NEW-NODE EMPTY-CANVAS)
                "render an unselected square node on an empty canvas"))

;; render-shape : Image Node Scene -> Scene
;; GIVEN: an Image, a Node and a Scene
;; RETURNS: a Scene but with the given node portrayed
;; as the given image on the given Scene
;; EXAMPLES:
;; (render-shape UNSELECTED-CIRCLE CIRCLE-NEW-NODE EMPTY-CANVAS)
;; = (place-image UNSELECTED-CIRCLE NEW-NODE-X NEW-NODE-Y EMPTY-CANVAS)
;; DESIGN STRATEGY: Combine simpler functions
(define (render-shape img n scene)
  (local
    (;; x- and y- coordinates of the Node n
     (define NODE-X (node-pos-x n))
     (define NODE-Y (node-pos-y n))
     
     ;; a LON that represents the sons of the Node n
     (define NODE-SONS (node-sons n))
     
     ;; image of the node rendered on the scene
     (define NODE-IMAGE (place-image img NODE-X NODE-Y scene))
     
     ;; render-line : Node Scene -> Scene
     ;; GIVEN: a Node that is the son of Node n and a Scene
     ;; RETURNS: a Scene with a line portrayed between Node n and son Node
     ;; DESIGN STRATEGY: Use template for Node on son
     (define (render-line son scene)
       (scene+line scene
                   NODE-X
                   NODE-Y
                   (node-pos-x son)
                   (node-pos-y son)
                   LINE-COLOR))
     
     ;; render-node-with-lines : Node -> Scene
     ;; GIVEN: a Node
     ;; RETURNS: a Scene with the given node portrayed on the Scene
     ;; where the sons of the given Node are connected with lines joining them
     ;; HALTING MEASURE: (length NODE-SONS)
     ;; DESIGN STRATEGY: Use HOF foldr on NODE-SONS
     (define (render-node-with-lines n)
       (foldr
        ;; Node Scene -> Scene
        ;; GIVEN: a Node and a Scene
        ;; RETURNS: a Scene with the given son Node portayed on the given Scene
        ;; DESIGN STRATEGY: Combine simpler functions
        (lambda (son scene) (render-line son scene)) NODE-IMAGE NODE-SONS)))
    
    (nodes-to-scene NODE-SONS (render-node-with-lines n))))
;; TESTS
(begin-for-test
  (check-equal? (render-shape UNSELECTED-CIRCLE CIRCLE-NEW-NODE EMPTY-CANVAS)
                (place-image UNSELECTED-CIRCLE
                             NEW-NODE-X NEW-NODE-Y EMPTY-CANVAS)
                "render an unselected circle with no sons")
  (check-equal? (render-shape UNSELECTED-CIRCLE
                              CIRCLE-NODE-WITH-SQUARE-SON
                              EMPTY-CANVAS)
                (place-image UNSELECTED-SQUARE
                             NEW-NODE-X (update-pos-y NEW-NODE-Y)
                             (scene+line
                              (place-image UNSELECTED-CIRCLE
                                           NEW-NODE-X NEW-NODE-Y EMPTY-CANVAS)
                              NEW-NODE-X NEW-NODE-Y
                              NEW-NODE-X (update-pos-y NEW-NODE-Y)
                              LINE-COLOR))
                "render an unselected circle with an unselected square son"))

;; world-to-trees : World -> ListOfTree
;; GIVEN: a World
;; RETURNS: a list of all the trees in the given world.
;; EXAMPLES:
;; (world-to-trees INITIAL-WORLD) = empty
;; (world-to-trees INITIAL-WORLD-WITH-SEL-SQUARE) = SEL-SQUARE-NEW-NODE
;; DESIGN STRATEGY: Use template for World on w
(define (world-to-trees w)
  (world-nodes w))
;; TESTS
(begin-for-test
  (check-equal? (world-to-trees INITIAL-WORLD) empty
                "no trees in initial world")
  (check-equal? (world-to-trees INITIAL-WORLD-WITH-SEL-SQUARE)
                (list SEL-SQUARE-NEW-NODE)
                "selected square node in initial world"))

;; tree-to-root : Tree -> Node
;; GIVEN: a tree
;; RETURNS: the node at the root of the tree
;; EXAMPLES:
;; (tree-to-root SEL-SQUARE-NEW-NODE) = SEL-SQUARE-NEW-NODE
;; DESIGN STRATEGY: Combine simpler functions
(define (tree-to-root t)
  t)
;; TESTS
(begin-for-test
  (check-equal? (tree-to-root CIRCLE-NEW-NODE) CIRCLE-NEW-NODE
                "node at tree of new circle node")
  (check-equal? (tree-to-root SEL-SQUARE-NEW-NODE) SEL-SQUARE-NEW-NODE
                "node at tree of selected square node"))

;; tree-to-sons : Tree -> ListOfTree
;; GIVEN: a tree
;; RETURNS: the data associated with the immediate subtrees of the given tree.
;; EXAMPLES:
;; (tree-to-sons SEL-SQUARE-NEW-NODE) = empty
;; DESIGN STRATEGY: Use template for Node on t
(define (tree-to-sons t)
  (node-sons t))
;; TESTS
(begin-for-test
  (check-equal? (tree-to-sons SEL-SQUARE-NEW-NODE) empty
                "sons of selected square new node is empty")
  (check-equal? (tree-to-sons CIRCLE-NODE-WITH-SQUARE-SON)
                (list SQUARE-FIRST-SON-NODE)
                "sons of circle node that has a square son"))

;; node-to-center : Node -> Posn
;; GIVEN: a Node
;; RETURNS: the center of the given node as it is to be displayed on the scene.
;; EXAMPLES:
;; (node-to-center SEL-SQUARE-NEW-NODE) = (make-posn NEW-NODE-X NEW-NODE-Y)
;; DESIGN STRATEGY: Use template for Node on n
(define (node-to-center n)
  (make-posn (node-pos-x n) (node-pos-y n)))
;; TESTS
(begin-for-test
  (check-equal? (node-to-center SEL-SQUARE-NEW-NODE)
                (make-posn NEW-NODE-X NEW-NODE-Y)
                "posn of selected square new node"))

;; node-to-selected? : Node -> Boolean
;; GIVEN: a Node
;; RETURNS: true iff the given node is selected.
;; EXAMPLES:
;; (node-to-selected? SEL-SQUARE-NEW-NODE) = #true
;; (node-to-selected? CIRCLE-NEW-NODE) = #false
;; DESIGN STRATEGY: Use template for Node on n
(define (node-to-selected? n)
  (node-selected? n))
;; TESTS
(begin-for-test
  (check-equal? (node-to-selected? SEL-SQUARE-NEW-NODE) #true
                "selected square node is selected")
  (check-equal? (node-to-selected? CIRCLE-NEW-NODE) #false
                "circle new node is not selected"))

;; (run 5)