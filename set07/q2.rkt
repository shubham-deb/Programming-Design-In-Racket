;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Goal: To design and implement a programmable
;;       Pluto probe as per the specification
;;       mentioned in Set 07, Question 2.

(require rackunit)
(require "sets.rkt")
(require "extras.rkt")

(check-location "07" "q2.rkt")

(provide probe-possible-outcome?)
(provide make-turn-right)
(provide make-turn-left)
(provide make-move-forward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;; direction constants
(define NORTH "north")
(define SOUTH "south")
(define EAST "east")
(define WEST "west")

;; origin coordinate constants
(define ORIGIN-X 0)
(define ORIGIN-Y 0)

;; probe constants
(define PROBE-INITIAL-DIR NORTH)

;; tolerance constant
(define TOLERANCE 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

(define-struct turn-left ())
;; A TurnLeft is a (make-turn-left)
;; INTERP: represents an instruction after which the probe turns left

;; EXAMPLE:
(define TURN-LEFT-INS (make-turn-left))

(define-struct turn-right ())
;; A TurnRight is a (make-turn-right)
;; INTERP: represents an instruction after which the probe turns right

;; EXAMPLE:
(define TURN-RIGHT-INS (make-turn-right))

(define-struct move-forward (steps))
;; A MoveForward is a (make-move-forward PosInt)
;; steps is the number of steps the probe takes.
;; INTERP: represents an instruction after which the probe changes its location

;; TEMPLATE:
;; mvfwd-fn : MoveForward -> ??
#; (define (mvfwd-fn mvf)
  (... (move-forward-steps mvf)))

;; EXAMPLES:
;; move forward instruction with 1 step
(define MOVE-FORWARD-1-STEP (make-move-forward 1))

;; move forward instruction with 5 steps
(define MOVE-FORWARD-5-STEP (make-move-forward 5))

;; move forward instruction with 10 steps
(define MOVE-FORWARD-10-STEP (make-move-forward 10))

;; A LeftOrRight is one of
;; -- "left"
;; -- "right"
;; INTERP: represents either a left direction or a right direction.

;; TEMPLATE:
;; left-or-right-fn : LeftOrRight -> ??
#; (define (left-or-right-fn lor)
     (cond
       [(string=? lor LEFT) ...]
       [(string=? lor RIGHT) ...]))

;; EXAMPLES: 
(define LEFT "left")
(define RIGHT "right")

;; An Instruction is one of
;; -- (make-turn-left)            Interp: a turn-left instruction
;; -- (make-turn-right)           Interp: a turn-right instruction
;; -- (make-move-forward PosInt)  Interp: an instruction to move forward
;;                                        the given number of steps.
;; INTERP: represents an instruction given to the probe. 

;; TEMPLATE:
;; instruction-fn : Instruction -> ??
#; (define (instruction-fn ins)
     (cond
       [(turn-left? ins) (...(turn-left-fn))]
       [(turn-right? ins) (...(turn-right-fn))]
       [(move-forward? ins) (... (mvfwd-fn ins))]))

;; EXAMPLES:
;; same as defined above

;; A ListOfInstruction (LOI) is one of:
;; -- empty
;;    interp: a sequence of Instruction with no elements
;; -- (cons Instruction LOI)
;;    interp: (cons Instruction LOI) represents a sequence of Instruction
;;            whose first element is a Instruction and whose
;;            other elements are represented by a LOI.
;; INTERP: is a list of instructions.

;; TEMPLATE:
;; loi-fn : LOI -> ??
;; HALTING MEASURE: (length insts)
#; (define (loi-fn insts)
     (cond
       [(empty? insts) ...]
       [else (... (instruction-fn (first insts))
                  (loi-fn (rest insts)))]))

;; EXAMPLES: 
;; a list of instruction with two move-forward instructions
(define LOI-TWO-MOVE-FORWARDS (list MOVE-FORWARD-1-STEP MOVE-FORWARD-5-STEP))

;; a list of instruction with a turn-right and a move-forward instruction
(define LOI-TURN-RIGHT-MVF (list TURN-RIGHT-INS MOVE-FORWARD-1-STEP))

;; A Program is a ListOfInstruction
;; INTERP: A sequence of instructions, to be executed from left to right.

;; EXAMPLES:
;; a program that moves the probe in east direction by 1 step
(define PROGRAM-MOVES-PROBE-EAST LOI-TURN-RIGHT-MVF)

;; a program that moves a north facing probe in south east direction
;; by 10 steps in both x- and y- coordinates
(define PROGRAM-MOVES-PROBE-SE
  (list TURN-RIGHT-INS
        MOVE-FORWARD-10-STEP
        TURN-RIGHT-INS
        MOVE-FORWARD-10-STEP
        TURN-LEFT-INS
        TURN-LEFT-INS))

;; a program that moves a north facing probe in north east direction
;; by 10 steps in x- coordinate and 5 steps in y- coordinate
(define PROGRAM-MOVES-PROBE-NE
  (list TURN-RIGHT-INS
        MOVE-FORWARD-10-STEP
        TURN-LEFT-INS
        MOVE-FORWARD-5-STEP))

;; another program that moves a north facing probe in south east direction
;; by 10 steps in x- coordinate and 5 steps in y- coordinate
(define PROGRAM-MOVES-PROBE-SE-2
  (list TURN-RIGHT-INS
        MOVE-FORWARD-10-STEP
        TURN-RIGHT-INS
        MOVE-FORWARD-5-STEP))

;; a program that has list of instructions which can be further simplified
(define PROGRAM-WITH-SIMPLIFICATION-POSSIBLE
  (list TURN-LEFT-INS
        MOVE-FORWARD-1-STEP
        MOVE-FORWARD-5-STEP
        TURN-RIGHT-INS
        TURN-LEFT-INS))

;; A Direction is one of
;; -- "north"
;; -- "south"
;; -- "east"
;; -- "west"
;; INTERP: self-evident

;; TEMPLATE:
;; direction-fn : Direction -> ??
#; (define (direction-fn dir)
     (cond
       [(string=? dir NORTH) ...]
       [(string=? dir SOUTH) ...]
       [(string=? dir EAST) ...]
       [(string=? dir WEST) ...]))

;; TEMPLATE:
;; direction-ew-fn : Direction -> ??
#; (define (direction-ew-fn dir)
     (cond
       [(string=? dir EAST) ...]
       [(string=? dir WEST) ...]
       [else ...]))

;; TEMPLATE:
;; direction-ns-fn : Direction -> ??
#; (define (direction-ns-fn dir)
     (cond
       [(string=? dir NORTH) ...]
       [(string=? dir SOUTH) ...]
       [else ...]))

(define-struct bounds (xlb xub ylb yub))
;; A Bounds is a (make-bounds Int Int Int Int)
;; xlb is the lower bound on the x-coordinate, in pixels
;; xub is the upper bound on the x-coordinate, in pixels
;; ylb is the lower bound on the y-coordinate, in pixels
;; yub is the upper bound on the y-coordinate, in pixels
;; INTERP: represents the bounds on the coordinates of a Probe.

;; TEMPLATE:
;; bounds-fn : Bounds -> ??
#; (define (bounds-fn b)
     (... (bounds-xlb b)
          (bounds-xub b)
          (bounds-ylb b)
          (bounds-yub b)))

;; EXAMPLES:
;; bounds for probe at the origin
(define INITIAL-BOUNDS (make-bounds ORIGIN-X ORIGIN-X ORIGIN-Y ORIGIN-Y))

;; bounds for a probe facing north
(define BOUNDS-FOR-PROBE-FACING-NORTH (make-bounds 20 20 100 100))

;; bounds for a probe facing west
(define BOUNDS-FOR-PROBE-FACING-WEST (make-bounds 20 20 8 8))

;; bounds for a probe facing south
(define BOUNDS-FOR-PROBE-FACING-SOUTH (make-bounds 4 4 20 20))

;; bounds for a probe facing east
(define BOUNDS-FOR-PROBE-FACING-EAST (make-bounds 1 1 5 5))

(define-struct probe (x y dir bounds))
;; A Probe is a (make-probe Int Int Direction Bounds)
;; x is the x-coordinate of the probe, in pixels
;; y is the y-coordinate of the probe, in pixels
;; dir is the direction that the probe faces
;; bounds is the bounds on the coordinates of the probe
;; INTERP: represents the space probe to Pluto

;; TEMPLATE:
;; probe-fn : Probe -> ??
#; (define (probe-fn pr)
     (... (probe-x pr)
          (probe-y pr)
          (probe-dir pr)
          (probe-bounds pr)))

;; EXAMPLES:
;; probe at origin facing north
(define ORIGIN-PROBE-FACING-NORTH
  (make-probe 0 0 PROBE-INITIAL-DIR INITIAL-BOUNDS))

;; probe at specific location facing north
(define PROBE-FACING-NORTH
  (make-probe 20 100 NORTH BOUNDS-FOR-PROBE-FACING-NORTH))

;; probe at specific location facing west
(define PROBE-FACING-WEST (make-probe 20 8 WEST BOUNDS-FOR-PROBE-FACING-WEST))

;; probe at specific location facing south
(define PROBE-FACING-SOUTH
  (make-probe 4 20 SOUTH BOUNDS-FOR-PROBE-FACING-SOUTH))

;; probe at specific location facing east
(define PROBE-FACING-EAST
  (make-probe 1 5 EAST BOUNDS-FOR-PROBE-FACING-EAST))

;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; probe-possible-outcome? : Int Int Program Int Int -> Boolean
;; GIVEN: starting coordinates x0, y0, a robot program p, and ending
;; coordinates x1, y1.
;; RETURNS: true iff the robot, starting at (x0, y0) and facing north,
;; and executing program p according to the tolerances given above,
;; could end at (x1, y1).
;; EXAMPLES:
;; (probe-possible-outcome? 20 100 P2 29 96) = #true
;; DESIGN STRATEGY: Combine simpler functions
(define (probe-possible-outcome? x0 y0 p x1 y1)
  (local
    (;; initial probe has bounds equal to the given starting coordinate
     (define PROBE-BOUNDS (make-bounds x0 x0 y0 y0))
     
     ;; initial probe with given x0 and y0 coordinates facing "north" direction
     (define PROBE (make-probe x0 y0 PROBE-INITIAL-DIR PROBE-BOUNDS))
     
     ;; program after simplification after application of heuristics
     (define SIMPLIFIED-PROGRAM (simplify-program p))
     
     ;; probe after execution of the simplified program
     (define PROBE-AFTER-PROGRAM-EXECUTION
       (execute-program SIMPLIFIED-PROGRAM PROBE))
     
     ;; probe-moved-unreliably? : Probe Int Int -> Boolean
     ;; GIVEN: a Probe and coordinates x, y that'd be the
     ;; ending coordinates of the given Probe
     ;; RETURNS: true iff the given Probe pr moving unreliably
     ;; could wind up in the given coordinates.
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (probe-moved-unreliably? pr x1 y1)
       (and (probe-moved-x-unreliably? pr x1)
            (probe-moved-y-unreliably? pr y1))))
    
    (probe-moved-unreliably? PROBE-AFTER-PROGRAM-EXECUTION x1 y1)))
;; TESTS
(begin-for-test
  (check-equal? (probe-possible-outcome? 20 23 PROGRAM-MOVES-PROBE-EAST 22 23)
                #true "probe moved east, outcome possible")
  (check-equal? (probe-possible-outcome? 20 23 PROGRAM-MOVES-PROBE-EAST 19 23)
                #false "probe moved east, outcome not possible")
  (check-equal? (probe-possible-outcome? 20 100 PROGRAM-MOVES-PROBE-NE 29 96)
                #true "probe moved north east, outcome possible"))

;; probe-moved-x-unreliably? : Probe Int -> Boolean
;; GIVEN: a Probe and a ending x-coordinate of the given Probe, respectively.
;; RETURNS: true iff the Probe pr moving unreliably in either east or west
;; directions could wind up in the x-coordinate x1.
;; EXAMPLES:
;; (probe-moved-x-unreliably? (execute-program PROGRAM-MOVES-PROBE-SE
;;                                             ORIGIN-PROBE-FACING-NORTH) 11 8)
;; = #true
;; DESIGN STRATEGY: Combine simpler functions
(define (probe-moved-x-unreliably? pr x1)
  (local
    (;; x-coordinate of the given Probe pr
     (define PROBE-X (probe-x pr))

     ;; bounds of the given Probe pr
     (define PROBE-BOUNDS (probe-bounds pr))

     ;; lower and upper bounds of the x-coordinate of the given probe pr
     (define PROBE-BOUNDS-X-LB (bounds-xlb PROBE-BOUNDS))
     (define PROBE-BOUNDS-X-UB (bounds-xub PROBE-BOUNDS)))
     
    (<= PROBE-BOUNDS-X-LB x1 PROBE-BOUNDS-X-UB)))
;; TESTS
(begin-for-test
  (check-equal? (probe-moved-x-unreliably?
                 (execute-program PROGRAM-MOVES-PROBE-SE
                                  ORIGIN-PROBE-FACING-NORTH) 11)
                #true "probe facing south east moved x reliably")
  (check-equal? (probe-moved-x-unreliably?
                 (execute-program PROGRAM-MOVES-PROBE-SE-2
                                  PROBE-FACING-NORTH) 29)
                #true "another probe facing south east moved x reliably")
  (check-equal? (probe-moved-x-unreliably?
                 (execute-program PROGRAM-MOVES-PROBE-EAST
                                  PROBE-FACING-NORTH) 19)
                #false "probe facing north moved x unreliably"))

;; probe-moved-y-unreliably? : Probe Int -> Boolean
;; GIVEN: a Probe and a ending y-coordinate of the given Probe, respectively.
;; RETURNS: true iff the Probe pr moving unreliably in either north or south
;; directions could wind up in the y-coordinate y1.
;; EXAMPLES:
;; (probe-moved-y-unreliably? PROBE-FACING-SOUTH 15 25) = #false
;; (probe-moved-y-unreliably? ORIGIN-PROBE-FACING-NORTH -1 -2) = #true
;; DESIGN STRATEGY: Combine simpler functions
(define (probe-moved-y-unreliably? pr y1)
  (local
    (;; y-coordinate of the given Probe pr
     (define PROBE-Y (probe-y pr))
     
     ;; bounds of the given Probe pr
     (define PROBE-BOUNDS (probe-bounds pr))
     
     ;; lower and upper bounds of the y-coordinate of the given probe pr
     (define PROBE-BOUNDS-Y-LB (bounds-ylb PROBE-BOUNDS))
     (define PROBE-BOUNDS-Y-UB (bounds-yub PROBE-BOUNDS)))
     
    (<= PROBE-BOUNDS-Y-LB y1 PROBE-BOUNDS-Y-UB)))
;; TESTS
(begin-for-test
  (check-equal? (probe-moved-y-unreliably?
                 (execute-program PROGRAM-MOVES-PROBE-SE
                                  ORIGIN-PROBE-FACING-NORTH) 12)
                #true "probe facing south east moved y reliably")
  (check-equal? (probe-moved-y-unreliably?
                 (execute-program PROGRAM-MOVES-PROBE-SE-2
                                  PROBE-FACING-NORTH) 106)
                #true "another probe facing south east moved y reliably")
  (check-equal? (probe-moved-y-unreliably?
                 (execute-program PROGRAM-MOVES-PROBE-EAST
                                  PROBE-FACING-NORTH) 25)
                #false "probe facing east moved y unreliably"))

;; execute-program : Program Probe -> Probe
;; GIVEN: a Program p and a Probe pr 
;; WHERE: pr is the Probe after execution of the n-th instruction of Program p
;; RETURNS: a Probe like the original after execution of the given Program p
;; EXAMPLES:
;; (execute-program PROGRAM-MOVES-PROBE-SE
;;                  ORIGIN-PROBE-FACING-NORTH)
;; = (make-probe 10 10 NORTH (make-bounds 8 12 8 12))
;; HALTING MEASURE: (length p)
;; DESIGN STRATEGY: Use template for Program on p
(define (execute-program p pr)
  (local
    (;; probe-turned-left : Probe -> Probe
     ;; GIVEN: a Probe
     ;; RETURNS: a Probe like the original, but turned 90 degrees left
     ;; DESIGN STRATEGY: Call a more general function
     (define (probe-turned-left pr)
       (probe-turned pr LEFT))

     ;; probe-turned-right : Probe -> Probe
     ;; GIVEN: a Probe
     ;; RETURNS: a Probe like the original, but turned 90 degrees right
     ;; DESIGN STRATEGY: Call a more general function
     (define (probe-turned-right pr)
       (probe-turned pr RIGHT))

     ;; execute-instruction : Instruction Probe -> Probe
     ;; GIVEN: an Instruction and a Probe
     ;; RETURNS: a Probe like the given one
     ;; after execution of the given instruction ins
     ;; DESIGN STRATEGY: Use template for Instruction on ins
     (define (execute-instruction ins pr)
       (cond
         [(turn-left? ins) (probe-turned-left pr)]
         [(turn-right? ins) (probe-turned-right pr)]
         [(move-forward? ins) (update-bounds pr
                                             (probe-moved-forward pr ins))])))
    
    (cond
      [(empty? p) pr]
      [else (execute-program (rest p) (execute-instruction (first p) pr))])))
;; TESTS
(begin-for-test
  (check-equal? (execute-program PROGRAM-MOVES-PROBE-SE
                                 ORIGIN-PROBE-FACING-NORTH)
                (make-probe 10 10 NORTH (make-bounds 8 12 8 12))
                "origin probe moved south east facing north"))

;; probe-turned : Probe LeftOrRight -> Probe
;; GIVEN: a Probe, a LeftOrRight
;; RETURNS: a Probe like the original, but turned 90 degrees
;; in the given LeftOrRight direction
;; EXAMPLES:
;; (probe-turned ORIGIN-PROBE-FACING-NORTH RIGHT)
;; = (make-probe 0 0 EAST INITIAL-BOUNDS)
;; (probe-turned ORIGIN-PROBE-FACING-NORTH LEFT)
;; = (make-probe 0 0 WEST INITIAL-BOUNDS)
;; DESIGN STRATEGY: Use template for Probe on pr
(define (probe-turned pr lor)
  (local
    (;; probe-dir-turned-right : Direction -> Direction
     ;; GIVEN: a Direction
     ;; RETURNS: a Direction when turned 90 degrees right
     ;; DESIGN STRATEGY: Use template for Direction on dir
     (define (probe-dir-turned-right dir)
       (cond
         [(string=? dir NORTH) EAST]
         [(string=? dir SOUTH) WEST]
         [(string=? dir EAST) SOUTH]
         [(string=? dir WEST) NORTH]))

     ;; probe-dir-turned-left : Direction -> Direction
     ;; GIVEN: a Direction
     ;; RETURNS: a Direction when turned 90 degrees left
     ;; DESIGN STRATEGY: Use template for Direction on dir
     (define (probe-dir-turned-left dir)
       (cond
         [(string=? dir NORTH) WEST]
         [(string=? dir SOUTH) EAST]
         [(string=? dir EAST) NORTH]
         [(string=? dir WEST) SOUTH]))

     ;; probe-dir-turned : LeftOrRight Direction -> Direction
     ;; GIVEN: a LeftOrRight, and a Direction
     ;; RETURNS: a Direction when turned 90 degrees
     ;; depending on the given LeftOrRight
     ;; DESIGN STRATEGY: Use template for LeftOrRight on lor
     (define (probe-dir-turned lor dir)
       (cond
         [(string=? lor LEFT) (probe-dir-turned-left dir)]
         [(string=? lor RIGHT) (probe-dir-turned-right dir)])))

  (make-probe (probe-x pr)
              (probe-y pr)
              (probe-dir-turned lor (probe-dir pr))
              (probe-bounds pr))))
;; TESTS
(begin-for-test
  (check-equal? (probe-turned ORIGIN-PROBE-FACING-NORTH RIGHT)
               (make-probe 0 0 EAST INITIAL-BOUNDS)
               "probe turned right from north to east")
  (check-equal? (probe-turned ORIGIN-PROBE-FACING-NORTH LEFT)
                (make-probe 0 0 WEST INITIAL-BOUNDS)
               "probe turned left from north to west")
  (check-equal? (probe-turned PROBE-FACING-SOUTH RIGHT)
                (make-probe 4 20 WEST BOUNDS-FOR-PROBE-FACING-SOUTH)
               "probe turned right from south to west")
  (check-equal? (probe-turned PROBE-FACING-SOUTH LEFT)
                (make-probe 4 20 EAST BOUNDS-FOR-PROBE-FACING-SOUTH)
               "probe turned left from south to east")
  (check-equal? (probe-turned PROBE-FACING-EAST RIGHT)
                (make-probe 1 5 SOUTH BOUNDS-FOR-PROBE-FACING-EAST)
               "probe turned right from east to south")
  (check-equal? (probe-turned PROBE-FACING-EAST LEFT)
                (make-probe 1 5 NORTH BOUNDS-FOR-PROBE-FACING-EAST)
               "probe turned left from east to north")
  (check-equal? (probe-turned PROBE-FACING-WEST RIGHT)
                (make-probe 20 8 NORTH BOUNDS-FOR-PROBE-FACING-WEST)
               "probe turned right from west to north")
  (check-equal? (probe-turned PROBE-FACING-WEST LEFT)
                (make-probe 20 8 SOUTH BOUNDS-FOR-PROBE-FACING-WEST)
               "probe turned left from west to south"))

;; probe-moved-forward : Probe MoveForward -> Probe
;; GIVEN: a Probe and a MoveForward instruction
;; RETURNS: a Probe like the original, but moved by the given mvf instruction
;; EXAMPLES:
;; (probe-moved-forward ORIGIN-PROBE-FACING-NORTH MOVE-FORWARD-1-STEP)
;; = (make-probe (probe-x ORIGIN-PROBE-FACING-NORTH)
;;               (sub1 (probe-y ORIGIN-PROBE-FACING-NORTH))
;;               (probe-dir ORIGIN-PROBE-FACING-NORTH))
;; DESIGN STRATEGY: Use template for Probe on pr
(define (probe-moved-forward pr mvf)
  (local
    (;; the number of steps of the mvf instruction 
     (define MOVE-FORWARD-STEPS (move-forward-steps mvf))

     ;; move-probe-x-west : Int -> Int
     ;; GIVEN: a x-coordinate of the Probe
     ;; RETURNS: a x-coordinate like the original when the Probe moves west
     ;; with the number of steps specified by the given mvf instruction
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (move-probe-x-west x)
       (- x MOVE-FORWARD-STEPS))
     
     ;; move-probe-x-east : Int -> Int
     ;; GIVEN: a x-coordinate of the Probe
     ;; RETURNS: a x-coordinate like the original when the Probe moves east
     ;; with the number of steps specified by the given mvf instruction
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (move-probe-x-east x)
       (+ x MOVE-FORWARD-STEPS))

     ;; move-probe-x : Int Direction -> Int
     ;; GIVEN: a x-coordinate and a Direction
     ;; RETURNS: a x-coordinate like the original when the Probe moves
     ;; along the x-axis in the given Direction dir
     ;; DESIGN STRATEGY: Use template for Direction on dir
     (define (move-probe-x x dir)
       (cond
         [(string=? dir EAST) (move-probe-x-east x)]
         [(string=? dir WEST) (move-probe-x-west x)]
         [else x]))

     ;; move-probe-y-south : Int -> Int
     ;; GIVEN: a y-coordinate of the Probe
     ;; RETURNS: a y-coordinate like the original when the Probe moves south
     ;; with the number of steps specified by the given mvf instruction
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (move-probe-y-south y)
       (+ y MOVE-FORWARD-STEPS))
     
     ;; move-probe-y-north : Int -> Int
     ;; GIVEN: a y-coordinate of the Probe
     ;; RETURNS: a y-coordinate like the original when the Probe moves north
     ;; with the number of steps specified by the given mvf instruction
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (move-probe-y-north y)
       (- y MOVE-FORWARD-STEPS))
     
     ;; move-probe-y : Int Direction -> Int
     ;; GIVEN: a y-coordinate and a Direction
     ;; RETURNS: a y-coordinate like the original when the Probe moves
     ;; along the y-axis in the given Direction dir
     ;; DESIGN STRATEGY: Use template for Direction on dir
     (define (move-probe-y y dir)
       (cond
         [(string=? dir NORTH) (move-probe-y-north y)]
         [(string=? dir SOUTH) (move-probe-y-south y)]
         [else y]))
       
     ;; move-probe : Int Int Direction Bounds -> Probe
     ;; GIVEN: a x- and y- coordinate, a Direction dir and a Bounds
     ;; RETURNS: a Probe with the given x- and y- coordinates updated
     ;; depending on the given Direction dir
     ;; DESIGN STRATEGY: Use template for Probe
     (define (move-probe x y dir bounds)
       (make-probe (move-probe-x x dir) (move-probe-y y dir) dir bounds)))
       
    (move-probe (probe-x pr) (probe-y pr) (probe-dir pr) (probe-bounds pr))))
;; TESTS
(begin-for-test
  (check-equal? (probe-moved-forward ORIGIN-PROBE-FACING-NORTH
                                     MOVE-FORWARD-1-STEP)
                (make-probe (probe-x ORIGIN-PROBE-FACING-NORTH)
                            (sub1 (probe-y ORIGIN-PROBE-FACING-NORTH))
                            (probe-dir ORIGIN-PROBE-FACING-NORTH)
                            (probe-bounds ORIGIN-PROBE-FACING-NORTH))
                "probe facing north moved forward by 1 step")
  (check-equal? (probe-moved-forward PROBE-FACING-SOUTH
                                     MOVE-FORWARD-1-STEP)
                (make-probe (probe-x PROBE-FACING-SOUTH)
                            (add1 (probe-y PROBE-FACING-SOUTH))
                            (probe-dir PROBE-FACING-SOUTH)
                            (probe-bounds PROBE-FACING-SOUTH))
                "probe facing south moved forward by 1 step")
  (check-equal? (probe-moved-forward PROBE-FACING-EAST
                                     MOVE-FORWARD-1-STEP)
                (make-probe (add1 (probe-x PROBE-FACING-EAST))
                            (probe-y PROBE-FACING-EAST)
                            (probe-dir PROBE-FACING-EAST)
                            (probe-bounds PROBE-FACING-EAST))
                "probe facing east moved forward by 1 step")
  (check-equal? (probe-moved-forward PROBE-FACING-WEST
                                     MOVE-FORWARD-1-STEP)
                (make-probe (sub1 (probe-x PROBE-FACING-WEST))
                            (probe-y PROBE-FACING-WEST)
                            (probe-dir PROBE-FACING-WEST)
                            (probe-bounds PROBE-FACING-WEST))
                "probe facing west moved forward by 1 step"))

;; update-bounds : Probe Probe -> Probe
;; GIVEN: two probes, one is the original one, and the other
;; is the probe moved forward but with bounds not yet updated
;; RETURNS: an updated Probe for the other Probe
;; with the bounds updated as per the move
;; EXAMPLES:
;; (update-bounds PROBE-FACING-WEST
;;                (probe-moved-forward PROBE-FACING-WEST MOVE-FORWARD-10-STEP))
;; = (make-probe 10 8 WEST (make-bounds 8 12 8 8))
;; DESIGN STRATEGY: Use template for Probe on upd-pr
(define (update-bounds pr upd-pr)
  (local
    (;; the x- and y- coordinates of the original probe pr
     (define PR-X (probe-x pr))
     (define PR-Y (probe-y pr))

     ;; the x- and y- coordinates of other probe moved forward upd-pr
     (define UPD-PR-X (probe-x upd-pr))
     (define UPD-PR-Y (probe-y upd-pr))
     
     ;; the direction of other probe moved forward upd-pr
     (define UPD-PR-DIR (probe-dir upd-pr))

     ;; the move that places the original probe pr
     ;; at the coordinates of the other given probe upd-pr
     (define MOVE-X (- UPD-PR-X PR-X))
     (define MOVE-Y (- UPD-PR-Y PR-Y))
     
     ;; update-xlb : Int Direction -> Int
     ;; GIVEN: a x-coordinate of the updated Probe upd-pr
     ;; RETURNS: the lower bound on the given x-coordinate
     ;; DESIGN STRATEGY: Use template for Direction on dir
     (define (update-xlb dir updprx)
       (local
         (;; the lower bound on the given x-coord based on tolerance
          (define POSSIBLE-X-LB (- (+ updprx MOVE-X) TOLERANCE)))
         
         (cond
           [(string=? dir EAST) (max updprx POSSIBLE-X-LB)]
           [(string=? dir WEST) POSSIBLE-X-LB]
           [else updprx])))
     
     ;; update-xub : Int Direction -> Int
     ;; GIVEN: a x-coordinate of the updated Probe upd-pr
     ;; RETURNS: the upper bound on the given x-coordinate
     ;; DESIGN STRATEGY: Use template for Direction on dir
     (define (update-xub dir updprx)
       (local 
         (;; the upper bound on the given x-coord based on tolerance
          (define POSSIBLE-X-UB (+ (+ updprx MOVE-X) TOLERANCE)))
         
         (cond
           [(string=? dir EAST) POSSIBLE-X-UB]
           [(string=? dir WEST) (min updprx POSSIBLE-X-UB)]
           [else updprx])))
     
     ;; update-ylb : Int Direction -> Int
     ;; GIVEN: a y-coordinate of the updated Probe upd-pr
     ;; RETURNS: the lower bound on the given y-coordinate
     ;; DESIGN STRATEGY: Use template for Direction on dir
     (define (update-ylb dir updpry)
       (local
         (;; the lower bound on the given y-coord based on tolerance
          (define POSSIBLE-Y-LB (- (+ updpry MOVE-Y) TOLERANCE)))
         
         (cond
           [(string=? dir NORTH) POSSIBLE-Y-LB]
           [(string=? dir SOUTH) (max updpry POSSIBLE-Y-LB)]
           [else updpry])))

     ;; update-yub : Int Direction -> Int
     ;; GIVEN: a y-coordinate of the updated Probe upd-pr
     ;; RETURNS: the upper bound on the given y-coordinate
     ;; DESIGN STRATEGY: Use template for Direction on dir
     (define (update-yub dir updpry)
       (local 
         (;; the upper bound on the given y-coord based on tolerance
          (define POSSIBLE-Y-UB (+ (+ updpry MOVE-Y) TOLERANCE)))
         
         (cond
           [(string=? dir NORTH) (min updpry POSSIBLE-Y-UB)]
           [(string=? dir SOUTH) POSSIBLE-Y-UB]
           [else updpry])))

     ;; updated-bounds : Bounds -> Bounds
     ;; GIVEN: a Bounds
     ;; RETURNS: a Bounds like the original
     ;; but updated depending on the updated Probe upd-pr
     ;; DESIGN STRATEGY: Use template for Bounds on upd-pr-bounds
     (define (updated-bounds upd-pr-bounds)
       (make-bounds (update-xlb UPD-PR-DIR (bounds-xlb upd-pr-bounds))
                    (update-xub UPD-PR-DIR (bounds-xub upd-pr-bounds))
                    (update-ylb UPD-PR-DIR (bounds-ylb upd-pr-bounds))
                    (update-yub UPD-PR-DIR (bounds-yub upd-pr-bounds)))))
    
    (make-probe UPD-PR-X
                UPD-PR-Y
                UPD-PR-DIR
                (updated-bounds (probe-bounds upd-pr)))))
;; TESTS
(begin-for-test
  (check-equal? (update-bounds PROBE-FACING-WEST
                               (probe-moved-forward PROBE-FACING-WEST
                                                    MOVE-FORWARD-10-STEP))
                (make-probe 10 8 WEST (make-bounds 8 12 8 8))
                "bounds updated for probe facing west moved by 10 steps")
  (check-equal? (update-bounds ORIGIN-PROBE-FACING-NORTH
                               (probe-moved-forward ORIGIN-PROBE-FACING-NORTH
                                                    MOVE-FORWARD-5-STEP))
                (make-probe 0 -5 NORTH (make-bounds 0 0 -7 -3))
                "bounds updated for probe facing north moved by 5 steps"))

;; simplify-program : Program -> Program
;; GIVEN: a Program p
;; WHERE: (empty? p) = #false
;; RETURNS: a Program like the original which might
;; be shorter depending on whether or not certain
;; program simplication heuristics apply on the given Program p
;; EXAMPLES
;; (simplify-program PROGRAM-WITH-SIMPLIFICATION-POSSIBLE)
;; = (list (make-turn-left) (make-move-forward 22))
;; DESIGN STRATEGY: Use HOF foldr on p
(define (simplify-program p)
  (local
    (;; negatable? : Instruction LeftOrRight -> Boolean
     ;; GIVEN: the previous Instruction and a LeftOrRight denoting
     ;; whether or not the current Instruction is a TurnLeft or a TurnRight
     ;; RETURNS: true iff the previous Instruction matches the given LeftOrRight
     ;; DESIGN STRATEGY: Use template for LeftOrRight on lor
     (define (negatable? pins lor)
       (cond
         [(string=? lor LEFT) (turn-left? pins)]
         [(string=? lor RIGHT) (turn-right? pins)]))

     ;; negate-consecutive-left-right : Instruction Instruction LOI LeftOrRight
     ;;                                 -> LOI
     ;; GIVEN: a current Instruction, a previous instruction, a LOI representing
     ;; the list of other previous instructions of the Program p,
     ;; and a LeftOrRight
     ;; RETURNS: a LOI like the original one but updated depending on
     ;; the previous instruction and given LeftOrRight
     ;; DESIGN STRATEGY: Cases on whether or not the given previous
     ;; instruction is a TurnLeft or a TurnRight 
     (define (negate-consecutive-left-right ins pins slst lor)
       (if (negatable? pins lor) (rest slst) (cons ins slst)))

     ;; reduce-instruction/given-prev : Instruction Instruction LOI -> LOI
     ;; GIVEN: a current Instruction, a previous instruction and a LOI
     ;; representing the list of other previous instructions of the Program p
     ;; RETURNS: a LOI like the original but updated
     ;; depending on the current Instruction ins
     ;; DESIGN STRATEGY: Use template for Instruction on ins
     (define (reduce-instruction/given-prev ins pins slst)
       (cond
         [(turn-left? ins) (negate-consecutive-left-right ins pins slst RIGHT)]
         [(turn-right? ins) (negate-consecutive-left-right ins pins slst LEFT)]
         [(move-forward? ins) (cons ins slst)]))
     
     ;; reduce-instruction : Instruction LOI -> LOI
     ;; GIVEN: an Instruction and a LOI representing the
     ;; list of previous instructions of the Program p
     ;; RETURNS: a LOI like the original one but updated
     ;; depending on the given Instruction ins
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (reduce-instruction ins slst)
       (cond
         [(empty? slst) (list ins)]
         [else (reduce-instruction/given-prev ins (first slst) slst)])))
    
    (foldr
     ;; Instruction Program -> Program
     ;; GIVEN: a Instruction and a Program
     ;; RETURNS: a reduced Program after application of heuristics
     ;; DESIGN STRATEGY: Combine simpler functions
     (lambda (ins lst) (reduce-instruction ins lst)) '() p)))
;; TESTS
(begin-for-test
  (check-equal? (simplify-program '()) '()
                "simplify a program with no instructions")
  (check-equal? (simplify-program PROGRAM-WITH-SIMPLIFICATION-POSSIBLE)
                (list TURN-LEFT-INS
                      MOVE-FORWARD-1-STEP
                      MOVE-FORWARD-5-STEP)
                "simplify a program where simplification possible"))
