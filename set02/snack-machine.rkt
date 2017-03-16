;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snack-machine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "02" "snack-machine.rkt")

(provide initial-machine)
(provide machine-next-state)
(provide machine-output)
(provide machine-remaining-kale)
(provide machine-remaining-carrots)
(provide machine-bank)

;; DATA DEFINITION
(define-struct machinestate(kalechips carrotsticks machinebank customerbank))

;; A MachineState is a (make-machinestate PosInt PosInt PosReal PosReal)
;; kalechips indicates the numbers of bags of kale chips
;; carrotsticks indicates the numbers of bags of carrot sticks
;; machinebank indicates all the money that the machine has kept from customers' purchases in cents
;; customerbank indicates the number of quarters that the customer has put in the bank

;; TEMPLATE
;; machinestate-fn : MachineState -> ??
#|                   
(define (machinestate-fn state)
  (...
    (machinestate-KaleChips state)
    (machinestate-CarrotSticks state)
    (machinestate-MachineBank state)
    (machinestate-CustomerBank state)
    ))
|#

;; DATA DEFINITION
;; A CustomerInput is one of
;; -- a PosInt        interp: insert the specified number of quarters
;; -- "kale"          interp: request a bag of kale chips
;; -- "carrots"       interp: request a bag of carrots
;; -- "change"        interp: return all the unspent money that the
;;                             customer has inserted

;; TEMPLATE
;; customerinput-fn : CustomerInput -> ??
#|
(define (customerinput-fn ci)
 (cond
   [(string? ci)
   (cond    [(string=? ci "kale")    ...]
            [(string=? ci "carrots") ...]
            [(string=? ci "change")  ...])]
   [(number? ci) ...]))
|#

;; DATA DEFINITION
;; A MachineOutput is one of
;; -- "kale"           interp: machine dispenses a bag of kale chips
;; -- "carrots"        interp: machine dispenses a bag of carrot sticks
;; -- "Out of Item"    interp: machine displays "Out of Item"
;; -- a PosInt         interp: machine releases the specified number of quarters
;; -- "Nothing"        interp: the machine does nothing

;; TEMPLATE
;; machineoutput-fn : MachineOutput -> ??
#|
(define (machineoutput-fn mo)
 (cond
   [(string? mo)
   (cond    [(string=? mo "kale") ...]
            [(string=? mo "carrots") ...]
            [(string=? mo "Out of Item") ...]
   )]
   [(number? mo) ...]
   [else "Nothing"]
   ))  
|#

;; FUNCTION DEFINITIONS

;; initial-machine : NonNegInt NonNegInt -> MachineState
;; GIVEN: a number of bags of kale chips and carrot sticks
;; RETURNS: the state of a machine loaded with the given numbers of bags
;;          of kale chips and carrot sticks, with an empty bank.
;; EXAMPLES:
;; (initial-machine 10 10) => (make-machinestate 10 10 0 0)
;; DESIGN STARTEGY: Combine simpler functions

(define (initial-machine kalechips carrotsticks)
      (make-machinestate kalechips carrotsticks 0 0)
 )

;; TESTS
(define s0(initial-machine 2 2))

;; machine-remaining-kale : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the number of bags of kale chips left in the machine
;; EXAMPLES:
;; (machine-remaining-kale s0) => 2
;; DESIGN STRATEGY: Use template for MachineState on state

(define (machine-remaining-kale state)
  (machinestate-kalechips state)
  )

;; TESTS
(begin-for-test
  (check-equal? (machine-remaining-kale s0) 2)
 )

;; machine-remaining-carrots : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the number of bags of carrots left in the machine
;; EXAMPLES:
;; (machine-remaining-carrots s0) => 2
;; DESIGN STRATEGY: Use template for MachineState on state

(define (machine-remaining-carrots state)
  (machinestate-carrotsticks state)
  )

;; TESTS
( begin-for-test
   (check-equal? (machine-remaining-carrots s0) 2) 
 )

;; HELPER FUNCTION
;; quarters-in-machine : State -> State
;; GIVEN: a machine state
;; RETURNS: the number of quarters in the machine
;; EXAMPLES:
;; (money-in-customerbank (make-machinestate 2 2 0 2)) -> 2
;; DESIGN STARTEGY:Use template for MachineState on state

(define (quarters-in-machine state)
  (machinestate-customerbank state)
 )

;; TESTS
(begin-for-test
(check-equal? (quarters-in-machine (make-machinestate 2 2 0 2)) 2)
)

;; machine-next-state : MachineState CustomerInput -> MachineState
;; GIVEN: a machine state and a customer input
;; RETURNS: the state of the machine that should follow the customer's
;;          input
;; EXAMPLES :
;; (machine-next-state (make-machinestate 2 2 0 0.75) "kale") -> (make-machinestate 1 2 75 0.75)
;; DESIGN STRATEGY : Use template for CustomerInput on customerinput
  
(define (machine-next-state state customerinput)
  (cond
    [(string? customerinput)
     (cond [(and (string=? customerinput "change") (> (quarters-in-machine state) 0))
           (make-machinestate (machinestate-kalechips state) (machinestate-carrotsticks state) (machinestate-machinebank state) 0)]
           [(and (string=? customerinput "carrots") (> (machine-remaining-carrots state) 0)(>= (quarters-in-machine state) 2))
           (make-machinestate (machinestate-kalechips state) (- (machinestate-carrotsticks state) 1) (+ (machinestate-machinebank state) 50)
                                 (- (machinestate-customerbank state) 2))]
           [(and (string=? customerinput "kale") (> (machine-remaining-kale state) 0)(>= (quarters-in-machine state) 3))
           (make-machinestate (- (machinestate-kalechips state) 1) (machinestate-carrotsticks state) (+ (machinestate-machinebank state) 75)
                                 (- (machinestate-customerbank state) 3))]
           [else state]
      )]
    [(number? customerinput)
            (make-machinestate (machinestate-kalechips state) (machinestate-carrotsticks state) 0 (+ customerinput (machinestate-customerbank state)) ) ]
    [else state]
    )
)

;; TESTS
(begin-for-test
  (check-equal? (machine-next-state (make-machinestate 2 2 0 3) "change") (make-machinestate 2 2 0 0))
  (check-equal? (machine-next-state (make-machinestate 2 2 0 0) 4) (make-machinestate 2 2 0 4))
  (check-equal? (machine-next-state (make-machinestate 2 2 0 3) "kale") (make-machinestate 1 2 75 0))
  (check-equal? (machine-next-state (make-machinestate 1 2 75 2) "carrots") (make-machinestate 1 1 125 0))
  (check-equal? (machine-next-state (make-machinestate 1 2 0 1) "change") (make-machinestate 1 2 0 0))
  (check-equal? (machine-next-state (make-machinestate 2 2 0 3) "change") (make-machinestate 2 2 0 0))
  (check-equal? (machine-next-state (make-machinestate 2 2 0 0) "abcd") (make-machinestate 2 2 0 0))
  (check-equal? (machine-next-state (make-machinestate 2 2 0 3) "") (make-machinestate 2 2 0 3))
  (check-equal? (machine-next-state (make-machinestate 2 2 0 2) "carrots") (make-machinestate 2 1 50 0))
  (check-equal? (machine-next-state (make-machinestate 2 2 0 3) #true) (make-machinestate 2 2 0 3))
  (check-equal? (machine-next-state s0 "kale") s0)
)

;; machine-output : MachineState CustomerInput -> MachineOutput
;; GIVEN: a machine state and a customer input
;; RETURNS: a MachineOutput that describes the machine's response to the
;;          customer input
;; EXAMPLES:
;; (machine-output (make-machinestate 2 2 0 2) "change") => (make-machinestate 2 2 0 0)
;; DESIGN STRATEGY: Use template for CustomerInput on customerinput

(define (machine-output state customerinput)
  (cond
    [(string? customerinput)
     (cond [(and (string=? customerinput "change") (> (quarters-in-machine state) 0)) (quarters-in-machine state)]
           [(and (= (machine-remaining-kale state) 0) (< (quarters-in-machine state) 3)  (string=? customerinput "kale")) "Nothing"]
           [(and (= (machine-remaining-kale state) 0) (string=? customerinput "kale")) "Out of Item"]
           [(and (= (machine-remaining-carrots state) 0) (< (quarters-in-machine state) 2) (string=? customerinput "carrots")) "Nothing"]
           [(and (= (machine-remaining-carrots state) 0) (string=? customerinput "carrots")) "Out of item"]
           [(and (string=? customerinput "carrots") (> (machine-remaining-carrots state) 0)(>= (quarters-in-machine state) 2)) "carrots"]
           [(and (string=? customerinput "kale") (> (machine-remaining-kale state) 0)(>= (quarters-in-machine state) 3)) "kale"]
           [ else "Nothing"])
     ]
    [else "Nothing"]))

;; TESTS
(begin-for-test
    (check-equal? (machine-output (make-machinestate 1 2 0 3) "kale") "kale")
     (check-equal?(machine-output (make-machinestate 1 2 0 2) "carrots") "carrots")
     (check-equal?(machine-output (make-machinestate 1 1 0 4) "carrots") "carrots")
     (check-equal?(machine-output (make-machinestate 1 0 0 2) "carrots") "Out of item")
     (check-equal?(machine-output (make-machinestate 1 0 0 1) "carrots") "Nothing")
     (check-equal?(machine-output (make-machinestate 0 2 0 3) "kale") "Out of Item")
     (check-equal?(machine-output (make-machinestate 1 2 0 3) "change") 3)
     (check-equal?(machine-output (make-machinestate 0 2 0 2) "kale") "Nothing")
     (check-equal?(machine-output (make-machinestate 0 2 0 2) 21) "Nothing")
     (check-equal?(machine-output (make-machinestate 0 2 0 2) "") "Nothing")
     (check-equal?(machine-output s0 "kale") "Nothing")
     )

;; machine-bank : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the amount of money in the machine's bank, in cents
;; EXAMPLES :
;; (machine-bank (make-machinestate 2 2 20 3)) =>
;; DESIGN STRATEGY: Use template for MachineState on state

(define (machine-bank state)
  (machinestate-machinebank state)
  )

;; TESTS
(begin-for-test
  (check-equal? (machine-bank (make-machinestate 2 2 50 1)) 50) 
  )