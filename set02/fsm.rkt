;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "02" "fsm.rkt")

(provide initial-state)
(provide next-state)
(provide accepting-state?)
(provide error-state?)

;; DATA DEFINITIONS

;; A State is one of :
;; -- "s0",
;; -- "s1",
;; -- "s2",
;; -- "s3",
;; -- "s4",
;; -- "s5"
;; INTERPRETATION: Each of them represent a state of the finite state machine(fsm)
;; WHERE: s0 is the initial state, s4 is the final(accepting) state and s5 is the error state.

;; TEMPLATE
;; state-fn : State -> ??
#|
(define (state-fn st)
 (cond
   [(string=? st "s0") ...]
   [(string=? st "s1") ...]
   [(string=? st "s2") ...]
   [(string=? st "s3") ...]
   [(string=? st "s4") ...]
   [(string=? st "s5") ...]
))  
|#

;; DATA DEFINITION
;; A MachineInput can be one of:
;; -- "q",
;; -- "x",
;; -- "u",
;; -- "a",
;; -- "b",
;; -- "d",
;; -- "e",
;; -- "f"

;; INTERPRETATION: Each of them represent the legal inputs to the machine

#|
;; TEMPLATE
(define (machineinput-fn mi)
 (cond
   [(string=? mi "q") ...]
   [(string=? mi "x") ...]
   [(string=? mi "u") ...]
   [(string=? mi "a") ...]
   [(string=? mi "b") ...]
   [(string=? mi "d") ...]
   [(string=? mi "e") ...]
   [(string=? mi "f") ...]
)) 
|#

;; FUNCTION DEFINITIONS

;; initial-state : Number -> State
;; GIVEN: a number
;; RETURNS: a representation of the initial state
;;         of your machine.The given number is ignored.
;; EXAMPLES:
;; (next-state "s0" "q") -> "s1"
;; (next-state "s1" "u") -> "s2"
;; DESIGN STARTEGY: Combine simpler functions

(define (initial-state num)
  "s0"
  )

;; TESTS
(begin-for-test
  (check-equal? (initial-state 5) "s0")
  )

;; HELPER FUNCTION
;; machine-next-state-on-q-or-x : State -> String
;; GIVEN: current state of the machine given as a string
;; RETURNS: next state of the machine on the inputs q or x given as a string
;; EXAMPLES:
;; (machine-next-state-on-q-or-x "s0") -> "s1"
;; (machine-next-state-on-q-or-x "s4") -> "s5"
;; DESIGN STARTEGY: Cases on State

(define (machine-next-state-on-q-or-x state)
  (cond [(equal? state "s0") "s1"]
        [(equal? state "s1") "s1"]
        [(equal? state "s2") "s5"]
        [(equal? state "s3") "s5"]
        [(equal? state "s4") "s5"]
        [(equal? state "s5") "s5"]
      )
 )

;; TESTS
(begin-for-test
  (check-equal? (machine-next-state-on-q-or-x "s3") "s5")
  (check-equal? (machine-next-state-on-q-or-x "s1") "s1")
  (check-equal? (machine-next-state-on-q-or-x "s2") "s5")
 )

;; HELPER FUNCTION
;; machine-next-state-on-u : State -> String
;; GIVEN: current state of the machine given as a string
;; RETURNS: next state of the machine on input u given as a string
;; EXAMPLES:
;; (machine-next-state-on-u "s0") -> "s2"
;; (machine-next-state-on-u "s3") -> "s5"
;; DESIGN STARTEGY: Cases on State

(define (machine-next-state-on-u state)
  (cond [(equal? state "s0") "s2"]
        [(equal? state "s1") "s2"]
        [(equal? state "s2") "s2"]
        [(equal? state "s3") "s5"]
        [(equal? state "s4") "s5"]
        [(equal? state "s5") "s5"]
      )
 )

;; TESTS
(begin-for-test
  (check-equal? (machine-next-state-on-u "s2") "s2")
  (check-equal? (machine-next-state-on-u "s3") "s5")
  (check-equal? (machine-next-state-on-u "s4") "s5")
  (check-equal? (machine-next-state-on-u "s5") "s5")
 )

;; HELPER FUNCTION
;; machine-next-state-on-a-or-b : State -> String
;; GIVEN: current state of the machine given as a string
;; RETURNS: next state of the machine on the inputs a or b given as a string
;; EXAMPLES:
;; (machine-next-state-on-a-or-b "s0") -> "s3"
;; (machine-next-state-on-a-or-b "s1") -> "s3"
;; DESIGN STARTEGY: Cases on State

(define (machine-next-state-on-a-or-b state)
  (cond [(equal? state "s0") "s3"]
        [(equal? state "s1") "s3"]
        [(equal? state "s2") "s3"]
        [(equal? state "s3") "s3"]
        [(equal? state "s4") "s5"]
        [(equal? state "s5") "s5"]
      )
 )

;; TESTS
(begin-for-test
  (check-equal? (machine-next-state-on-a-or-b "s0") "s3")
  (check-equal? (machine-next-state-on-a-or-b "s4") "s5")
  (check-equal? (machine-next-state-on-a-or-b "s5") "s5")
 ) 

;; HELPER FUNCTION
;; machine-next-state-on-d : State -> String
;; GIVEN: current state of the machine given as a string
;; RETURNS: next state of the machine on the inputs u given as a string
;; EXAMPLES:
;; (machine-next-state-on-d "s0") -> "s2"
;; (machine-next-state-on-d "s1") -> "s2"
;; DESIGN STARTEGY: Cases on State

(define (machine-next-state-on-d state)
  (cond [(equal? state "s0") "s4"]
        [(equal? state "s1") "s4"]
        [(equal? state "s2") "s4"]
        [(equal? state "s3") "s4"]
        [(equal? state "s4") "s5"]
        [(equal? state "s5") "s5"]
      )
 )

;; TESTS
(begin-for-test
  (check-equal? (machine-next-state-on-d "s5") "s5")
 )

;; HELPER FUNCTION
;; machine-next-state-on-e-or-f : State -> String
;; GIVEN: current state of the machine given as a string
;; RETURNS: next state of the machine on the inputs e or f given as a string
;; EXAMPLES:
;; (machine-next-state-on-e-or-f "s0") -> "s5"
;; (machine-next-state-on-e-or-f "s4") -> "s4"
;; DESIGN STARTEGY: Cases on State

(define (machine-next-state-on-e-or-f state)
  (cond [(equal? state "s0") "s5"]
        [(equal? state "s1") "s5"]
        [(equal? state "s2") "s5"]
        [(equal? state "s3") "s5"]
        [(equal? state "s4") "s4"]
        [(equal? state "s5") "s5"]
      )
 )

;; TESTS
(begin-for-test
  (check-equal? (machine-next-state-on-e-or-f "s5") "s5")
 )

;; next-state : State MachineInput -> State
;; GIVEN: a state of the machine and a machine input
;; RETURNS: the state that should follow the given input.
;; EXAMPLES:
;; (next-state "s0" "q") => "s1"
;; (next-state "s4" "u") => "s5"
;; STRATEGY: Cases on MachineInput
 
(define (next-state state mi)
  (cond
   [(or (string=? mi "q") (string=? mi "x")) (machine-next-state-on-q-or-x state)]
   [(string=? mi "u") (machine-next-state-on-u state)]
   [(or (string=? mi "a") (string=? mi "b")) (machine-next-state-on-a-or-b state)]
   [(string=? mi "d") (machine-next-state-on-d state)]
   [(or (string=? mi "e") (string=? mi "f")) (machine-next-state-on-e-or-f state)]      
   )
)

;; TESTS
(begin-for-test
  (check-equal? (next-state "s0" "q") "s1")
  (check-equal? (next-state "s0" "u") "s2")
  (check-equal? (next-state "s0" "e") "s5")
  (check-equal? (next-state "s0" "d") "s4")
  (check-equal? (next-state "s1" "q") "s1")
  (check-equal? (next-state "s1" "u") "s2")
  (check-equal? (next-state "s1" "a") "s3")
  (check-equal? (next-state "s1" "f") "s5")
  (check-equal? (next-state "s1" "d") "s4")
  (check-equal? (next-state "s2" "d") "s4")
  (check-equal? (next-state "s2" "a") "s3")
  (check-equal? (next-state "s2" "q") "s5")
  (check-equal? (next-state "s2" "f") "s5")
  (check-equal? (next-state "s3" "d") "s4")
  (check-equal? (next-state "s3" "b") "s3")
  (check-equal? (next-state "s3" "f") "s5")
  (check-equal? (next-state "s4" "d") "s5")
  (check-equal? (next-state "s4" "e") "s4")
  (check-equal? (next-state "s4" "q") "s5")
  (check-equal? (next-state "s5" "q") "s5")
 )

;; accepting-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff the given state is a final (accepting) state
;; EXAMPLES:
;; (accepting-state? "s0") => "false"
;; (accepting-state? "s4") => "true"
;; STRATEGY: Use template for State on state

(define (accepting-state? state)
  (if (string=? state "s4")
      #true
      #false)
 )

;; TESTS
(begin-for-test
  (check-equal? (accepting-state? "s2") #false)
  (check-equal? (accepting-state? "s4") #true)
  )

;; error-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff there is no path (empty or non-empty) from the given
;;         state to an accepting state
;; EXAMPLES:
;; (error-state? "s5") => "true"
;; (accepting-state? "s2") => "false"
;; DESIGN STRATEGY: Use template for State on state

(define (error-state? state)
 (if (string=? state "s5")
     #true
     #false)
 )

;; TESTS
(begin-for-test
  (check-equal? (error-state? "s3") #false)
  (check-equal? (error-state? "s5") #true)
  )