;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Goal: To write a program that helps in determining whether
;;       or not resolution theorem proving can lead to a failure.

(require rackunit)
(require "sets.rkt")
(require "extras.rkt")

(check-location "08" "q2.rkt")

(provide make-pos)
(provide make-neg)
(provide make-clause)
(provide is-null-derivable?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONSTANTS

;; a symbol that states that two clauses were not unifiable
(define NOT-UNIFIABLE 'not-unifiable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

;; A Variable is a Racket Symbol.

(define-struct pos (v))
;; A PosLiteral is a (make-pos Variable)
;; v is a variable of the positive literal.
;; INTERP: represents a positive literal

;; TEMPLATE:
;; pos-fn : PosLiteral -> ??
#; (define (pos-fn pl)
     (... (pos-v pl)))

;; EXAMPLES:
(define APOS (make-pos 'A))
(define BPOS (make-pos 'B))
(define CPOS (make-pos 'C))
(define DPOS (make-pos 'D))

(define-struct neg (v))
;; A NegLiteral is a (make-neg Variable)
;; v is a variable of the negative literal.
;; INTERP: represents a negative literal

;; TEMPLATE:
;; neg-fn : NegLiteral -> ??
#; (define (neg-fn nl)
     (... (neg-fn nl)))

;; EXAMPLES:
(define ANEG (make-neg 'A))
(define BNEG (make-neg 'B))
(define CNEG (make-neg 'C))

;; A Literal is one of
;; -- (make-pos Variable)  Interp: a literal containing the variable
;; -- (make-neg Variable)  Interp: a literal containing the negation of
;;                                 the variable

;; TEMPLATE:
;; literal-fn : Literal -> ??
#; (define (literal-fn l)
     (cond
       [(pos? l) (... (pos-fn l))]
       [(neg? l) (... (neg-fn l))]))

;; EXAMPLES:
;; positive literals: x, y, z
(define POSX (make-pos 'x))
(define POSY (make-pos 'y))
(define POSZ (make-pos 'z))

;; negative literals: ~x, ~y, ~z
(define NEGX (make-neg 'x))
(define NEGY (make-neg 'y))
(define NEGZ (make-neg 'z))

;; A ListOfLiteral (LOL) is one of:
;; -- empty
;;    interp: a sequence of Literal with no elements
;; -- (cons Literal LOL)
;;    interp: (cons Literal LOL) represents a sequence of Literal
;;            whose first element is a Literal and whose
;;            other elements are represented by a LOL.
;; INTERP: is a list of literals.

;; TEMPLATE:
;; lol-fn : LOL -> ??
;; HALTING MEASURE: (length literals)
#; (define (lol-fn literals)
     (cond
       [(empty? literals) ...]
       [else (... (literal-fn (first literals))
                  (lol-fn (rest literals)))]))

;; A SymbolOrLOL (SOLOL) is one of:
;; -- Symbol
;;    interp: representing a Symbol that signifies not unifiable clauses
;; -- LOL
;;    interp: representing a LOL which is a list of literals.
;; INTERP: is a symbol or a list of literals.

;; TEMPLATE:
;; solol-fn : SOLOL -> ??
#;(define (solol-fn solol)
    (cond
      [(symbol? solol) ...]
      [else (... (lol-fn (solol)))]))


;; EXAMPLES:
(define ALL-POS-LITS-XYZ (list POSX POSY POSZ))
(define ALL-NEG-LITS-XYZ (list NEGX NEGY NEGZ))
(define DUP-LOL (list POSX NEGY POSX NEGZ NEGY))

;; A SetOfLiteral (SOL) is one of:
;; -- empty
;;    interp: a sequence of Literal with no elements
;; -- (cons Literal SOL)
;;    interp: (cons Literal SOL) represents a sequence of Literal
;;            whose first element is a Literal and whose
;;            other elements are represented by a SOL.
;; WHERE: all elements of SOL are distinct
;; INTERP: is a set of literals.

;; TEMPLATE:
;; sol-fn : SOL -> ??
;; HALTING MEASURE: (length literals)
#; (define (sol-fn literals)
     (cond
       [(empty? literals) ...]
       [else (... (literal-fn (first literals))
                  (sol-fn (rest literals)))]))

;; EXAMPLES:
(define SOL-FROM-DUP-LOL (list POSX NEGY NEGZ))

;; A Clause is a SetOfLiteral

;; EXAMPLES:
;; Same as above.

;; A ListOfClause (LOC) is one of:
;; -- empty
;;    interp: a sequence of Clause with no elements
;; -- (cons Clause LOC)
;;    interp: (cons Clause LOC) represents a sequence of Clause
;;            whose first element is a Clause and whose
;;            other elements are represented by a LOC.
;; INTERP: is a list of clauses.

;; TEMPLATE:
;; loc-fn : LOC -> ??
;; HALTING MEASURE: (length clauses)
#; (define (loc-fn clauses)
     (cond
       [(empty? clauses) ...]
       [else (... (sol-fn (first clauses))
                  (loc-fn (rest clauses)))]))

;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TEST EXAMPLES: 

;; list of clauses for clauses with no clause with one literal only
(define NO-CLAUSE-WITH-ONE-LITERAL
  (list (list (make-pos 'Q) (make-neg 'P))
        (list (make-pos 'R) (make-neg 'Q))
        (list (make-pos 'S) (make-neg 'R))
        (list (make-neg 'U) (make-neg 'S))))

;; example of null-derivable set of clauses
(define NULL-DERIVABLE-CLAUSES
  (list (list APOS BNEG CPOS)
        (list DPOS BPOS)
        (list ANEG CPOS)
        (list BPOS)
        (list CNEG)))

;; another example of null-derivable set of clauses
(define ANOTHER-NULL-DERIVABLE-CLAUSES
  (list (list APOS BPOS)
        (list BNEG CNEG)
        (list ANEG DPOS)
        (list BPOS CNEG)
        (list CPOS)))

;; no clauses
(define NO-CLAUSES '())

;; one clause with no literals
(define ONE-CLAUSE-WITH-NO-LITERALS
  (list '()))

;; one clause with one +ve literal
(define ONE-CLAUSE-WITH-ONE-POS-LITERAL
  (list (list APOS)))

;; one clause with two complimentary literals
(define ONE-CLAUSE-WITH-TWO-COMP-LITERALS
  (list (list APOS ANEG)))

;; one clause with two -ve literals
(define ONE-CLAUSE-WITH-TWO-NEG-LITERALS
  (list (list ANEG BNEG)))

;; two clauses with one complimentary literal
(define TWO-CLAUSES-WITH-ONE-COMP-LITERAL
  (list (list ANEG BNEG)
        (list BPOS)))

;; two clauses with only one complimentary literal
(define TWO-CLAUSES-WITH-ONLY-ONE-COMP-LITERAL
  (list (list BNEG)
        (list BPOS)))

;; two clauses with no complimentary literals
(define TWO-CLAUSES-WITH-NO-COMP-LITERALS
  (list (list APOS BPOS)
        (list BPOS APOS)))

;; two clauses with two literals each 
(define TWO-CLAUSES-WITH-TWO-LITERALS-EACH
  (list (list APOS BNEG)
        (list BPOS ANEG)))

;; three clauses example
(define THREE-CLAUSES-EXAMPLE
  (list (list APOS BPOS CPOS)
        (list ANEG BNEG CPOS)
        (list CNEG ANEG)))

;; four clauses example
(define FOUR-CLAUSES-EXAMPLE
  (list (list APOS BNEG)
        (list APOS BPOS)
        (list ANEG BPOS)
        (list ANEG BNEG)))

;; two clauses with two complimentary literals
(define TWO-CLAUSES-WITH-BOTH-COMP-LITERALS
  (list (list APOS BPOS)
        (list BNEG ANEG)))

;; another four clauses example
(define ANOTHER-FOUR-CLAUSES-EXAMPLE
  (list (list APOS ANEG)
        (list ANEG APOS)
        (list ANEG)
        (list APOS)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; merge : Literal LOL -> LOL
;; GIVEN: a Literal and a LOL
;; RETURNS: a LOL like the original but with
;; the given literal added to it depending on whether
;; or not the literal needs to be merged with the given LOL.
;; EXAMPLES:
;; (merge-literal POSX '()) = '()
;; (merge-literal POSX (list APOS BPOS BNEG)) = (list APOS BPOS BNEG)
;; HALTING MEASURE: (length literals)
;; DESIGN STRATEGY: Use template for LOL on literals
(define (merge-literal literal literals)
  (cond
    [(empty? literals) '()]
    [else (merge-nel literal literals)]))
;; TESTS
(begin-for-test
  (check-equal? (merge-literal POSX '()) '()
                "no literals to merge with")
  (check-equal? (merge-literal POSX (list APOS BPOS BNEG))
                (list APOS BPOS BNEG)
                "literal not mergeable"))

;; merge-nel : Literal LOL -> LOL
;; GIVEN: a Literal and a LOL
;; RETURNS: a LOL like the original but with
;; the given literal added to it depending on whether
;; or not the literal needs to be merged with the given LOL.
;; WHERE: (empty? literals) = #false
;; EXAMPLES:
;; (merge-nel POSX (list APOS BPOS BNEG)) = (list APOS BPOS BNEG)
;; DESIGN STRATEGY: Cases on whether or not the given non-empty
;; list of literals is mergeable with the given literal
(define (merge-nel literal literals)
  (local
    (;; first literal of the given literals
     (define FIRL (first literals))
     
     ;; rest of the literals of the given literals
     (define RESL (rest literals)))
    
    (if (nel-mergeable? literal literals FIRL RESL)
        (merge-literal literal RESL)
        (cons FIRL (merge-literal literal RESL)))))
;; TESTS
(begin-for-test
  (check-equal? (merge-nel POSX (list APOS BPOS BNEG))
                (list APOS BPOS BNEG)
                "literal not mergeable"))

;; nel-mergeable? : Literal LOL Literal LOL -> Boolean
;; GIVEN: a Literal, a LOL, the first element of the given LOL
;; and the rest of the elements of the given LOL
;; RETURNS: true iff the given literal can neither
;; match itself nor its negation in the list of literals
;; and also checks for duplicates literals.
;; EXAMPLES:
;; (nel-mergeable? ANEG (list APOS BNEG) APOS (list BNEG)) = #true
;; (nel-mergeable? POSX (list APOS BNEG) APOS (list BNEG)) = #false
;; DESIGN STRATEGY: Combine simpler functions
(define (nel-mergeable? literal literals firl resl)
  (or (equal? literal firl)
      (equal? (negate literal) firl)
      (member? firl resl)))
;; TESTS
(begin-for-test
  (check-equal? (nel-mergeable? POSX (list APOS BNEG) APOS (list BNEG))
                #false "literal not mergeable with literals")
  (check-equal? (nel-mergeable? ANEG (list APOS BNEG) APOS (list BNEG))
                #true "literal mergeable with literals"))

;; negate : Literal -> Literal
;; GIVEN: a Literal
;; RETURNS: a negated Literal, i.e. PosLiteral to NegLiteral and vice-versa.
;; EXAMPLES:
;; (negate APOS) = ANEG
;; (negate ANEG) = APOS
;; DESIGN STRATEGY: Cases on whether or not the given literal is a PosLiteral
(define (negate literal)
  (if (pos? literal) (make-neg (pos-v literal)) (make-pos (neg-v literal))))
;; TESTS
(begin-for-test
  (check-equal? (negate APOS) ANEG "negate +ve literal to -ve literal")
  (check-equal? (negate ANEG) APOS "negate +ve literal to -ve literal"))

;; is-null-derivable? : ListOfClause -> Boolean
;; GIVEN: a list of clauses
;; RETURNS: true iff the empty clause is derivable from the given
;; clauses using the rule of resolution as given in the question.
;; EXAMPLES:
;; (is-null-derivable? NO-CLAUSES) = #false
;; (is-null-derivable? ANOTHER-FOUR-CLAUSES-EXAMPLE) = #true
;; HALTING MEASURE: (length premise)
;; DESIGN STRATEGY: Use template for LOC on pr
(define (is-null-derivable? premise)
  (cond
    [(empty? premise) #false]
    [else (null-derivable-on-clauses? premise premise)]))
;; TESTS
(begin-for-test
  (check-equal? (is-null-derivable? NO-CLAUSE-WITH-ONE-LITERAL) #false
                "null not derivable on clauses with no clause with one literal")
  (check-equal? (is-null-derivable? NULL-DERIVABLE-CLAUSES) #true
                "null derivable set of clauses")
  (check-equal? (is-null-derivable? ANOTHER-NULL-DERIVABLE-CLAUSES) #true
                "null derivable set of clauses")
  (check-equal? (is-null-derivable? NO-CLAUSES) #false
                "null not derivable on an empty list of clauses")
  (check-equal? (is-null-derivable? ONE-CLAUSE-WITH-NO-LITERALS) #true
                "null derivable on only one clause with no literals")
  (check-equal? (is-null-derivable? ONE-CLAUSE-WITH-ONE-POS-LITERAL) #false
                "null not derivable on only one clause wth one +ve literal")
  (check-equal? (is-null-derivable? ONE-CLAUSE-WITH-TWO-COMP-LITERALS) #false
                "null not derivable on only one clause with two comp. literals")
  (check-equal? (is-null-derivable? ONE-CLAUSE-WITH-TWO-NEG-LITERALS) #false
                "null not derivable on only one clause with two -ve literals")
  (check-equal? (is-null-derivable? TWO-CLAUSES-WITH-ONE-COMP-LITERAL) #false
                "null not derivable on two clauses with one comp. literal")
  (check-equal? (is-null-derivable? TWO-CLAUSES-WITH-ONLY-ONE-COMP-LITERAL) #t
                "null derivable on two clauses with only one comp. literal")
  (check-equal? (is-null-derivable? TWO-CLAUSES-WITH-NO-COMP-LITERALS) #false
                "null not derivable on two clauses with no comp. literals")
  (check-equal? (is-null-derivable? TWO-CLAUSES-WITH-TWO-LITERALS-EACH) #false
                "null not derivable on two clauses with two literals each")
  (check-equal? (is-null-derivable? THREE-CLAUSES-EXAMPLE) #false
                "null not derivable on the three clauses example")
  (check-equal? (is-null-derivable? FOUR-CLAUSES-EXAMPLE) #true
                "null not derivable on the four clauses example")
  (check-equal? (is-null-derivable? TWO-CLAUSES-WITH-BOTH-COMP-LITERALS) #false
                "null not derivable on two clauses with both comp. literals")
  (check-equal? (is-null-derivable? ANOTHER-FOUR-CLAUSES-EXAMPLE) #true))

;; null-derivable-on-clauses? : LOC LOC -> Boolean
;; GIVEN: two LOCs, one that represents the remainining clauses
;; and the other that represents the original list of clauses
;; RETURNS: true iff null is derivable on the remaining clauses
;; EXAMPLES:
;; (null-derivable-on-clauses? '() '()) = #false
;; HALTING MEASURE: (length remainder)
;; DESIGN STRATEGY: Use template for LOC on remainder
(define (null-derivable-on-clauses? remainder clauses)
  (cond
    [(empty? remainder) #false]
    [else (null-derivable-on-remainder? remainder clauses)]))
;; TESTS
(begin-for-test
  (check-equal? (null-derivable-on-clauses? '() '()) #false
                "null not derivable on empty set of clauses"))

;; null-derivable-on-remainder? : LOC LOC -> Boolean
;; GIVEN: two LOCs, one that represents the remaining clauses
;; and the other that represents the original list of clauses
;; RETURNS: true iff null is derivable on the the remaining clauses
;; EXAMPLES:
;; (null-derivable-on-remainder? ONE-CLAUSE-WITH-ONE-POS-LITERAL
;;                               ONE-CLAUSE-WITH-ONE-POS-LITERAL)
;; = #false
;; DESIGN STRATEGY: Cases on whether or not there is only one remaining clause
(define (null-derivable-on-remainder? remainder clauses)
  (if (= (length remainder) 1)
      (empty? (car remainder))
      (null-derivable-on-resolvent? (car remainder) (cdr remainder)
                                    remainder clauses)))
;; TESTS
(begin-for-test
  (check-equal? (null-derivable-on-remainder?
                 ONE-CLAUSE-WITH-ONE-POS-LITERAL
                 ONE-CLAUSE-WITH-ONE-POS-LITERAL)
                #false "null not derivable on the only one +ve literal")
  (check-equal? (null-derivable-on-remainder?
                 (list (list BPOS)) (list (list BPOS)))
                #false
                "only one clause given, null derivable"))

;; null-derivable-on-resolvent? : Clause LOC LOC LOC -> Boolean
;; GIVEN: the first remaining clause, the rest of remaining clauses, 
;; and the complete set of remaining and all clauses
;; RETURNS: true iff null is derivable on the resolvent clause
;; EXAMPLES:
;; (null-derivable-on-resolvent? (car TWO-CLAUSES-WITH-TWO-LITERALS-EACH)
;;                               (cdr TWO-CLAUSES-WITH-TWO-LITERALS-EACH)
;;                               TWO-CLAUSES-WITH-TWO-LITERALS-EACH
;;                               TWO-CLAUSES-WITH-TWO-LITERALS-EACH)
;; = #false
;; HALTING MEASURE: (length rest)
;; DESIGN STRATEGY: Use template for LOC on rest
(define (null-derivable-on-resolvent? first rest remainder clauses)
  (cond
    [(empty? rest) (null-derivable-on-clauses? (cdr remainder) clauses)]
    [else (null-derivable-on-resolvent?/#rsv first rest remainder clauses)]))

;; TESTS
(begin-for-test
  (check-equal? (null-derivable-on-resolvent?
                 (car TWO-CLAUSES-WITH-TWO-LITERALS-EACH)
                 (cdr TWO-CLAUSES-WITH-TWO-LITERALS-EACH)
                 TWO-CLAUSES-WITH-TWO-LITERALS-EACH
                 TWO-CLAUSES-WITH-TWO-LITERALS-EACH)
                #false "null not derivable, two clauses with two literals"))

;; null-derivable-on-resolvent?/#rsv : Clause LOC LOC LOC -> Boolean
;; GIVEN: the first remaining clause, the rest of remaining clauses, 
;; and the complete set of remaining and all clauses
;; RETURNS: true iff null is derivable on the resolvent clause
;; EXAMPLES:
;; (null-derivable-on-resolvent?/#rsv (car ANOTHER-FOUR-CLAUSES-EXAMPLE)
;;                                    (cdr ANOTHER-FOUR-CLAUSES-EXAMPLE)
;;                                    ANOTHER-FOUR-CLAUSES-EXAMPLE
;;                                    ANOTHER-FOUR-CLAUSES-EXAMPLE)
;; = #true
;; HALTING MEASURE: (length rest)
;; TERMINATION ARGUMENT: (cdr rest) is guaranteed to be empty,
;; and it reduces in size at every recursive call
;; DESIGN STRATEGY: Recur on (cdr rest)
(define (null-derivable-on-resolvent?/#rsv first rest remainder clauses)
  (local
    (;; resolve the first and second clause (from the first of rest)
     (define resolvent (resolve first (car rest))))
    
    (cond
      ;; when the two clauses could not unified or resolved
      ;; or the resolvent is part of the original list of clauses
      [(or (equal? resolvent NOT-UNIFIABLE) (member? resolvent clauses))
       
       ;; check whether null is derivable on the remaining clauses in rest
       (null-derivable-on-resolvent? first (cdr rest) remainder clauses)]
      
      ;; if the resolvent is an empty resolvent
      ;; i.e. the two clauses resolved to a null => null derived!
      [(empty? resolvent) #t]
      
      ;; if the null is not derived yet,
      ;; however, the resolvent leads to a tautology
      ;; then, null can't be derived
      [(tautology? resolvent) #f]
      
      ;; else, recur and see whether null is derivable on
      ;; the set of clauses with the resolvent added to it
      [else (null-derivable-on-clauses?
             (cons resolvent clauses) (cons resolvent clauses))])))
;; TESTS
(begin-for-test
  (check-equal? (null-derivable-on-resolvent?/#rsv
                 (car ANOTHER-FOUR-CLAUSES-EXAMPLE)
                 (cdr ANOTHER-FOUR-CLAUSES-EXAMPLE)
                 ANOTHER-FOUR-CLAUSES-EXAMPLE
                 ANOTHER-FOUR-CLAUSES-EXAMPLE)
                #true "null derivable on another example with four clauses"))

;; tautology? : SOLOL -> Boolean
;; GIVEN: a SOLOL after resolution of two clauses
;; RETURNS: #true iff the given SOLOL leads to tautology
;; EXAMPLES:
;; (tautology? NOT-UNIFIABLE) = #false
;; (tautology? '()) = #true
;; DESIGN STRATEGY: Use template for SOLOL on resolvent
(define (tautology? resolvent)
  (local
    (;; tautology?/#sym : Symbol -> Boolean
     ;; GIVEN: a Symbol
     ;; RETURNS: true iff the given symbol is NOT-UNIFIABLE
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (tautology?/#sym sym)
       (not (equal? sym NOT-UNIFIABLE)))
     
     ;; tautology?/#lol : LOL -> Boolean
     ;; GIVEN: a LOL
     ;; RETURNS: true iff the given LOL leads to a tautology
     ;; HALTING MEASURE: (length lol)
     ;; DESIGN STRATEGY: Use template for LOL on lol
     (define (tautology?/#lol lol)
       (cond
         [(empty? lol) #t]
         [else (tautology? (resolve lol lol))])))
    
    (cond
      [(symbol? resolvent) (tautology?/#sym resolvent)]
      [else (tautology?/#lol resolvent)])))
;; TESTS
(begin-for-test
  (check-equal? (tautology? NOT-UNIFIABLE) #false
                "given resolvent states that two clauses were not unifiable")
  (check-equal? (tautology? '()) #true
                "given resolvent leads to a tautology"))

;; resolve : Clause Clause -> SOLOL
;; GIVEN: two clauses that need to be unified
;; depending on whether or not they can be unified
;; RETURNS: a SOLOL signifying either the resolved clause
;; or a symbol stating that the two clauses were not unifiable
;; EXAMPLES:
;; (resolve (list (make-pos 'P)) (list (make-pos 'Q))) = NOT-UNIFIABLE
;; (resolve (list (make-pos 'P)) (list (make-neg 'P))) = '()
;; DESIGN STRATEGY: Combine simpler functions
(define (resolve x y)
  (local
    (;; resolve-clauses/#unify : Clause Clause -> SOLOL
     ;; GIVEN: two clauses that need to be unified
     ;; RETURNS: a SOLOL that represents the unified clauses
     ;; DESIGN STRATEGY: Cases on whether or not the
     ;; negation of rest-x is a member of rest-y
     (define (resolve-clauses/#unify rest-x rest-y)
       (if (member? (negate (car rest-x)) rest-y)
           (merge-literal (car rest-x) (append x y))
           (resolve-clauses (cdr rest-x) rest-y)))
     
     ;; resolve-clauses : Clause Clause -> SOLOL
     ;; GIVEN: two clauses that need to be unified
     ;; depending on whether or not they can be unified
     ;; RETURNS: a SOLOL signifying either the resolved clause
     ;; or a symbol stating that the two clauses were not unifiable
     ;; HALTING MEASURE: (length rest-x)
     ;; DESIGN STRATEGY: Use template for Clause on rest-x
     (define (resolve-clauses rest-x rest-y)
       (cond
         [(empty? rest-x) NOT-UNIFIABLE]
         [else (resolve-clauses/#unify rest-x rest-y)])))
    
    (resolve-clauses x y)))
;; TESTS
(begin-for-test
  (check-equal? (resolve (list (make-pos 'P)) (list (make-pos 'Q)))
                NOT-UNIFIABLE "the two clauses can not be unified")
  (check-equal? (resolve (list (make-pos 'P)) (list (make-neg 'P))) '()
                "the two clauses have led to null")
  (check-equal? (resolve (list (make-pos 'P) (make-neg 'Q))
                         (list (make-pos 'Q) (make-neg 'R)))
                (list (make-pos 'P) (make-neg 'R))
                "the two clauses were resolvable and were unified")
  (check-equal? (resolve (list (make-pos 'P) (make-neg 'Q))
                         (list (make-pos 'Q) (make-neg 'P)))
                (list (make-neg 'Q) (make-pos 'Q))
                "the two clauses were resolvable and were unified
                 leaving complimentary literals")
  (check-equal? (resolve (list APOS BPOS) (list ANEG BNEG))
                (list BPOS BNEG) "the two clauses were unified"))

;; make-clause : ListOfLiteral -> Clause
;; GIVEN: a list of literals, possibly with duplications
;; RETURNS: a clause containing exactly those literals
;; EXAMPLES:
;; (make-clause '()) = '()
;; (make-clause DUP-LOL) = SOL-FROM-DUP-LOL
;; DESIGN STRATEGY: Use HOF foldr on literals
(define (make-clause literals)
  (local
    (;; literals-differ? : Literal Literal -> Boolean
     ;; GIVEN: two literals
     ;; RETURNS: true iff the given literals differ from each other
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (literals-differ? l1 l2)
       (not (equal? l1 l2)))
     
     ;; lol-except-literal : LOL Literal -> SOL
     ;; GIVEN: a LOL and a Literal
     ;; RETURNS: a SOL by removing the literal, if it exists, from the LOL
     ;; DESIGN STRATEGY: Use HOF filter on lol
     (define (lol-except-literal lol literal)
       (filter
        ;; Literal -> Boolean
        ;; GIVEN: a Literal
        ;; RETURNS: true iff the given l differs from literal
        ;; DESIGN STRATEGY: Combine simpler functions
        (lambda (l) (literals-differ? l literal)) lol)))
    
    (foldr
     ;; Literal LOL -> Clause
     ;; GIVEN: a Literal and a LOL
     ;; RETURNS: a Clause that has the given LOL but with duplicates removed
     ;; DESIGN STRATEGY: Combine simpler functions
     (lambda (literal lol) (cons literal (lol-except-literal lol literal)))
     '() literals)))
;; TESTS
(begin-for-test
  (check-equal? (make-clause '()) '()
                "empty list of literals")
  (check-equal? (make-clause DUP-LOL) SOL-FROM-DUP-LOL
                "duplicate removed from given list of literals")
  (check-equal? (make-clause SOL-FROM-DUP-LOL) SOL-FROM-DUP-LOL
                "no duplicated removed from given unique list of literals"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
