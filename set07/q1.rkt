;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Goal: To write a pretty-printer which will convert
;;       the GarterSnake syntax-tree representation,
;;       from Lesson 7.4, into a nicely indented format.

(require rackunit)
(require "sets.rkt")
(require "extras.rkt")

(check-location "07" "q1.rkt")

(provide program-to-strings)
(provide make-def)
(provide make-varexp)
(provide make-appexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;; definition and empty string constants
(define DEF-STR "def")
(define EMPTY-STRING "")

;; parenthesis constants
(define PAREN-OPEN "(")
(define PAREN-CLOSE ")")

;; printing character constants
(define SPACE " ")
(define COMMA ",")
(define COLON ":")
(define COMMA-SPACE (string-append COMMA SPACE))

;; initial indent to definition body
(define INIT-INDENT-TO-BODY 4)
(define INIT-INDENT-TO-APPEXP 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

;; A Variable is a Symbol.

;; A ListOfVariable (LOV) is one of:
;; -- empty
;;    interp: a sequence of Variable with no elements
;; -- (cons Variable LOV)
;;    interp: (cons Variable LOV) represents a sequence of Variable
;;            whose first element is a Variable and whose
;;            other elements are represented by a LOV.
;; INTERP: is a list of variables.

;; TEMPLATE:
;; lov-fn : LOV -> ??
;; HALTING MEASURE: (length vars)
#; (define (lov-fn vars)
     (cond
       [(empty? vars) ...]
       [else (... (first vars)
                  (lov-fn (rest vars)))]))
;; EXAMPLES:
(define THREE-VARIABLES (list 'x 'y 'z))

(define-struct varexp (name))
;; A VarExp is a (make-varexp Variable)
;; name is the name of the variable
;; INTERP: represents a variable expression

;; TEMPLATE:
#; (define (varexp-fn vexp)
     (... (varexp-name vexp)))

;; EXAMPLES:
(define VEXP-X (make-varexp 'x))
(define VEXP-Y (make-varexp 'y))
(define VEXP-Z (make-varexp 'z))

(define-struct appexp (fn args))
;; An AppExp is a (make-appexp Variable ListOfExp)
;; fn is the name of the function being applied
;; args is the list of arguments to the function
;; INTERP: represents a function application expression

;; TEMPLATE:
#; (define (appexp-fn aexp)
     (... (appexp-fn aexp)
          (loe-fn (appexp-args aexp))))

;; EXAMPLES:
(define AEXP-F1-X (make-appexp 'f1 (list VEXP-X)))
(define AEXP-F1-Y (make-appexp 'f1 (list VEXP-Y)))
(define AEXP-F2   (make-appexp 'f2 (list VEXP-Z VEXP-Y)))
(define AEXP-F1-F2-Z (make-appexp 'f1 (list AEXP-F2 VEXP-Z)))

;; An Exp is one of
;; -- (make-varexp Variable)
;; -- (make-appexp Variable ListOfExp)
;; INTERPRETATION;
;; (make-varexp v)                   represents a use of the variable v
;; (make-appexp f (list e1 ... en))  represents a call to the function
;;                                   named f, with arguments e1,..,en

;; TEMPLATE:
#; (define (exp-fn e)
     (cond
       [(varexp? e) (... (varexp-fn e))]
       [(appexp? e) (... (appexp-fn e))]))

;; EXAMPLES:
;; same as defined above

;; A ListOfExp (LOE) is one of:
;; -- empty
;;    interp: a sequence of Exp with no elements
;; -- (cons Exp LOE)
;;    interp: (cons Exp LOE) represents a sequence of Exp
;;            whose first element is a Exp and whose
;;            other elements are represented by a LOE.
;; INTERP: is a list of expressions.

;; TEMPLATE:
;; loe-fn : LOE -> ??
;; HALTING MEASURE: (length exps)
#; (define (loe-fn exps)
     (cond
       [(empty? exps) ...]
       [else (... (exp-fn (first exps))
                  (loe-fn (rest exps)))]))

(define-struct def (name args body))
;; A Definition is a (make-def Variable ListOfVariable Exp)
;; name is the name of the function being defined
;; args is the list of arguments of the function
;; body is the body of the function
;; INTERP: represents a definition in the GarterSnake syntax-tree representation

;; TEMPLATE:
;; def-fn : Definition -> ??
#; (define (def-fn d)
     (... (def-name d)
          (def-args d)
          (def-body d)))

;; EXAMPLES:
;; a definition with no arguments
(define DEF-NO-ARGS (make-def 'a-not-so-long-name '() (make-varexp 'x)))

;; a definition with a very long definition name
(define DEF-LONG-NAME (make-def 'a-very-long-function-name (list 'x) AEXP-F1-X))

;; a definition with multiple arguments
(define DEF-MULTIPLE-ARGS (make-def 'multiple-args (list 'x 'y 'z 'a) AEXP-F2))

;; a definition with many arguments that requires to be stacked
(define DEF-STACK-ARGS (make-def 'stack-args (list 'u 'x 'y 'z 'a) AEXP-F2))

;; A ListOfDefinition (LOD) is one of:
;; -- empty
;;    interp: a sequence of Definition with no elements
;; -- (cons Definition LOD)
;;    interp: (cons Definition LOD) represents a sequence of Definition
;;            whose first element is a Definition and whose
;;            other elements are represented by a LOD.
;; INTERP: is a list of definitions.

;; TEMPLATE:
;; lod-fn : LOD -> ??
;; HALTING MEASURE: (length defs)
#; (define (lod-fn defs)
     (cond
       [(empty? defs) ...]
       [else (... (def-fn (first defs))
                  (lod-fn (rest defs)))]))

;; A Program is a ListOfDefinition.

;; EXAMPLES:
;; a sample program as demonstrated on the course page
(define SAMPLE-PROGRAM
  (list
   DEF-LONG-NAME
   DEF-NO-ARGS
   (make-def 'f2 (list 'x 'a-very-long-variable-name 'y) AEXP-F1-Y)
   (make-def 'f3 (list 'x 'z 't 'u)
             (make-appexp 'f1 (list AEXP-F2 VEXP-Z AEXP-F1-F2-Z)))))

;; A ListOfString (LOS) is one of:
;; -- empty
;;    interp: a sequence of String with no elements
;; -- (cons String LOS)
;;    interp: (cons String LOS) represents a sequence of String
;;            whose first element is a String and whose
;;            other elements are represented by a LOS.
;; INTERP: is a list of strings.

;; TEMPLATE:
;; los-fn : LOS -> ??
;; HALTING MEASURE: (length los)
#; (define (los-fn los)
     (cond
       [(empty? los) ...]
       [else (... (first los)
                  (los-fn (rest los)))]))

;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; output of sample program pretty printed at width 50
(define PRETTY-PRINTED-SAMPLE-PROGRAM-AT-WIDTH-50
  (list
   "def a-very-long-function-name (x) :"
   "    f1(x)"
   "def a-not-so-long-name () :"
   "    x"
   "def f2 (x,a-very-long-variable-name,y) :"
   "    f1(y)"
   "def f3 (x,z,t,u) :"
   "    f1(f2(z, y), z, f1(f2(z, y), z))"))

;; output of sample program pretty printed at width 20
(define PRETTY-PRINTED-SAMPLE-PROGRAM-AT-WIDTH-20
  (list
   "def a-very-long-function-name (x) :"
   "    f1(x)"
   "def a-not-so-long-name () :"
   "def f2 (x,"
   "        a-very-long-variable-name,"
   "        y) :"
   "    f1(y)"
   "def f3 (x,z,t,u) :"
   "    f1(f2(z, y),"
   "       z,"
   "       f1(f2(z, y),"
   "          z))"))

;; output of a definition that requires to be stacked for pretty printing
(define PRETTY-PRINTED-STACKED-DEF
  (list
   "def stack-args (u,"
   "                x,"
   "                y,"
   "                z,"
   "                a) :"
   "    f2(z, y)"))

;; program-to-strings : Program PosInt -> ListOfString
;; GIVEN: a GarterSnake program and a width
;; RETURNS: a representation of the program as a sequence of lines,
;; following the formatting rules described in the question.
;; EXAMPLES:
;; (program-to-strings SAMPLE-PROGRAM 20) = PRETTY-PRINTED-SAMPLE-PROGRAM
;; DESIGN STRATEGY: Use HOF foldr on p
(define (program-to-strings p w)
  (foldr
   ;; Definition LOS -> LOS
   ;; GIVEN: a Definition and a ListOfString
   ;; RETURNS: a ListOfString just like the original one but with
   ;; definitions pretty printed as strings
   ;; DESIGN STRATEGY: Combine simpler functions
   (lambda (d lst) (flatten (list (defn-to-strings d w) lst))) '() p))
;; TESTS
(begin-for-test
  (check-equal?  (program-to-strings SAMPLE-PROGRAM 20)
                 PRETTY-PRINTED-SAMPLE-PROGRAM-AT-WIDTH-20
                 "sample program pretty printed as strings at width 20")
  (check-equal?  (program-to-strings SAMPLE-PROGRAM 50)
                 PRETTY-PRINTED-SAMPLE-PROGRAM-AT-WIDTH-50
                 "sample program pretty printed as strings at width 50"))

;; defn-to-strings : Definition PosInt -> ListOfString
;; GIVEN: a Definition and a width
;; RETURNS: a representation of the Definition as a sequence of lines,
;; following the formatting rules described in the question.
;; EXAMPLES:
;; (defn-to-strings DEF-LONG-NAME 20)
;; = (list "def a-very-long-function-name (x) :" "    f1(x)")
;; DESIGN STRATEGY: Cases on whether or not the definition
;; header fit on one line depending on the given width
(define (defn-to-strings d w)
  (local
    (;; the arguments of the given definition 
     (define ARGS (def-args d))
     
     ;; has-only-one-arg? : ListOfVariable -> Boolean
     ;; GIVEN: a ListOfVariable representing the arguments of the definition
     ;; RETURNS: true iff there is only one argument in the given list
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (has-only-one-arg? args)
       (= (length args) 1))
     
     ;; fits-defn-width? : Definition PosInt -> Boolean
     ;; GIVEN: a Definition and a width
     ;; RETURNS: true iff the given Definition fits the given width
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (fits-defn-width? d w)
       (< (string-length (defn-to-string d)) w))
     
     ;; print-defn-header-in-one-line : Definition PosInt -> LOS
     ;; GIVEN: a Definition and a width
     ;; RETURNS: a LOS representing the definition header in one line
     ;; of the given width, followed by the body of the definition
     ;; DESIGN STRATEGY: Use template for Definition on d
     (define (print-defn-header-in-one-line d w)
       (cons (defn-to-string d)
             (stack-def-body (def-body d) INIT-INDENT-TO-BODY w))))
    
    (if (or (fits-defn-width? d w) (has-only-one-arg? ARGS))
        (print-defn-header-in-one-line d w)
        (stack-defn-to-strings d w))))
;; TESTS
(begin-for-test
  (check-equal? (defn-to-strings DEF-LONG-NAME 20)
                (list "def a-very-long-function-name (x) :" "    f1(x)")
                "pretty print a defn with very long function name"))

;; defn-to-string : Definition -> String
;; GIVEN: a Definition
;; RETURNS: a String representation of the given Definition d
;; EXAMPLES:
;; (defn-to-string DEF-MULTIPLE-ARGS) = "def multiple-args (x,y,z,a) :"
;; DESIGN STRATEGY: Combine simpler functions
(define (defn-to-string d)
  (local
    (;; the name of the given definition as a string
     (define NAME (symbol->string (def-name d)))
     
     ;; the arguments of the given definition
     (define ARGS (def-args d))
     
     ;; the start and end pretty blocks of a definition
     (define START (string-append DEF-STR SPACE NAME SPACE PAREN-OPEN))
     (define END (string-append PAREN-CLOSE SPACE COLON))
     
     ;; comma-with-arg : Variable LOS -> LOS
     ;; GIVEN: an argument of the definition and a LOS
     ;; WHERE: (length lst) always increases
     ;; RETURNS: a LOS after appending a comma with the given argument
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (comma-with-arg arg lst)
       (cons COMMA (cons (symbol->string arg) lst)))
     
     ;; comma-with-args : LOV -> LOS
     ;; GIVEN: a list of arguments of the definition
     ;; RETURNS: a list of string representing the given arguments with commas
     ;; DESIGN STRATEGY: Use HOF foldr on args
     (define (comma-with-args args)
       (foldr
        ;; Variable LOS -> LOS
        ;; GIVEN: a Variable and a LOS
        ;; RETURNS: a LOS representing the given argument with a comma
        ;; DESIGN STRATEGY: Combine simpler functions
        (lambda (arg lst) (comma-with-arg arg lst)) '() args))
     
     ;; args-as-los : LOV -> LOS
     ;; GIVEN: a list of arguments of the definition
     ;; RETURNS: a list of string representing the given arguments
     ;; HALTING MEASURE: (length args)
     ;; DESIGN STRATEGY: Use template for LOV on args
     (define (args-as-los args)
       (cond
         [(empty? args) args]
         [else (cdr (comma-with-args args))]))
     
     ;; def-args-as-string : LOV -> String
     ;; GIVEN: a LOV
     ;; RETURNS: a String representing the given list of arguments
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (def-args-as-string args)
       (string-append* (args-as-los args))))
    
    (string-append START (def-args-as-string ARGS) END)))
;; TESTS
(begin-for-test
  (check-equal? (defn-to-string DEF-MULTIPLE-ARGS)
                "def multiple-args (x,y,z,a) :"
                "pretty print a defn with multiple arguments"))

;; stack-defn-to-strings : Definition PosInt -> LOS
;; GIVEN: a Definition and a width
;; RETURNS: a LOS that represents the given definition
;; EXAMPLES:
;; (stack-defn-to-strings DEF-STACK-ARGS 20) = PRETTY-PRINTED-STACKED-DEF
;; DESIGN STRATEGY: Use template for Definition on d
(define (stack-defn-to-strings d w)
  (local
    (;; name of the definition as a string
     (define NAME (symbol->string (def-name d)))
     
     ;; the start and end pretty blocks for a definition
     (define START (string-append DEF-STR SPACE NAME SPACE PAREN-OPEN))
     (define END (string-append PAREN-CLOSE SPACE COLON))
     
     ;; the representation when there are no arguments in the definition
     (define NO-ARGS (string-append START END))
     
     ;; the indendation required with regard to the definition name
     (define INDENT (string-length START))
     
     ;; indent-arg : Variable String -> String
     ;; GIVEN: a Variable representing argument and a trail
     ;; representing the next String to associate with the given Variable
     ;; RETURNS: a String representing the given Variable with appropriate
     ;; indendation and trail associated with it
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (indent-arg arg trail)
       (string-append (replicate INDENT SPACE) (symbol->string arg) trail))
     
     ;; stack-def-args : LOV PosInt -> LOS
     ;; GIVEN: a LOV representing the arguments of the definition
     ;; other than the first argument, and a PosInt representing
     ;; the number of arguments processed so far
     ;; WHERE: nargs-so-far always decreases
     ;; RETURNS: a LOS representing the arguments of the definition
     ;; indented appropriately
     ;; HALTING MEASURE: (length args)
     ;; DESIGN STRATEGY: Use template for LOV on args
     (define (stack-def-args args nargs-so-far)
       (if (= nargs-so-far 1)
           (list (indent-arg (first args) END))
           (cons (indent-arg (first args) COMMA)
                 (stack-def-args (rest args) (sub1 nargs-so-far)))))
     
     ;; stack-def-with-args : Variable LOV Exp -> LOS
     ;; GIVEN: a Variable representing the first argument of the definition, 
     ;; a LOV representing the other arguments of the definition, and a Exp
     ;; representing the body of the definition
     ;; RETURNS: a LOS representing the arguments and body of the definition
     ;; stacked one below the other in a pretty printed manner
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (stack-def-with-args arg args body)
       (append (list (string-append START (symbol->string arg) COMMA))
               (stack-def-args args (length args))
               (stack-def-body body INIT-INDENT-TO-BODY w)))
     
     ;; stack-def : LOV Exp -> LOS
     ;; GIVEN: a LOV representing the list of arguments of the definition
     ;; and a Exp representing the body of the definition
     ;; RETURNS: a LOS representing the definition in a pretty printed manner
     ;; HALTING MEASURE: (length args)
     ;; DESIGN STRATEGY: Use template for LOV on args
     (define (stack-def args body)
       (cond
         [(empty? args) (list NO-ARGS)]
         [else (stack-def-with-args (first args) (rest args) body)])))
    
    (stack-def (def-args d) (def-body d))))
;; TESTS
(begin-for-test
  (check-equal? (stack-defn-to-strings DEF-STACK-ARGS 20)
                PRETTY-PRINTED-STACKED-DEF
                "a long def pretty printed by stacking across multiple lines"))

;; stack-def-body : Exp PosInt PosInt -> LOS
;; GIVEN: a Exp, the number of spaces to indent and the width
;; RETURNS: a LOS that'd pretty print the given Exp e
;; EXAMPLES:
;; (stack-def-body VEXP-X 0 20) = (list "x")
;; (stack-def-body (make-appexp 'f1 (list AEXP-F2 VEXP-Z)) 0 20)
;; = (list "f1(f2(z, y), z)")
;; DESIGN STRATEGY: Use template for Exp on e
(define (stack-def-body e indent w)
  (local
    (;; stack-def-body/varexp : VarExp PosInt -> LOS
     ;; GIVEN: a VarExp and the number of spaces to indent
     ;; RETURNS: a LOS that'd pretty print the given VarExp vexp
     ;; DESIGN STRATEGY: Use template for VarExp on vexp
     (define (stack-def-body/varexp vexp indent)
       (list  (string-append (replicate indent SPACE)
                             (symbol->string (varexp-name vexp))))))
    
    (cond
      [(varexp? e) (stack-def-body/varexp e indent)]
      [(appexp? e) (stack-def-body/appexp e indent w)])))
;; TESTS
(begin-for-test
  (check-equal? (stack-def-body VEXP-X 0 20) (list "x")
                "pretty print a varexp stacked one after the other")
  (check-equal? (stack-def-body (make-appexp 'f1 (list AEXP-F2 VEXP-Z)) 0 20)
                (list "f1(f2(z, y), z)")
                "pretty print a appexp stacked one after the other"))

;; stack-def-body/appexp : AppExp PosInt PosInt -> LOS
;; GIVEN: an AppExp representing the function call, 
;; the number of spaces to indent, and the width
;; RETURNS: a LOS that represents the AppExp in a pretty printed manner
;; EXAMPLES:
;; (stack-def-body/appexp AEXP-F1-F2-Z 4 20) = (list "    f1(f2(z, y), z)")
;; DESIGN STRATEGY: Cases on whether or not the given
;; AppExp fits the the given width
(define (stack-def-body/appexp aexp indent w)
  (if (appexp-fits-width? aexp w indent PAREN-CLOSE)
      (list (appexp-to-string aexp indent PAREN-CLOSE))
      (stack-appexp-to-strings aexp indent w)))
;; TESTS
(begin-for-test
  (check-equal? (stack-def-body/appexp AEXP-F1-F2-Z 4 20)
                (list "    f1(f2(z, y), z)")
                "pretty print a appexp that fits width"))

;; appexp-fits-width? : AppExp PosInt -> Boolean
;; GIVEN: an AppExp and a width
;; RETURNS: true iff the given AppExp fits the given width
;; EXAMPLES:
;; (appexp-fits-width? AEXP-F1-F2-Z 20 4 PAREN-CLOSE) = #true
;; DESIGN STRATEGY: Combine simpler functions
(define (appexp-fits-width? aexp w indent end)
  (< (string-length (appexp-to-string aexp indent end)) w))
;; TESTS
(begin-for-test
  (check-equal? (appexp-fits-width? AEXP-F1-F2-Z 20 4 PAREN-CLOSE)
                #true "appexp fits given width"))

;; appexp-to-string : AppExp PosInt String -> String
;; GIVEN: an AppExp, the number of spaces to indent
;; and the ending string on the expression
;; RETURNS: a String that pretty prints the given AppExp aexp
;; EXAMPLES:
;; (appexp-to-string AEXP-F1-F2-Z 2 PAREN-CLOSE) = "  f1(f2(z, y), z)"
;; DESIGN STRATEGY: Combine simpler functions
(define (appexp-to-string aexp indent end)
  (local
    (;; the name of the function definition called as a string
     (define FN (symbol->string (appexp-fn aexp)))
     
     ;; the start and end pretty blocks of the function call
     (define START (string-append FN PAREN-OPEN))
     
     ;; comma-with-exp : Exp -> String
     ;; GIVEN: a Exp
     ;; RETURNS: the given Exp as a String
     ;; DESIGN STRATEGY: Use template for Exp on e
     (define (comma-with-exp e)
       (cond
         [(varexp? e) (symbol->string (varexp-name e))]
         [(appexp? e) (appexp-to-string e INIT-INDENT-TO-APPEXP PAREN-CLOSE)]))
     
     ;; indent-exp : Exp String -> String
     ;; GIVEN: a Exp representing an expression and a trail
     ;; representing the next String to associate with the given Exp
     ;; RETURNS: a String representing the given Exp with appropriate
     ;; indendation and trail associated with it
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (indent-exp arg trail)
       (string-append (comma-with-exp arg) trail))
     
     ;; appexp-args-as-los : LOE PosInt -> LOS
     ;; GIVEN: a LOE representing the arguments of the AppExp,
     ;; and a PosInt representing the number of expressions processed so far
     ;; WHERE: nargs-so-far always decreases
     ;; RETURNS: a LOS representing the expressions of the AppExp
     ;; indented appropriately
     ;; HALTING MEASURE: (length loe)
     ;; DESIGN STRATEGY: Use template for LOE on args
     (define (appexp-args-as-los loe nargs-so-far)
       (if (= nargs-so-far 1)
           (list (indent-exp (first loe) end))
           (cons (indent-exp (first loe) COMMA-SPACE)
                 (appexp-args-as-los (rest loe) (sub1 nargs-so-far)))))
     
     ;; appexp-as-string : LOE -> String
     ;; GIVEN: a LOE
     ;; RETURNS: a String representing the given list of expressions
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (appexp-as-string args)
       (string-append* (appexp-args-as-los args (length args)))))
    
    (string-append (replicate indent SPACE) START
                   (appexp-as-string (appexp-args aexp)))))
;; TESTS
(begin-for-test
  (check-equal? (appexp-to-string AEXP-F1-F2-Z 2 PAREN-CLOSE)
                "  f1(f2(z, y), z)" "indented given appexp to string"))

;; stack-appexp-to-strings : AppExp PosInt PosInt -> LOS
;; GIVEN: an AppExp, the number of spaces to indent and the width
;; RETURNS: a LOS that represents the AppExp in a pretty printed manner
;; EXAMPLES:
;; (stack-appexp-to-strings AEXP-F2 0 4) = (list "f2" " (z," " y)")
;; DESIGN STRATEGY: Cases on whether or not the first argument
;; of the given AppExp fits the the given width
(define (stack-appexp-to-strings aexp indent w)
  (local
    (;; the name of the function called
     (define FN-APPEXP (appexp-fn aexp))
     (define FN (symbol->string FN-APPEXP))
     
     ;; the function name indented with the function name length
     (define FN-INDENT (+ (string-length FN) (string-length PAREN-OPEN)))
     
     ;; the arguments of the function application
     (define ARGS (appexp-args aexp))
     (define FIRST-ARG (first ARGS))
     (define FIRST-ARG-AS-LIST (list FIRST-ARG))
     (define REST-ARGS (rest ARGS))
     
     ;; the first argument along with the function name as AppExp
     (define FIRST-ARG-APPEXP (make-appexp FN-APPEXP FIRST-ARG-AS-LIST))
     
     ;; stack-after-first-argument : Exp PosInt -> LOS
     ;; GIVEN: a Exp, the number of spaces to indent
     ;; RETURNS: a LOS that represents the AppExp with the given arg
     ;; in a pretty printed manner
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (stack-after-first-argument arg indent)
       (cons (appexp-to-string arg indent COMMA)
             (stack-appexp-to-strings/stack-all
              REST-ARGS (+ indent FN-INDENT) w (length REST-ARGS))))
     
     ;; stack-all-arguments : LOE PosInt -> LOS
     ;; GIVEN: a LOE, the number of spaces to indent
     ;; RETURNS: a LOS that represents the AppExp with the given args
     ;; in a pretty printed manner
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (stack-all-arguments args indent)
       (local
         (;; all stacked arguments except first open parenthesis
          (define STACKED-ARGS (stack-appexp-to-strings/stack-all
                                args (add1 indent) w (length args)))
          
          ;; all stacked arguments with first open parenthesis corrected
          (define CORRECTED-STACKED-ARGS
            (cons (string-append SPACE PAREN-OPEN
                                 (substring (first STACKED-ARGS) 1))
                  (rest STACKED-ARGS))))
         
         (cons FN CORRECTED-STACKED-ARGS))))
    
    (if (appexp-fits-width? FIRST-ARG-APPEXP w indent COMMA)
        (stack-after-first-argument FIRST-ARG-APPEXP indent)
        (stack-all-arguments ARGS indent))))
;; TESTS
(begin-for-test
  (check-equal? (stack-appexp-to-strings AEXP-F2 0 4) (list "f2" " (z," " y)")
                "given appexp doesn't fit given width, stack all args"))

;; stack-appexp-to-strings/stack-all : LOE PosInt PosInt PosInt -> LOS
;; GIVEN: a LOE represents the list of expressions,
;; the number of spaces to indent, a given width,
;; the number of expressions processed so far
;; WHERE: nargs-so-far always decreases
;; RETURNS: a LOS representing the arguments of the AppExp
;; EXAMPLES:
;; (stack-appexp-to-strings/stack-all
;;  (appexp-args AEXP-F2) 0 5 (length (appexp-args AEXP-F2))) = (list "z," "y)")
;; HALTING MEASURE: (length args)
;; DESIGN STRATEGY: Use template for LOE on args
(define (stack-appexp-to-strings/stack-all args indent w nargs-so-far)
  (local
    (;; append-trail-to-los : LOS String -> LOS
     ;; GIVEN: a LOS and a String representing a trail
     ;; RETURNS: a LOS just like the original one
     ;; but with the given String appended to the last
     ;; String of the given LOS
     ;; DESIGN STRATEGY: Use template for LOS on (reverse lst)
     (define (append-trail-to-los lst tr)
       (local ((define REV (reverse lst)))
         (reverse (cons (string-append (first REV) tr) (rest REV)))))
     
     ;; indent-varexp : Exp String -> String
     ;; GIVEN: a Exp and a String representing a trail
     ;; RETURNS: a String with the given VarExp indented
     ;; DESIGN STRATEGY: Use template for VarExp on vexp
     (define (indent-varexp vexp tr)
       (string-append (replicate indent SPACE)
                      (symbol->string (varexp-name vexp)) tr))
     
     ;; indent-appexp : Exp String -> LOS
     ;; GIVEN: a Exp and a String representing a trail
     ;; RETURNS: a LOS with the given AppExp indented
     ;; DESIGN STRATEGY: Combine simpler functions
     (define (indent-appexp aexp tr)
       (append-trail-to-los (flatten (stack-def-body/appexp aexp indent w)) tr))
     
     ;; indent-arg : Exp String -> String
     ;; GIVEN: a Exp and a String representing a trail
     ;; RETURNS: a String with the given Exp indented
     ;; DESIGN STRATEGY: Use template for Expression on e
     (define (indent-arg e tr)
       (cond
         [(varexp? e) (indent-varexp e tr)]
         [(appexp? e) (indent-appexp e tr)])))
    
    (if (= nargs-so-far 1)
        (list (indent-arg (first args) PAREN-CLOSE))
        (cons (indent-arg (first args) COMMA)
              (stack-appexp-to-strings/stack-all
               (rest args) indent w (sub1 nargs-so-far))))))
;; TESTS
(begin-for-test
  (check-equal? (stack-appexp-to-strings/stack-all
                 (appexp-args AEXP-F2) 0 5 (length (appexp-args AEXP-F2)))
                (list "z," "y)")
                "stack appexp to strings - without first open parenthesis"))

;; string-append* : LOS -> String
;; GIVEN: a LOS
;; RETURNS: a merged String with each String of LOS appended into one
;; EXAMPLES:
;; (string-append* (list "a" "," "b" "," "c")) = "a,b,c"
;; DESIGN STRATEGY: Use HOF foldr on los
(define (string-append* los)
  (foldr
   ;; String LOS -> String
   ;; GIVEN: a String and a LOS
   ;; RETURNS: a String that has merged all String from LOS into one
   ;; DESIGN STRATEGY: Combine simpler functions
   (lambda (str lst) (string-append str lst)) EMPTY-STRING los))
;; TESTS
(begin-for-test
  (check-equal? (string-append* (list "a" "," "b" "," "c")) "a,b,c"
                "merges the given list into a string"))

;; flatten : Any -> list?
;; GIVEN: Any
;; RETURNS: true iff lst is a list, either the empty list,
;; or a pair whose second element is a list.
;; EXAMPLES:
;; (flatten (list "pdp" 5 4 (list "rocks"))) = (list "pdp 5 4 "rocks")
;; HALTING MEASURE: (length lst)
;; DESIGN STRATEGY: Use template for list? on lst
(define (flatten lst)
  (cond
    [(empty? lst) lst]
    [(not (list? lst)) (list lst)]
    [else (append (flatten (first lst))
                  (flatten (rest lst)))]))
;; TESTS
(begin-for-test
  (check-equal? (flatten (list "pdp" 5 4 (list "rocks")))
                (list "pdp" 5 4 "rocks")
                "flattens a test nested list"))

