;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; q2.rkt

; GOAL: to design a tester that will accept any good solution to the
;      registrar's original problem, and reject any incorrect solution.

(require rackunit)
(require "extras.rkt")
(check-location "05" "q2.rkt")

(provide make-enrollment
         enrollment-student
         enrollment-class
         make-roster
         roster-classname
         roster-students
         behavior-correct?
         enrollments-to-rosters
         enrollments-to-rosters-bad-1
         enrollments-to-rosters-bad-2
         enrollments-to-rosters-bad-3)

;;DATA DEFINITIONS

(define-struct enrollment (student class))
;; An EnrollmentAssertion is a (make-enrollment Student Class).
;; INTERP:
;; student is unspecified
;; class is unspecified

;; TEMPLATE
;; enrollment-fn : EnrollmentAssertion -> ??
;; (define (enrollment-fn e)
;;   (... (enrollment-student e) (enrollment-class e)))

(define-struct roster (classname students))
; A ClassRosterAssertion is a (make-roster Class SetOfStudent).
; INTERP:
; class is unspecified
; students represents the set of students who are enrolled in class

;; TEMPLATE
;; roster-fn : ClassRosterAssertion -> ??
;; (define (roster-fn r)
;;   (... (roster-classname r) (roster-students r)))

;; A ListOfX is either
;; -- empty
;; -- (cons X ListOfX)

;; TEMPLATE:
;; lox-fn : ListOfX -> ??
;; (define (lox-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... 
;;             (first lst)
;;             (lox-fn (rest lst)))]))

; A SetOfX is a list of X's without duplication.

; SetOfEnrollmentAssertion is a list of unique EnrollmentAssertions
; SetOfClassRosterAssertion is a list of unique ClassRostersAssertions

;; A ProposedSolution is a function with contract
;; SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;; that is, it is a function that takes a SetOfEnrollmentAssertion and
;; produces a SetOfClassRosterAssertion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS FOR TESTING

(define soln1
  (list (make-enrollment "John" "PDP")
        (make-enrollment "Kathryn" "Networks")
        (make-enrollment "Feng" "PDP")
        (make-enrollment "Amy" "PDP")
        (make-enrollment "Sahu" "Networks")
        (make-enrollment "Ankita" "Artificial Intelligence")
        (make-enrollment "Jason" "Artificial Intelligence")))

(define soln4
  (list (make-enrollment "" "PDP")
        (make-enrollment "Kathryn" "Networks")
        (make-enrollment "" "PDP")
        (make-enrollment "Amy" "PDP")
        (make-enrollment "Sahu" "")
        (make-enrollment "" "Artificial Intelligence")
        (make-enrollment "Jason" "Artificial Intelligence")))

(define soln3
  (list (make-enrollment "John" "PDP")
        (make-enrollment "Kathryn" "Networks")
        (make-enrollment "Feng" "PDP")
        (make-enrollment "Amy" "PDP")
        (make-enrollment "Amy" "Networks")
        (make-enrollment "Ankita" "Artificial Intelligence")
        (make-enrollment "Ankita" "PDP")
        (make-enrollment "Jason" "Artificial Intelligence")))

(define soln2
  (list (make-roster "PDP" (list "John" "Feng" "Amy"))
        (make-roster "Networks" (list "Kathryn" "Sahu"))
        (make-roster "Artificial Intelligence" (list "Ankita" "Jason"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FUNCTION DEFINITIONS

; get-list-of-unique-subjects: SetOfEnrollmentAssertion -> SetOfSubjects
; GIVEN: a set of enrollment assertions
; RETURNS:  set of subjects that are in setofenrollmenassertion
; examples:
; (get-list-of-unique-subjects soln1) ->
; (list "PDP" "Networks" "Artificial Intelligence")
; DESIGN STRATEGY: Use HOF foldr on soe
(define (get-list-of-unique-subjects soe)
  (local
    ;; add-unique-subjects: SetOfEnrollmentAssertion SetOfSubjects -> SetOfSubjects
    ;; GIVEN: set of enrollment assertions and set of subjects
    ;; WHERE: set of subjects is initially empty
    ;; RETURNS: a set of subjects that are in SetOfEnrollmentAssertion
    ((define (add-unique-subjects e sos)
       (if (member? (enrollment-class e) sos)
           sos
           (cons (enrollment-class e)  sos))))
    (foldr add-unique-subjects '() soe)))

;;tests
(begin-for-test
  (check-equal? (get-list-of-unique-subjects soln1)
                (list "PDP" "Networks" "Artificial Intelligence")
                "Unique subjects not returned!!!"))

; check-for-equal-class?: String String -> Boolean
; GIVEN: enrolled class and a class 
; RETURNS:  true iff the class is part of the enrolled class
; DESIGN STRATEGY: Combine simpler functions 
(define (check-for-equal-class? ec class)
  (equal? ec class))

; get-set-of-students-in-class: String SetOfEnrollmentAssertion -> SetOfStudents
; GIVEN: a class and a set of enrollment assertions
; RETURNS: set of students that are in each class
; DESIGN STRATEGY: Use HOF foldr on soe
(define (get-set-of-students-in-class c soe)
  (local
    ;; set-of-students: SetOfEnrollmentAssertion SetOfStudents -> SetOfStudents
    ;; GIVEN: set of enrollment assertions and a set of students
    ;; WHERE the set of students is initially empty
    ;; RETURNS: set of students who are enrolled in class
    ((define (set-of-students e sos)
       (if (check-for-equal-class? (enrollment-class e) c)
           (cons (enrollment-student e) sos)
           sos)))
    (foldr set-of-students '() soe)))

;;tests
(begin-for-test
  (check-equal? (get-set-of-students-in-class "PDP" soln1)
                (list "John" "Feng" "Amy")
                "Incorrect set of students are returned!!!"))

; enrollments-to-rosters: SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
; GIVEN: a set of enrollment asssertions
; RETURNS: correct set of class roster assertions
; DESIGN STRATEGY: Use HOF foldr on soe
(define (enrollments-to-rosters soe)
  (local
    ;; list-of-rosters: SetOfEnrollmentAssertion SetOfClassRosters -> SetOfClassRosters
    ;; GIVEN: set of enrollment assertion and a set of rosters
    ;; WHERE the set of class rosters is initially empty
    ;; RETURNS: a set of class rosters 
    ((define (set-of-rosters e sor)
       (cons (make-roster e (get-set-of-students-in-class e soe)) sor)))
    (foldr set-of-rosters '() (get-list-of-unique-subjects soe))))

;;tests
(begin-for-test
  (check-equal? (enrollments-to-rosters soln1) soln2
                "It should be soln2 but it isn't!!!"))

;enrollments-to-rosters-bad-1: SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;enrollments-to-rosters-bad-2: SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;enrollments-to-rosters-bad-3: SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;GIVEN: a set of enrollment assertions
;RETURN: an incorrect set of class rosters for the given enrollments.

; get-list-of-students-in-class-bad-1: String SetOfEnrollmentAssertion -> SetOfStudents
; GIVEN: a set of enrollment assertions and a class
; RETURNS: incorrect set of students in class
; DESIGN STRATEGY: Use HOF foldr on soe
(define (get-set-of-students-in-class-bad-1 c soe)
  (local
    ;; list-of-incorrect-students: SetOfEnrollmentAssertion SetOfStudents -> SetOfStudents
    ;; GIVEN: set of enrollment assertions and a set of students
    ;; WHERE the set of students is initially empty
    ;; RETURNS: a incorrect set of students who are enrolled in a class
    ((define (set-of-incorrect-students e sos)
       (if (or (check-for-equal-class? e c) (member? "shubham" sos))
           sos
           (cons "shubham" sos)) ))
    (foldr set-of-incorrect-students '() (list "" ""))))

; enrollments-to-rosters-bad-1: SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
; GIVEN: a set of enrollment assertions
; RETURNS: an incorrect set of class roster assertions
; examples:see tests below
; DESIGN STRATEGY: Use HOF foldr on soe
(define (enrollments-to-rosters-bad-1 soe)
  (local
    ;; list-of-incorrect-rosters:
    ;; SetOfEnrollmentAssertion SetOfClassRosterAssertion -> SetOfClassRosterAssertion
    ;; GIVEN: set of enrollment assertions and a set of class roster assertions
    ;; WHERE the set of class roster assertions which is initially empty
    ;; RETURNS: incorrect set of class roster assertions
    ((define (list-of-incorrect-rosters e sor)
       (cons (make-roster e (get-set-of-students-in-class-bad-1 e soe)) sor)))
    (foldr list-of-incorrect-rosters '() (get-list-of-unique-subjects soe))))

;;tests
(begin-for-test
  (check-equal? (enrollments-to-rosters-bad-1 soln3)
                (list (make-roster "Networks" (list "shubham"))
                      (make-roster "PDP" (list "shubham"))
                      (make-roster "Artificial Intelligence" (list "shubham")))
                "It should return an incorrect solution but it doesn't!!!!"))

; check-for-equal-class-bad?: String String -> Boolean
; GIVEN: enrolled class and a class 
; RETURNS:  false iff the class is part of the enrolled class
; DESIGN STRATEGY: Combine simpler functions 
(define (check-for-equal-class-bad? ec class)
  (not (equal? ec class)))

; get-set-of-students-in-class-bad: String SetOfEnrollmentAssertion -> SetOfStudents
; GIVEN: a set of enrollment assertions and a class
; RETURNS: incorrect set of students in that class
; DESIGN STRATEGY: Use HOF foldr on soe
(define (get-set-of-students-in-class-bad-2 class soe)
  (local
    ;; list-of-incorrect-rosters-2: SetOfEnrollmentAssertion SetOfStudents -> SetOfStudents
    ;; GIVEN: set of enrollment assertions and a set of students
    ;; WHERE the set of students is initially empty
    ;; RETURNS: incorrect set of students who are enrolled in class
    ((define (set-of-incorrect-rosters-2 e sos)
       (if (not(member? (enrollment-student e) sos))
           (cons (enrollment-student e) sos)
           sos)))
    (foldr set-of-incorrect-rosters-2 '() soe)))

;enrollments-to-rosters-bad-2: SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;GIVEN: a set of enrollment assertions
;RETURNS: in correct set of class roster assertions
;DESIGN STRATEGY: Use HOF foldr on soe
(define (enrollments-to-rosters-bad-2 soe)
  (local
    ;; set-of-incorrect-rosters:
    ;; SetOfEnrollmentAssertion SetOfClassRosterAssertion -> SetOfClassRosterAssertion
    ;; GIVEN: set of enrollment assertion and a set of class rosters
    ;; WHERE the set of class roster assertions is initially empty
    ;; RETURNS: a set of incorrect class roster assertions 
    ((define (set-of-incorrect-rosters e sor)
       (cons (make-roster e (get-set-of-students-in-class-bad-2 e (rest soe))) sor)))
    (foldr set-of-incorrect-rosters '() (get-list-of-unique-subjects soe))))

;;tests
(begin-for-test
  (check-equal?
   (enrollments-to-rosters-bad-2 soln3)
   (list
    (make-roster "Networks" (list "Kathryn" "Feng" "Amy" "Ankita" "Jason"))
    (make-roster "PDP" (list "Kathryn" "Feng" "Amy" "Ankita" "Jason"))
    (make-roster "Artificial Intelligence" (list "Kathryn" "Feng" "Amy" "Ankita" "Jason")))
   "Incorrect solution is not as expected!!!!"))

; get-list-of-students-in-class-bad-3: String SetOfEnrollmentAssertion -> SetOfStudents
; GIVEN: class and a set of enrollment assertions
; RETURNS: incorrect set of students in that class
; DESIGN STRATEGY: Use HOF foldr on soe
(define (get-set-of-students-in-class-bad-3 class soe)
  (local
    ;; set-of-incorrect-rosters: SetOfEnrollmentAssertion SetOfStudents -> SetOfStudents
    ;; GIVEN: set of enrollment assertion and a set of students
    ;; WHERE the set of students is initially empty
    ;; RETURNS: incorrect set of students
    ((define (set-of-incorrect-students e sos)
       (if (and (check-for-equal-class-bad? (enrollment-class e) class)
                (not (member? (enrollment-student e) sos)))
           (cons (enrollment-student e) sos) sos)))
    (foldr set-of-incorrect-students '() soe)))

; enrollments-to-rosters: SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
; GIVEN: a set of enrollment assertions
; RETURNS: an incorrect set of class roster assertions for the given enrollments
; DESIGN STRATEGY: Use HOF foldr on soe
(define (enrollments-to-rosters-bad-3 soe)
  (local
    ;; set-of-incorrect-rosters:
    ;; SetOfEnrollmentAssertion SetOfClassRosterAssertion -> SetOfClassRosterAssertion
    ;; GIVEN: set of enrollment assertion and a set of class roster assertions
    ;; WHERE the set of class roster assertions are initially empty
    ;; RETURNS: incorrect set of class roster assertions
    ((define (set-of-incorrect-rosters e sor)
       (cons (make-roster e (get-set-of-students-in-class-bad-3 e soe)) sor)))
    (foldr set-of-incorrect-rosters '() (get-list-of-unique-subjects soe))))

;;tests
(begin-for-test
  (check-equal?
   (enrollments-to-rosters-bad-3 soln3)
   (list
    (make-roster "Networks" (list "John" "Feng" "Amy" "Ankita" "Jason"))
    (make-roster "PDP" (list "Kathryn" "Amy" "Ankita" "Jason"))
    (make-roster "Artificial Intelligence" (list "John" "Kathryn" "Feng" "Amy" "Ankita")))
   "Incorrect solution is not as expected!!!!"
   ))

;; my-member? : X SetOfX -> Boolean
;; GIVEN: an X and a set of X's
;; RETURNS: true iff the X is an element of the set
;; examples:see tests below
;; STRATEGY: Use HOF ormap on set1
(define (my-member? x set1)
  (ormap
   ;; X -> Boolean
   ;; RETURNS: true iff X is equal to element in set1
   (lambda (elt) (equal? x elt))
   set1))

; tests
(begin-for-test
  (check-equal? (my-member? 1 (list 1 2 3)) #true
                "1 is a member of list but returned false"))

;; subset? : SetOfX SetOfX -> Boolean
;; GIVEN: two set of X's
;; RETURNS: true iff the SetOfX is a subset of the SetOfX
;; examples:see tests below
;; STRATEGY: Use HOF andmap on set1
(define (subset? set1 set2)
  (local
    ;; test: X -> Boolean
    ;; GIVEN: an element
    ;; RETURNS: true iff X is subset of set2
    ((define (test elt) (my-member? elt set2)))
    (andmap test set1)))

;; tests
(begin-for-test
  (check-equal?(subset? (list 1 2) (list 2 3)) #false
               "list (1 2) not a subset of list(1 2 3) but returned true"))

;; set-equal? : SetOfX SetOfX -> Boolean
;; GIVEN: two set of X's
;; RETURNS: true iff the SetOfX is equal to SetOfX
;; STRATEGY: Combine simpler functions
(define (set-equal? set1 set2)
  (and
   (subset? set1 set2)
   (subset? set2 set1)))

;;tests
(begin-for-test
  (check-equal? (subset? (list "amy" "shubham" "lords")
                         (list "amy" "lords" "lords")) #false
                                                       "list 1 not equal to list 2 but returned true")
  (check-equal? (subset? (list "amy" "shubham" "lords")
                         (list "amy" "lords" "shubham")) #true
                                                         "list 1 equal to list 2 but returned false"))
; ProposedSolution: SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
; INTERP:
; SetOfEnrollmentAssertion is a unique set of EnrollmentAssertions.
; SetOfClassRosterAssertion is a unique set of ClassRosterAssertions.

; behavior-correct? : ProposedSolution SetOfEnrollmentAssertion -> Boolean
; GIVEN: a ProposedSolution soln-fn and a SetOfEnrollmentAssertion se
; RETURNS: true iff the output of soln-fn on se is an example of correct
;         behavior by a ProposedSolution.
; EXAMPLES:
; If soln1 is a ProposedSolution, we might have
;  (soln1
;    (list (make-enrollment "John" "PDP")
;          (make-enrollment "Kathryn" "Networks")
;          (make-enrollment "Feng" "PDP")
;          (make-enrollment "Amy" "PDP")
;          (make-enrollment "Amy" "Networks"))) =>
; (list
;   (make-roster "PDP" (list "John" "Feng" "Amy"))
;   (make-roster "Networks" (list "Kathryn" "Amy")))
; This is an example of correct behavior by ProposedSolution fn.
; DESIGN STRATEGY: generalization
(define (behavior-correct? ps soe)
  (set-equal? (ps soe) (enrollments-to-rosters soe)))

;tests
(begin-for-test
  (check-equal? (behavior-correct? enrollments-to-rosters-bad-1 soln1) #false
                "Incorrect solution returned but returned true")
  (check-equal? (behavior-correct? enrollments-to-rosters soln1) #true
                "Correct solution returned but returned false"))