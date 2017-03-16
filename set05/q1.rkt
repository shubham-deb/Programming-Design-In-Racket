;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;q1.rkt

(require rackunit)
(require "extras.rkt")
(check-location "05" "q1.rkt")

(provide felleisen-roster)
(provide shivers-roster)
(provide possible-roster?)
(provide acceptable-felleisen-answer?)
(provide make-slip)
(provide slip-color)
(provide slip-name1)
(provide slip-name2)

;;DATA DEFINITIONS

(define-struct slip (color name1 name2))
;; A Slip is a (make-slip Color String String)
;; INTERPRETATION
;; A Color is one of
;; -- "yellow"
;; -- "blue"
;; name1 and name2 represents the name of the student

;;TEMPLATE
;; slip-fn : Slip -> ??
#|                   
(define (slip-fn s)
  (...
    (slip-color s)
    (slip-name1 s)
    (slip-name2 s)))
|#

;; A List of Slips (LOS) is one of:
;; -- empty
;; -- (cons Slip LOS)

;; Template:
;; los-fn : LOS -> ??
;; (define (los-fn lst)
;;   (cond
;;     [(empty? los) ...]
;;     [else (... (first los)
;;                (los-fn (rest los)))]))

;; END OF DATA DEFINTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define los1 (list
              (make-slip "yellow" "Wang" "Xi")
              (make-slip "blue" "Jones" "Tom")
              (make-slip "yellow" "Xi" "Wang")
              (make-slip "yellow" "Shriram" "K.")))

(define los2 (list
              (make-slip "yellow" "Wang" "Xi")
              (make-slip "yellow" "Jones" "Tom")
              ))

(define los7 (list
              (make-slip "yellow" "Xi" "Wang")
              (make-slip "yellow" "Shriram" "K.")))

(define los3 (list
              (make-slip "yellow" "Wang" "Xi")
              (make-slip "blue" "Jones" "Tom")
              (make-slip "yellow" "Xi" "Wang")
              (make-slip "yellow" "Shriram" "K.")))

(define los8 (list (make-slip "blue" "Jones" "Tom")))

(define los4 (list
              (make-slip "yellow" "Wang" "Xi")
              (make-slip "yellow" "Shriram" "K.")))

(define los5 (list
              (make-slip "yellow" "Wang" "Xi")
              (make-slip "yellow" "Xi" "Wang")))

(define los6 (list
              (make-slip "yellow" "Wang" "Xi")
              (make-slip "yellow" "Shriram" "K.")
              (make-slip "yellow" "Shubham" "K.")
              (make-slip "yellow" "Shubham" "K.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;FUNCTION DEFINITIONS

;; equal-name-slips? : Slip Slip -> Boolean
;; GIVEN: two slips
;; RETURNS: true iff two slips are equal else false
;; examples: see tests below
;; DESIGN STRATEGY:Use template for Slip on slip1 and slip2
(define (equal-name-slips? slip1 slip2)
  (or (and (equal? (slip-name1 slip1) (slip-name1 slip2))
           (equal? (slip-name2 slip1) (slip-name2 slip2)))
      (and (equal? (slip-name1 slip1) (slip-name2 slip2))
           (equal? (slip-name2 slip1) (slip-name1 slip2)))))

;;TESTS
(begin-for-test
  (check-equal?
   (equal-name-slips? (make-slip "yellow" "Wang" "Xi")
                      (make-slip "yellow" "Wang" "Xi")) #true
                      "it should return true but it's not!!!!!"))

;; student-part-of-roster? : Slip ListOfSlip -> Boolean
;; GIVEN: a slip and list of slips
;; RETURNS:a list of slips containing all the students in Professor's
;;         class, without duplication.
;; examples: see tests below
;; DESIGN STRATEGY:Use HOF ormap on restoflos
(define (student-part-of-roster? firstoflos restoflos)
  (ormap 
   ;; Slip -> Boolean
   ;; GIVEN: a slip
   ;; RETURNS: true iff names on slips are equal
   (lambda(slip) (equal-name-slips? firstoflos slip))
   restoflos))

;; DESIGN STRATEGY: call a more general function
(define (felleisen-roster los)
  (professor-roster los "felleisen" "blue"))
(define (shivers-roster los)
  (professor-roster los "shivers" "yellow"))

;; professor-roster : ListOfSlip String String -> ListOfSlip
;; GIVEN: a list of slips, professor's name and the color of the slip
;;        that does not belong to the professor's class
;; RETURNS: a list of slips containing all the students in the Professor's
;;          class 
;; examples: see tests below
;; DESIGN STRATEGY: Use HOF filter on los followed by HOF foldr 
(define (professor-roster los professor color)
  (foldr
   ;; Slip ListOfSlip -> ListOfSlip
   ;; GIVEN: a list of slip and a slip
   ;; WHERE ListOfSlip is initially empty
   ;; RETURNS: the list of slips belonging to professor's class
   (lambda (slip los)
     (if (student-part-of-roster? slip los)
          los
         (cons slip los))) '()
         (filter
          ;; Slip -> Boolean
          ;; GIVEN: a slip
          ;; RETURNS: false iff slip does not belong to professor's class
          (lambda (slip) (not (equal? (slip-color slip) color)))
          los)))

;;TESTS
(begin-for-test
  (check-equal? (felleisen-roster los1 ) los7
                "it should return los7 but it's not!!!!!"))

(begin-for-test
  (check-equal? (shivers-roster los1 ) los8
                "it should return los8 but it's not!!!!!"))

;; possible-roster? : ListOfSlip -> Boolean
;; GIVEN: a list of slips
;; RETURNS: true iff all the slips in the list are the same color,
;;          and no student is represented twice.
;; examples:see tests below
;; DESIGN STRATEGY: Combine simpler functiona
(define (possible-roster? los)
  (or (equal? (professor-roster los "felleisen" "blue") los)
      (equal? (professor-roster los "shivers" "yellow") los)))

;;TESTS
(begin-for-test
  (check-equal? (possible-roster? los4) #true
                "It is a possible roster but given false")
  (check-equal? (possible-roster? los1) #false
                "It is a not possible roster but given true"))

;; blue-color-on-slip?: ListOfSlip -> Boolean
;; GIVEN: a list of slips
;; RETURNS: true iff blue slip is in the list of slips
;;          else false
;; EXAMPLES:shown below
;; DESIGN STRATEGY: use HOF ormap on los2
(define (blue-color-on-slip? los)
  (ormap 
   ;; Slip -> Boolean
   ;; GIVEN: a slip
   ;; RETURNS: true iff the color on the slip is blue
   (lambda(slip) (equal? (slip-color slip) "blue"))
   los))

;;TESTS
(begin-for-test
  (check-equal? (blue-color-on-slip? los1)
                #true "there is a slip with blue color!")
  (check-equal? (blue-color-on-slip? los2)
                #false "there is no slip with blue color!"))

;; equal-names-on-slips? : Slip ListOfSlip -> ListOfSlip
;; GIVEN: a slip and list of slips los2
;; RETURNS: all the slips in los2 that match the name on the slip
;; examples: see tests below
;; DESIGN STRATEGY: Use HOF filter on los2
(define (equal-names-on-slips? slip los2)
  (filter
   ;; Slip -> ListOfSlip
   ;; GIVEN: a slip
   ;; RETURNS: list of slip containing all
   ;;          the names similar to slip
   (lambda(s)
     (equal-name-slips? slip s))
   los2))

;;TESTS
(begin-for-test
  (check-equal? (equal-names-on-slips? (make-slip "yellow" "Wang" "Xi") los2)
                (list (make-slip "yellow" "Wang" "Xi"))
                "no equal slips present but returned more than two slips!!!")
  (check-equal? (equal-names-on-slips? (make-slip "yellow" "Wang" "Xi") los5)
                (list
                 (make-slip "yellow" "Wang" "Xi")
                 (make-slip "yellow" "Xi" "Wang"))
                "equal slips present but returned wrong slips!!"))

;; felleisen-answer? ListOfSlip ListOfSlip -> Boolean
;; GIVEN: two lists of slips, los1 and los2
;; RETURNS: true iff every student in los1 appears once
;;          and only once in los2.
;; examples:see tests below
;; DESIGN STRATEGY: Use HOF andmap on los1
(define (felleisen-answer? los1 los2)
  (local
    ;; Slip -> Boolean
    ;; GIVEN: a slip
    ;; RETURNS: true iff there are no duplicates of slip in los2
    ((define (duplicate-slips? slip)
       (if ( = (length(equal-names-on-slips? slip los2)) 1)
           #true
           #false)))
  (andmap duplicate-slips? los1)))

;; acceptable-felleisen-answer? : ListOfSlip ListOfSlip -> Boolean
;; GIVEN: two lists of slips, los1 and los2
;; RETURNS: true iff every student on a yellow slip in los1 appears once
;;          and only once in los2.
;; examples:see tests below
;; DESIGN STRATEGY: Cases on los2 to check for any blue slips
(define (acceptable-felleisen-answer? los1 los2)
  (if (blue-color-on-slip? los2)
      #false
      (felleisen-answer? (felleisen-roster los1) los2)))

;;TESTS
(begin-for-test
  (check-equal? (acceptable-felleisen-answer? los3 los4) #true
                "Unique elements present in lst2 but given false")
  (check-equal? (acceptable-felleisen-answer? los3 los5) #false
                "Unique elements not present in lst2 but given true")
  (check-equal? (acceptable-felleisen-answer? los3 los6) #true
                "Unique elements present in los6 but given false")
  (check-equal? (acceptable-felleisen-answer? los1 los3) #false
                "blue present in los3 but given true"))