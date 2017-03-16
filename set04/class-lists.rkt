;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname class-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;class-lists.rkt

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(check-location "04" "class-lists.rkt")

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
;; ;; los-fn : LOS -> ??
;; (define (los-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... (first lst)
;;                (los-fn (rest lst)))]))

;;FUNCTION DEFINITIONS

;; check-slips-equal? : ListOfSlip ListOfSlip String -> Boolean
;; GIVEN: a list of slips and the color of the slip which
;;        should not be the part of roster
;; RETURNS: a list of slips containing all the students in Professor
;;         Felleisen's class, without duplication.
;; examples: see tests below
;; DESIGN STRATEGY:Use template for Slip on slip
(define (check-slips-equal? firstoflos restoflos color)
  (if (or (and (equal? (slip-name1 firstoflos) (slip-name1 (first restoflos)))
                (equal? (slip-name2 firstoflos) (slip-name2 (first restoflos))))
           (and (equal? (slip-name1 firstoflos) (slip-name2 (first restoflos)))
                (equal? (slip-name2 firstoflos) (slip-name1 (first restoflos))))
           (equal? (slip-color firstoflos) color))
      #true
      #false))

;; part-of-roster? : ListOfSlip ListOfSlip String -> Boolean
;; GIVEN: a list of slips and the color of the slip which
;;        should not be the part of roster
;; RETURNS: a list of slips containing all the students in Professor
;;         Felleisen's class, without duplication.
;; examples: see tests below
;; DESIGN STRATEGY:Use template for LOS on firstoflos and restoflos
(define (part-of-roster? firstoflos restoflos color)
 (cond
   [(empty? restoflos) #false]
   [else
    (if (equal? (check-slips-equal? firstoflos restoflos color) #true)
     #true
     (part-of-roster? firstoflos (rest restoflos) color))]
   ))


;; felleisen-roster : ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;;         Felleisen's class, without duplication.
;; examples: see tests below
;; DESIGN STRATEGY:Use template for LOS on los
(define (felleisen-roster los)
  (cond
    [(empty? los) empty]
    [(equal? (slip-color (first los)) "blue") (felleisen-roster (rest los))]
    [(empty? (rest los)) los]
    [else
     (if (part-of-roster? (first los) (rest los) "blue")
         (felleisen-roster (rest los))
         (cons (first los) (felleisen-roster (rest los))))]))

;; shivers-roster: ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a losist of slips containing all the students in Professor
;;          Shivers' class, without duplication.
;; examples:see tests below
;; DESIGN STRATEGY: Use template for LOS on los
(define (shivers-roster los)
  (cond
    [(empty? los) empty]
    [(equal? (slip-color (first los)) "yellow") (shivers-roster (rest los))]
    [else
     (if (part-of-roster? (first los) (rest los) "yellow")
         (shivers-roster (rest los))
         (cons (first los) (shivers-roster (rest los))))]))

;; possible-roster? : ListOfSlip -> Boolean
;; GIVEN: a list of slips
;; RETURNS: true iff all the slips in the list are the same color,
;;          and no student is represented twice.
;; examples:see tests below
;; DESIGN STRATEGY: Use template for LOS on los
(define (possible-roster? los)
  (cond
    [(null? (cdr los)) #true]
    [else (if (equal? (first los) (second los))
              (possible-roster? (rest los))
              #false
              )]))

;; equal-slips? : ListOfSlip ListOfSlip -> Boolean
;; GIVEN: a list of slips los1 and los2
;; RETURNS: true iff all the slips in the list are the same color,
;;          and no student is represented twice.
;; examples:see tests below
;; DESIGN STRATEGY: Use template for Slip on slip
(define (equal-slips? los1 los2)
  (cond
    [(empty? los2) #false]
    [else (if (or (and (equal? (slip-name1 (first (felleisen-roster los1))) (slip-name1 (first los2)))
         (equal? (slip-name2 (first (felleisen-roster los1))) (slip-name2 (first los2)))
         (equal? (slip-color(first los2)) "yellow"))   
    (and (equal? (slip-name1 (first (felleisen-roster los1))) (slip-name2 (first los2)))
         (equal? (slip-name2 (first (felleisen-roster los1))) (slip-name1 (first los2)))
         (equal? (slip-color(first los2)) "yellow")))
      (duplicates? (first los1) (rest los2))
      (equal-slips? los1 (rest los2)))]))

;; check-for-duplicates? : ListOfSlip ListOfSlip -> Boolean
;; GIVEN: two slips firstoflos2 and restoflos2
;; RETURNS: true iff all there are duplictae values of
;;          firstoflos2 in restoflos2
;; examples:see tests below
;; DESIGN STRATEGY: Use template for Slip on slip
(define (check-for-duplicates firstoflos1 restoflos2)
(or (and (equal? (slip-name1 firstoflos1) (slip-name1 (first restoflos2)))
            (equal? (slip-name2 firstoflos1) (slip-name2 (first restoflos2))))
       (and (equal? (slip-name1 firstoflos1) (slip-name2 (first restoflos2)))
            (equal? (slip-name2 firstoflos1) (slip-name1 (first restoflos2))))))

;; duplicates? : ListOfSlip ListOfSlip -> Boolean
;; GIVEN: a list of slips los1 and los2
;; RETURNS: true iff all the slips in the list are the same color,
;;          and no student is represented twice.
;; examples:see tests below
;; DESIGN STRATEGY: Use template for LOS on restoflos2
(define (duplicates? firstoflos1 restoflos2)
  (cond
  [(empty? restoflos2) #false]
  [(empty? (cdr restoflos2))
   (if (check-for-duplicates firstoflos1 restoflos2)
      #true
      #false)
   ]
  [else (if (check-for-duplicates firstoflos1 (rest restoflos2))
      #true
      (duplicates? firstoflos1 (rest restoflos2)))]))

;;tests
(begin-for-test
  (check-equal? (duplicates? (make-slip "yellow" "Xi" "Wang") lst5) #true "Blue slip present and duplicate value present but still accepted!")
)

(define (equal-color? los2)
  (cond
    [(empty? los2) #false]
    [else (if (equal? (slip-color (first los2)) "blue")
        #true
        (equal-color? (rest los2)))]))

;; acceptable-felleisen-answer? : ListOfSlip ListOfSlip -> Boolean
;; GIVEN: two lists of slips, lst1 and lst2
;; RETURNS: true iff every student on a yellow slip in lst1 appears once
;;          and only once in lst2.
;; examples:see tests below
;; DESIGN STRATEGY: Use template for LOS on los1 and los2
(define (acceptable-felleisen-answer? los1 los2)
(cond
  [(empty? (felleisen-roster los1)) #true]
  [else
    (if (or (equal-slips? los1 los2) (equal-color? los2))
            #false
            (acceptable-felleisen-answer? (rest (felleisen-roster los1)) los2))
    ]))


;; TESTS
(define lst1 (list
              (make-slip "yellow" "Wang" "Xi")
              (make-slip "yellow" "Xi" "Xi")
              (make-slip "yellow" "Xi" "Xi")
              (make-slip "blue" "Jones" "Tom")
              (make-slip "yellow" "Xi" "Wang")
              (make-slip "yellow" "Shriram" "K.")))

(define lst4 (list
              (make-slip "yellow" "Wang" "Xi")
              (make-slip "blue" "Jones" "Tom")
              (make-slip "yellow" "Xi" "Wang")
              (make-slip "yellow" "Shriram" "K.")))

 (define lst2 (list
 (make-slip "yellow" "Xi" "Wang")
 (make-slip "yellow" "Shriram" "K.")))

 (define lst6 (list
 (make-slip "yellow" "Wang" "Xi")
 (make-slip "yellow" "Shriram" "K.")))

 (define lst5 (list
 (make-slip "yellow" "Xi" "Wang")
 (make-slip "yellow" "Wang" "Xi")))

  (define lst3 (list
 (make-slip "yellow" "Xi" "Wang")
 (make-slip "blue" "Shriram" "K.")))
