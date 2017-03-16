;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname distance-to-origin) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; distance-to-origin.rkt

;; GOAL: find the Euclidean distance of a point(x,y) from the origin

(require "extras.rkt")
(require rackunit)
(check-location "01" "distance-to-origin.rkt")
(provide distance-to-origin)

;; DATA DEFINITION: none

;; FUNCTION DEFINITION
;; distance-to-origin : Number Number -> Number
;; GIVEN : any two numbers
;; WHERE : the numbers are real except +inf.0 (positive infinity),+inf.f (single-precision variant), -inf.0 (negative infinity),
;; -inf.f (single-precision variant), +nan.0 (not-a-number), and +nan.f (single-precision variant).
;; RETURNS : Euclidean distance of the point from the origin(0,0)
;; EXAMPLES:
;; (distance-to-origin 0 1) => 1
;; (distance-to-origin 3 4) => 5
;; DESIGN STRATEGY: Combine simpler functions

(define (distance-to-origin x y)
  (inexact->exact(sqrt(+(sqr x)(sqr y))))
  )

;; TESTS
(begin-for-test
  (check-equal? (distance-to-origin 0 1) 1)
  (check-= (distance-to-origin pi pi) 4.44 0.2 )
  )