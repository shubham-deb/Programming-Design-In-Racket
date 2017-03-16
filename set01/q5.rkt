;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; q5.rkt

;; GOAL: Remove a string of length 1 from a given position in the string

(require "extras.rkt")
(require rackunit)
(check-location "01" "q5.rkt")
(provide string-delete)

;; DATA DEFINITIONS: none

;; FUNCTION DEFINITION
;; string-delete: String PosInt -> String
;; GIVEN: a string and a position which is a positive integer
;; WHERE: the position is between 0(inclusive) and the length of the string(exclusive)
;; RETURNS: the string with a string of length 1 deleted at the corresponding position
;; EXAMPLES:
;; (string-delete "hello" 1) => "hell"
;; (string-delete " " 0) => ""
;; DESIGN STRATEGY: combine simpler functions

(define (string-delete s pos)
  (if (or (< pos 0) (>= pos (string-length s) )) (error 'error: "wrong position entered") 
  (string-append (substring s 0 pos) (substring s (+ pos 1) (string-length s)))
  )
  )

;; TESTS:
(begin-for-test
  (check-equal? (string-delete "hello" 1) "hllo")
  (check-equal? (string-delete " " 0) "")
  )