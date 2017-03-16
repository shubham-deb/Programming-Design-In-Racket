;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; q2.rkt

;; GOAL: Find the last string of length 1 in the string

(require "extras.rkt")
(require rackunit)
(check-location "01" "q2.rkt")
(provide string-last)

;; DATA DEFINITIONS: none

;; FUNCTION DEFINITION
;; string-last: String -> String
;; RETURNS: the last string of length 1 in the given string
;; EXAMPLES:
;; (string-last "hello") => "o"
;; (string-last "hello ") => " "
;; (string-last "hello\n") => "\n"
;; DESIGN STRATEGY: Combine simpler functions

(define (string-last s)
  (string-ith s (-(string-length s) 1))
  )

;; TESTS
(begin-for-test
  (check-equal? (string-last "hello") "o")
  (check-equal? (string-last "hello\n") "\n")
  (check-equal? (string-last " ") " ")
  )