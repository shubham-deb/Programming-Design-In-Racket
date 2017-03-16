;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; q4.rkt 

;; Goal: inserting a character in a string

(require rackunit)
(require "extras.rkt")
(check-location "01" "q4.rkt")
(provide string-insert)

;; DATA DEFINITIONS: none

;; string-insert: string PosInt -> string
;; GIVEN: a string and a positive integer which represents the position on the string
;; WHERE: the position is between 0(inclusive) and the length of the string(exclusive)
;; RETURNS: a string "_" concatenated to the given string at the corresponding position
;; EXAMPLES:
;; (string-insert "hello" 1) => "h ello"
;; (string-insert "" 0) => " "
;; DESIGN STRATEGY: Combine simpler functions

(define (string-insert s pos)
  (if (or (< pos 0) (> pos (string-length s) )) (error 'error: "wrong position entered") 
      (string-append (substring s 0 pos) "_" (substring s pos (string-length s)))
      )
  )

;; TESTS:
(begin-for-test
  (check-equal? (string-insert "hello" 1) "h_ello" )
  (check-equal? (string-insert "" 0) "_" )
  )