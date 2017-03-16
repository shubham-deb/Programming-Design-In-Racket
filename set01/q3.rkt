;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; q3.rkt

;; GOAL: get the total area of the given image in pixels

(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(check-location "01" "q3.rkt")
(provide image-area)

;; DATA DEFINITIONS:none

;; FUNCTION DEFINITION
;; image-area: image -> RealNumber
;; RETURNS: the total area of the corresponding image in pixels
;; EXAMPLES:
;; (define img (bitmap "cat.png"))
;; (image-area img) => 8775
;; DESIGN STRATEGY: Combine simpler functions

(define (image-area img)
  (* (image-width img) (image-height img))
  )

;; TESTS:
(define img (bitmap "cat.png"))
(begin-for-test
  (check-equal? (image-area img) 8775 "The area of the image is 8775 pixels")
  )