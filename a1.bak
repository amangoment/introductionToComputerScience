;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname a1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image )

;;;;;;;;;;;;;;;
;; Problem 1
;;;;;;;;;;;;;;;
;the smallest natural number
(define zero 0)

;some positive integer with 20 or more digits
(define big-number 11111111111111111111)

;the username on your C211 Handin account
(define handin-name "mangechen")

;the day your lab meets
(define lab-day "Thursday")

;your favorite color of the rainbow
(define favorite-rainbow-color "yellow")

;;;;;;;;;;;;;;;
;; Problem 2
;;;;;;;;;;;;;;;
;a bigger one by tripling the big-number
(define bigger-number (* 3 big-number))

;raising the big-number to power 1000
(define even-bigger-number (expt big-number 1000))

;;;;;;;;;;;;;;;
;; Problem 3
;;;;;;;;;;;;;;;
(define lab-reminder
  (string-append "Reminder for " handin-username ": your C211 lab is on " lab-day "."))

;;;;;;;;;;;;;;;
;; Problem 4
;;;;;;;;;;;;;;;
(define d (circle 10 "solid" favorite-rainbow-color))
(define row-one (beside d d d d d))
(define row-two (beside d d d d))
(define row-three (beside d d d))
(define row-four (beside d d))

; a cannonballs that creates an image
(define cannonballs (above d row-four row-three row-two row-one))