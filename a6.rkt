;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname a6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;
;; Problem 1
;;;;;;;;;;;;;;;
;; 1a
(define a-cons (cons 3 (cons 8 (cons 2 empty))))
(define a-list (list 3 8 2))

(check-expect a-cons a-list)

;; 1b
(define b-cons (cons (cons 5 empty)
                     (cons (cons 7 empty)
                           (cons (cons 1 empty) empty))))
(define b-list (list (list 5) (list 7) (list 1)))

(check-expect b-cons b-list)

;; 1c
(define c-cons (cons (cons 1 (cons 2 empty))
                     (cons 7 (cons (cons 5 empty) empty))))
(define c-list (list (list 1 2) 7 (list 5)))

(check-expect c-cons c-list)

;; 1d
(define d-cons (cons 
                     (cons empty
                           (cons 8 (cons (cons (cons 2 empty) empty) empty)))
                     (cons 4 empty)))
(define d-list (list (list '() 8 (list (list 2))) 4))

(check-expect d-cons d-list)

;;;;;;;;;;;;;;;
;; Problem 2
;;;;;;;;;;;;;;;
#|
 
   A NegInt is one of
   - -1
   - (sub1 NegInt)
 
|#

;; 2a
; process-neg-int : NegInt -> ...
; (process-neg-int a-neg-int) returns ...
(define (process-neg-int a-neg-int)
  (cond
    [(= a-neg-int 0) ...]
    [else (... a-neg-int ...
               (process-neg-int (add1 a-neg-int)) ...)]))

;; 2b
; countup : NegInt -> ListOfNum
; (countup n) returns a ListOfNum starting with n and increasing by
; one each time until reaching zero.
(define (countup n)
  (cond
    [(= 0 n) (list 0)]
    [else (cons n
                (countup (add1 n)))]))

(check-expect (countup -1) (list -1 0))
(check-expect (countup -6) (list -6 -5 -4 -3 -2 -1 0))
(check-expect (countup -12)
              (list -12 -11 -10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0))

;; 2c

#|
(countup -5)
== (cons -5 (countup -4))
== (cons -5 (cons -4 (countup -3)))
== (cons -5 (cons -4 (cons -3 (countup -2))))
== (cons -5 (cons -4 (cons -3 (cons -2 (countup -1)))))
== (cons -5 (cons -4 (cons -3 (cons -2 (cons -1 (countup 0))))))
== (cons -5 (cons -4 (cons -3 (cons -2 (cons -1 (list 0))))))
== (cons -5 (cons -4 (cons -3 (cons -2 (list -1 0)))))
== (cons -5 (cons -4 (cons -3 (list -2 -1 0))))
== (cons -5 (cons -4 (list -3 -2 -1 0)))
== (cons -5 (list -4 -3 -2 -1 0))
== (list -5 -4 -3 -2 -1 0)
|#

;; 2d
; Interval : Num Num -> Boolean
; (Interval [a b]) gives the range of number and a is smaller than b.
(define-struct interval [a b])

; explode-interval : Int -> ListOfInt
; (explode-interval interval) returns a list of the integers
; in the interval.
(define (explode-interval interval)
  (cond
    [(equal? (interval-a interval) (interval-b interval))
     (list (interval-a interval))]
    [else (cons (interval-a interval)
                (explode-interval
                 (make-interval (add1 (interval-a interval))
                                (interval-b interval))))]))

(check-expect (explode-interval (make-interval -6 0))
              (list -6 -5 -4 -3 -2 -1 0))
(check-expect (explode-interval (make-interval -6 3))
              (list -6 -5 -4 -3 -2 -1 0 1 2 3))
(check-expect (explode-interval (make-interval 4 5))
              (list 4 5))
(check-expect (explode-interval (make-interval 0 0))
              (list 0))

;; 2e

#|
(explode-interval (make-interval -2 3))
== (cons -2 (explode-interval -1 3))
== (cons -2 (cons -1 (explode-interval 0 3)))
== (cons -2 (cons -1 (cons 0 (explode-interval 1 3))))
== (cons -2 (cons -1 (cons 0 (cons 1 (explode-interval 2 3)))))
== (cons -2 (cons -1 (cons 0 (cons 1 (cons 2
                                         (explode-interval 3 3))))))
== (cons -2 (cons -1 (cons 0 (cons 1 (cons 2 (list 3))))))
== (cons -2 (cons -1 (cons 0 (cons 1 (list 2 3)))))
== (cons -2 (cons -1 (cons 0 (list 1 2 3))))
== (cons -2 (cons -1 (list 0 1 2 3)))
== (cons -2 (list -1 0 1 2 3))
== (list -2 -1 0 1 2 3)
|#

;;;;;;;;;;;;;;;
;; Problem 3
;;;;;;;;;;;;;;;

#|
   A Coin is one of
   - 'penny
   - 'nickel
   - 'dime
   - 'quarter
|#

#|
 A PiggyBank is one of
 - '()
 - (cons Coin PiggyBank)
|#

; process-piggybank : ListOfNat -> ...
; (process-piggybank piggybank) returns ...
(define (process-piggybank piggybank)
  (cond
    [(empty? piggybank) ...]
    [else (... (first piggybank) ...
               (process-piggybank (rest piggybank)))]))

; penny-pincher : List -> List
; (penny-pincher piggybank) returns a list has coins without 'penny.
(define (penny-pincher piggybank)
  (cond
    [(empty? piggybank) '()]
    [(equal? (first piggybank) 'penny)
     (penny-pincher (rest piggybank))]
    [else (cons (first piggybank)
                (penny-pincher (rest piggybank)))]))

(check-expect (penny-pincher (list)) '())
(check-expect (penny-pincher (list 'quarter))
              (list 'quarter))
(check-expect (penny-pincher (list 'penny 'penny 'penny 'penny))
              '())
(check-expect (penny-pincher '
                             (penny quarter dime penny nickel penny))
              (list 'quarter 'dime 'nickel))

;;;;;;;;;;;;;;;
;; Problem 4
;;;;;;;;;;;;;;;
; wheres-waldo : List -> Nat
; (wheres-waldo lon) returns the index of the leftmost occurrence
; of 'waldo in the list
(define (wheres-waldo lon)
  (cond
    [(empty? lon) (error "waldo is nowhere to be found")]
    [(equal? (first lon) 'waldo) 0]
    [else
     (+ 1 (wheres-waldo (rest lon)))]))

(check-expect (wheres-waldo '(waldo)) 0)
(check-expect (wheres-waldo
               '(beach zoo ski-slope waldo france camping)) 3)
(check-error (wheres-waldo '(ranier rushmore kilimanjaro everest))
             "waldo is nowhere to be found")
(check-expect (wheres-waldo
               '(water water waldo water water water waldo waldo)) 2)


;;;;;;;;;;;;;;;
;; Problem 5
;;;;;;;;;;;;;;;
; mouse-journey : Int Int Int -> List
; (mouse-journey cat mouse cheese) return a list of the locations
; visited by the mouse.
(define (mouse-journey cat mouse cheese)
  (cond
    [(equal? mouse cat) (cons cat (cons 'sad '()))]
    [(equal? mouse cheese) (cons cheese (cons 'happy '()))]
  [else (cons mouse
        (mouse-journey cat (if (equal? (random 2) 1)
                               (sub1 mouse)
                               (add1 mouse))
                       cheese))]))

(check-satisfied (mouse-journey 0 3 6) cons?)
(check-satisfied (mouse-journey 7 4 -1) cons?)
(check-satisfied (mouse-journey 1 2 3) cons?)
(check-satisfied (mouse-journey 3 2 1) cons?)
(check-satisfied (mouse-journey -5 0 7) cons?)

;;;;;;;;;;;;;;;
;; Problem 6
;;;;;;;;;;;;;;;
;; 6a
; misspell : 1String -> 1String
; (misspell letter) returns the misspelled version by applying the
; four change rules.
(define (misspell letter)
  (cond
    [(equal? letter "s") "5"]
    [(equal? letter "e") "3"]
    [(equal? letter "o") "0"]
    [(equal? letter "l") "1"]
    [else letter]))

(check-expect (misspell "s") "5")
(check-expect (misspell "e") "3")
(check-expect (misspell "l") "1")
(check-expect (misspell "o") "0")
(check-expect (misspell "x") "x")

;; 6b
; leet-speak : str -> misspell
; (leet-speak word) returns the result of misspelling all
; letters in str.
(define (leet-speak word)
  (cond
    [(equal? word "") ""]
    [else (string-append (misspell (string-ith word 0))
               (leet-speak
                (substring word 1)))]))

(check-expect (leet-speak "") "")
(check-expect (leet-speak "leet speak neat speak")
              "133t 5p3ak n3at 5p3ak")
(check-expect (leet-speak "foolishness") "f001i5hn355")

;;;;;;;;;;;;;;;
;; Problem 7
;;;;;;;;;;;;;;;
; make-starry-night : Nat Nat Nat -> Img
; (make-starry-night num-stars width height) returns a
; rectangle image of the given dimensions with a black
; background and num-stars placed in randomly selected positions.
(define (make-starry-night num-stars width height)
  (cond
    [(= num-stars 0) (rectangle width height "solid" "black")]
    [else (place-image
           (circle 1 "solid" "white")
           (random width)
           (random height)
           (make-starry-night (sub1 num-stars) width height))]))

(check-satisfied (make-starry-night 0 300 50) image?)
(check-satisfied (make-starry-night 5 300 50) image?)
(check-satisfied (make-starry-night 100 50 75) image?)
(check-satisfied (make-starry-night 2000 500 300) image?)

;;;;;;;;;;;;;;;
;; Problem 8
;;;;;;;;;;;;;;;
(square 50 "solid" (make-color 100 230 50))
(overlay (square 20 "solid" (make-color 0 255 0 255))
           (square 40 "solid" (make-color 0 200 255)))
(overlay (square 10 "solid" (make-color 0 255 0))
           (square 40 "solid" (make-color 0 255 0 100))
           (square 80 "solid" (make-color 0 200 255)))

;; 8a
; shade-of-gray : Int -> Color
; (shade-of-gray num) takes an integer in the range 0 to 255,
; inclusive, and returns the corresponding shade of gray.
(define (shade-of-gray num)
  (make-color num num num 255))

(check-expect (shade-of-gray 100) (make-color 100 100 100 255))
(check-expect (shade-of-gray 220) (make-color 220 220 220 255))

(check-satisfied (square 30 "solid" (shade-of-gray 200)) image?)
(check-satisfied (square 40 "solid" (shade-of-gray 80)) image?)

;; 8b

#|
  ListOfImage is one of
 - '()
 - (cons image ListOfImage)
|#

; helper : Pos Pos Pos -> ListOfImage
; (helper n1 n2 radius) returns a list of images which can help
; us get the diminishing-circles.
(define (helper n1 n2 radius)
  (cond
    [(zero? n2) '()]
    [else (cons
           (circle radius "solid"
                   (shade-of-gray
                   (round (- 255 (* n2 (/ 255 n1))))))
           (helper n1
                   (sub1 n2)
                   (* radius (/ (sub1 n2) n2))))]))

; diminishing-circles : Int Int -> ListOfImage
; (diminishing-circles n radius) returns a list of n images, each
; of which is a solid circle.
(define (diminishing-circles n radius)
  (helper n n radius))

(check-satisfied (diminishing-circles 1 30) cons?)
(check-satisfied (diminishing-circles 2 40) cons?)
(check-satisfied (diminishing-circles 3 50) cons?)
(check-satisfied (diminishing-circles 4 60) cons?)
(check-satisfied (diminishing-circles 5 70) cons?)
(define colorway (diminishing-circles 15 60))
(check-expect (list (image-width (first colorway))
                    (image-width (second colorway))
                    (image-width (third colorway)))
              (list 120 112 104))
(check-satisfied (diminishing-circles 1 30) cons?)

;;;;;;;;;;;;;;;
;; Problem 9
;;;;;;;;;;;;;;;

;; 9a
; make-square : String -> Image
; (make-square color) returns a solid 20x20 rectangle in the given
; color.
(define (make-square color)
  (square 20 "solid" color))

(check-satisfied (make-square "red") image?)
(check-satisfied (make-square "black") image?)

;; 9b
; make-column : Nat Img Img -> Img
; (make-column n tu1 tu2) Return the image that results from
; stacking the given images one on top of the other in an
; alternating pattern of height n.
(define (make-column n tu1 tu2)
  (cond
    [(zero? n) empty-image]
    [(integer? (/ n 2)) (above
                         tu1 tu2 (make-column (- n 2) tu1 tu2))]
    [else (above
           tu1
           (make-column (- n 1) tu2 tu1))]))

(check-expect (image-width
               (make-column 0
                            (make-square "red")
                            (make-square "green"))) 0)

(make-column 8 (make-square "red") (make-square "green"))
(make-column 4
             (make-column 2 (make-square "blue") (make-square "yellow"))
             (make-column 2 (make-square "red") (make-square "green")))

;; 9c
; make-row : Nat Img Img -> Img
; (make-row n tu1 tu2) returns the image that results from placing
; the given images one next to the other in an alternating pattern
; of width n.
(define (make-row n tu1 tu2)
  (cond
    [(zero? n) empty-image]
    [(integer? (/ n 2)) (beside
                         tu1 tu2 (make-row (- n 2) tu1 tu2))]
    [else (beside
           (make-row (- n 1) tu1 tu2) tu1)]))

(check-expect (image-width
               (make-row 0
                         (make-square "red")
                         (make-square "green"))) 0)
(make-row 8 (make-square "black") (make-square "cyan"))
(make-row 25 (make-square "red") (make-square "green"))
(make-row 7
          (make-row 2 (make-square "blue") (make-square "yellow"))
          (make-row 2 (make-square "red") (make-square "green")))

;; 9d
; make-checkerboard : Int String String -> Image
; (make-checkerboard n color1 color2) returns a image by using the
; given number and color.
(define (make-checkerboard n color1 color2)
   (make-column n
   (make-row n (make-square color1) (make-square color2))
   (make-row n (make-square color2) (make-square color1))))

(check-satisfied (make-checkerboard 1 "blue" "pink") image?)
(check-satisfied (make-checkerboard 12 (shade-of-gray 50)
                                    (shade-of-gray 200)) image?)

;;;;;;;;;;;;;;;
;; Problem 10
;;;;;;;;;;;;;;;
; make-lumberjack-plaid : Int Int -> Img
; (make-lumberjack-plaid num-rows num-cols) returns an image
; resembling the plaid fabric for a lumberjack shirt.
(define (make-lumberjack-plaid num-rows num-cols)
     (cond
       [(zero? num-rows) empty-image]
       [(zero? num-cols) empty-image]
       [else (make-column num-rows
   (make-row num-cols (make-square "darkred") (make-square "red"))
   (make-row num-cols (make-square "black") (make-square "darkred")))]))

(check-satisfied (make-lumberjack-plaid 2 2) image?)
