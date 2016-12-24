;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname a3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

(define handin "a3")
(define collaboration-statement "I worked alone")


;;;;;;;;;;;;;;;
;; Problem 1
;;;;;;;;;;;;;;;

#|
   A Season is one of
   - 'spring
   - 'summer
   - 'fall
   - 'winter
 
   An Icon is an Image of dimensions 32x32
 
   A DiceRoll is an Int in the interval [2..12]
|#

;; 1a
; the username of one of your lab instructors
(define my-lab-instructor 'Suzanne-Menzel)

; the current season of the year
(define tis-the-season 'winter)

; a solid purple square
(define purple-icon (square 32 "solid" "purple"))

; the result of rolling two fair dice
(define craps (+ 2(random 11)))


;; 1b
; 
(check-satisfied my-lab-instructor symbol?)
(check-satisfied tis-the-season symbol?)
(check-member-of tis-the-season 'spring 'summer 'fall 'winter)
(check-expect (image-width purple-icon)
              (image-height purple-icon))
(check-range craps 2 12)
; add
(check-satisfied purple-icon image?)
(check-satisfied craps number?)
(check-expect tis-the-season 'winter)


;;;;;;;;;;;;;;;
;; Problem 2
;;;;;;;;;;;;;;;
; flip-coin : symbol -> symbol
; (flip-coin n) : one of the symbols represents a coin
; and returns the opposite one.
(define (flip-coin n)
  (cond
    [(equal? n 'heads) 'tails]
    [(equal? n 'tails) 'heads]
    [else 'wrong]))

(check-expect (flip-coin 'heads) 'tails)
(check-expect (flip-coin 'tails) 'heads)
(check-expect (flip-coin (flip-coin 'tails)) 'tails)


;;;;;;;;;;;;;;;
;; Problem 3
;;;;;;;;;;;;;;;
; wins-rps? : symbol symbol -> boolean
; (wins-rps? a b) : tells the result of Rock-paper-scissors
(define (wins-rps? a b)
  (or
  (and (equal? a 'rock) (equal? b 'scissors))
  (and (equal? a 'scissors) (equal? b 'paper))
  (and (equal? a 'paper) (equal? b 'rock))))

(check-expect (wins-rps? 'rock 'scissors) #true)
(check-expect (wins-rps? 'scissors 'paper) #true)
(check-expect (wins-rps? 'scissors 'paper) #true)
(check-expect (wins-rps? 'paper 'rock) #true)
(check-expect (wins-rps? 'paper 'paper) #false)
(check-expect (wins-rps? 'rock 'paper) #false)
(check-expect (wins-rps? 'lizard 'spock) #false)

;;;;;;;;;;;;;;;
;; Problem 4
;;;;;;;;;;;;;;;
; fib-like? : number number number -> boolean
; (fib-like? x a b) : checks Fibonacci order
(define (fib-like? x a b)
  (or
   (= (+ a b) x)
   (= (+ b x) a)
   (= (+ x a) b)))

(check-expect (fib-like? 3 5 8) #true)
(check-expect (fib-like? 0 0 0)  #true)
(check-expect (fib-like? 7 -2 5)  #true)
(check-expect (fib-like? 10 15 20) #false)


;;;;;;;;;;;;;;;
;; Problem 5
;;;;;;;;;;;;;;;
; dog-years->human-years : number -> number
; (dog-years->human-years n) : produces the dog's age to
; human's age
(define (dog-years->human-years n)
   (if (<= n 2) (* n 7)
       (+ (* (- n 2) 4) 21)))

(check-expect (dog-years->human-years 0) 0)
(check-expect (dog-years->human-years 1) 7)
(check-expect (dog-years->human-years 2) 14)
(check-expect (dog-years->human-years 3) 25)
(check-expect (dog-years->human-years 4) 29)
(check-expect (dog-years->human-years 7) 41)

 ;;;;;;;;;;;;;;;
;; Problem 6
;;;;;;;;;;;;;;;

;; 6a
; tee-size : symbol -> boolean
; (tee-size? n1) : produces if the symbol belongs to tee-size
(define (tee-size? n1)
  (cond
    [(equal? n1 'x-small) #true]
    [(equal? n1 'small) #true]
    [(equal? n1 'medium) #true]
    [(equal? n1 'large) #true]
    [(equal? n1 'x-large) #true]
    [else #false]))

(check-satisfied 'x-small tee-size?)
(check-satisfied 'small tee-size?)
(check-satisfied 'medium tee-size?)
(check-satisfied 'large tee-size?)
(check-satisfied 'x-large tee-size?)

;; 6b
; next-size-up : symbol -> symbol
; (next-size-up n2) : produeces the one size larger than the
; given value
(define (next-size-up n2)
  (cond
    [(equal? n2 'x-small) 'small]
    [(equal? n2 'small) 'medium]
    [(equal? n2 'medium) 'large]
    [(equal? n2 'large) 'x-large]
    [(equal? n2 'x-large) 'x-large]
    [else #false]))

(check-expect (next-size-up 'x-small) 'small)
(check-expect (next-size-up 'small) 'medium)
(check-expect (next-size-up 'medium) 'large)
(check-expect (next-size-up 'large) 'x-large)
(check-expect (next-size-up 'x-large) 'x-large)

(check-satisfied (next-size-up 'small) tee-size?)
(check-satisfied (next-size-up 'x-small) tee-size?)
(check-satisfied (next-size-up 'medium) tee-size?)
(check-satisfied (next-size-up 'large) tee-size?)
(check-satisfied (next-size-up 'x-large) tee-size?)

;; 6c
; tee>=? : symbol symbol -> boolean
; (tee>=? size1 size2) : checks if the first one is
; greater than or equal to the second one in size
(define (tee>=? size1 size2)
  (cond
   [(and(equal? size1 'x-small) (equal? size2 'x-small))#true]
   [(and(equal? size1 'small) (equal? size2 'x-small))#true]
   [(and(equal? size1 'medium) (equal? size2 'x-small))#true]
   [(and(equal? size1 'large) (equal? size2 'x-small))#true]
   [(and(equal? size1 'x-large) (equal? size2 'x-small))#true]
   [(and(equal? size1 'small) (equal? size2 'small))#true]
   [(and(equal? size1 'medium) (equal? size2 'small))#true]
   [(and(equal? size1 'large) (equal? size2 'small))#true]
   [(and(equal? size1 'x-large) (equal? size2 'small))#true]
   [(and(equal? size1 'medium) (equal? size2 'medium))#true]
   [(and(equal? size1 'large) (equal? size2 'medium))#true]
   [(and(equal? size1 'x-large) (equal? size2 'medium))#true]
   [(and(equal? size1 'large) (equal? size2 'large))#true]
   [(and(equal? size1 'x-large) (equal? size2 'large))#true]
   [(and(equal? size1 'x-large) (equal? size2 'x-large))#true]
    [else #false]))

(check-expect (tee>=? 'small 'x-small) #true)
(check-expect (tee>=? 'x-small 'x-small) #true)
(check-expect (tee>=? 'medium 'x-small) #true)
(check-expect (tee>=? 'large 'x-small) #true)
(check-expect (tee>=? 'x-large 'x-small) #true)
(check-expect (tee>=? 'small 'small) #true)
(check-expect (tee>=? 'medium 'small) #true)
(check-expect (tee>=? 'large 'small) #true)
(check-expect (tee>=? 'x-large 'small) #true)
(check-expect (tee>=? 'medium 'medium) #true)
(check-expect (tee>=? 'large 'medium) #true)
(check-expect (tee>=? 'x-large 'medium) #true)
(check-expect (tee>=? 'large 'large) #true)
(check-expect (tee>=? 'x-large 'large) #true)
(check-expect (tee>=? 'x-large 'x-large) #true)

(check-expect (tee>=? 'x-small 'small) #false)
(check-expect (tee>=? 'x-small 'medium) #false)
(check-expect (tee>=? 'x-small 'large) #false)
(check-expect (tee>=? 'x-small 'x-large) #false)
(check-expect (tee>=? 'x-small 'large) #false)
(check-expect (tee>=? 'x-small 'medium) #false)
(check-expect (tee>=? 'x-small 'small) #false)
(check-expect (tee>=? 'medium 'large) #false)
(check-expect (tee>=? 'medium 'x-large) #false)
(check-expect (tee>=? 'large 'x-large) #false)

;; 6d
; tee-max : symbol symbol -> symbol
; (tee-max size1 size2) : takes two TeeSize arguments
; and returns the maximal one
(define (tee-max size1 size2)
  (if (tee>=? size1 size2)
      size1
      size2))
       

(check-satisfied (tee-max 'large 'large) tee-size?)
(check-member-of (tee-max 'medium 'small) 'medium 'small)



;; 6e
; tee-max3 : symbol symbol symbol -> symbol
; (tee-max3 size1 size2 size3) : takes three TeeSize
; arguments and returns the maximal one
(define (tee-max3 size1 size2 size3)
  (tee-max (tee-max size1 size2) size3))

(check-expect (tee-max3 'large 'small 'medium) 'large)
(check-satisfied (tee-max3 'large 'large 'large) tee-size?)
(check-member-of (tee-max3 'medium 'small 'large)
                 'medium 'small 'large)


;;;;;;;;;;;;;;;
;; Problem 7
;;;;;;;;;;;;;;;

;; 7a
; sleep-in? : symbol -> boolean
; (sleep-in? d) : returns #true if the given day is weekend,
; and #false otherwise
(define (sleep-in? d)
    (cond
      [(equal? d 'sat) #true]
      [(equal? d 'sun) #true]
  [else #false]))

(check-expect (sleep-in? 'sat) #true)
(check-expect (sleep-in? 'sun) #true)
(check-expect (sleep-in? 'mon) #false)

;; 7b
; tomorrow : symbol -> symbol
; (tomorrow d2): produces the day after now
(define (tomorrow d2)
  (cond
    [(equal? d2 'mon) 'tue]
    [(equal? d2 'tue) 'wed]
    [(equal? d2 'wed) 'thu]
    [(equal? d2 'thu) 'fri]
    [(equal? d2 'fri) 'sat] 
    [(equal? d2 'sat) 'sun]
    [(equal? d2 'sun) 'mon]
    [else #false]))

(check-expect (tomorrow 'mon) 'tue)
(check-expect (tomorrow 'sat) 'sun)

;; 7c
; day-after-tomorrow : symbol -> symbol
; (day-after-tomorrow d2) : returns the DayOfWeek two
; days later
(define (day-after-tomorrow d2)
  (tomorrow (tomorrow d2)))

(check-expect (day-after-tomorrow 'mon) 'wed)
(check-expect (day-after-tomorrow 'sat) 'mon)

;; 7d
; party-on? : symbol -> boolean
; (party-on? d) : predicate if people can have party or not
(define (party-on? d)
  (sleep-in? (tomorrow d)))

(check-expect (party-on? 'mon) #false)
(check-expect (party-on? 'fri) #true)
(check-expect (party-on? 'sat) #true)


;;;;;;;;;;;;;;;
;; Problem 8
;;;;;;;;;;;;;;;

;; 8a
; next-odd : number -> number
; (next-odd a) : returns the smallest odd integer greater
; than n
(define (next-odd a)
  (if
    (integer? (/ a 2))
    (+ 1 a)
    (+ 2 a)))


(check-expect (next-odd 5) 7)
(check-expect (next-odd -2)-1)
(check-expect (next-odd (next-odd (next-odd 6) )) 11)

;; 8b
; next-collatz : number -> number
; (next-collatz an-1) : takes a number in the Collatz
; sequenceand returns the next number in the sequence
(define (next-collatz an-1)
  (if
    (integer? (/ an-1 2))
    (/ an-1 2)
    (+ (* an-1 3) 1)))

(check-expect (next-collatz 7) 22)
(check-expect (next-collatz 22) 11)
(check-expect (next-collatz 1) 4)
(check-expect (next-collatz 5) 16)
(check-expect (next-collatz 6) 3)
(check-expect (next-collatz 16) 8)


;;;;;;;;;;;;;;;
;; Problem 9
;;;;;;;;;;;;;;;
; make-line : number string number string -> image
; (make-line num1 type num2 color) : produces a image by
; giving additional arguments
(define (make-line num1 type num2 color)
  (cond
    [(equal? type "hor") (rectangle num1 num2 "solid" color)]
    [(equal? type "ver") (rectangle num2 num1 "solid" color)]
    [else "unreachable"]))

(make-line 300 "hor" 1 "black")
(make-line 500 "hor" 3 "chartreuse")
(make-line 30 "ver" 10 "lightsalmon")
(make-line 42 "ver" 7 "my favorite color")

;;;;;;;;;;;;;;;
;; Problem 10
;;;;;;;;;;;;;;;

;; 10a
; sweet? : number -> boolean
; (sweet? n) : takes a number and returns #true if n is a
; multiple of either 3 or 5, and #false otherwise. 
(define (sweet? n)
  (or
    (integer? (/ n 3))
    (integer? (/ n 5))
    #false))

(check-expect (sweet? 7) #false)
(check-expect (sweet? 10) #true)

;; 10b
; next-sweetest : number -> number
; (next-sweetest n) : returns the next integer greater than
; n which is a multiple of either 3 or 5
(define (next-sweetest n)
  (if
   (or
    (integer? (/ (+ n 1) 3))
    (integer? (/ (+ n 1) 5)))
    (+ n 1)
     (if
     (or
      (integer? (/ (+ n 2) 3))
      (integer? (/ (+ n 2) 5)))
     (+ n 2)
     (if
      (or
      (integer? (/ (+ n 3) 3))
      (integer? (/ (+ n 3) 5)))
      (+ n 3)
      (if
       (or
        (integer? (/ (+ n 4) 3))
        (integer? (/ (+ n 4) 5)))
       (+ n 4)
        #false)))))

(check-expect (next-sweetest 6) 9)
(check-expect (next-sweetest 21) 24)
(check-expect (next-sweetest (next-sweetest
                              (next-sweetest 42)))50)
(check-expect (next-sweetest
         429237489284723749274982734982734982734892)
  429237489284723749274982734982734982734895)
          

      