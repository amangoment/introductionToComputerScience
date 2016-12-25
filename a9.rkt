;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname a9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

;;;;;;;;;;;;;;;
;; Problem 1
;;;;;;;;;;;;;;;
(define (last-positive/slow nums)
  (cond
   [(empty? nums) #false]
   [(positive? (first nums))
    (if (false? (last-positive/slow (rest nums)))
        (first nums)
        (last-positive/slow (rest nums)))]
   [else
    (last-positive/slow (rest nums))]))

;; 1a
#|
 A NumF is one of
 - #false
 - Num

A ListOfNum is one of
- '()
- (cons Num ListOfNum)
|#

; last-positive : ListOfNum -> NumF
; (last-positive nums) returns the last positive num in the list.
; Iff the list does not contains positive num, it #false.

(define nums (make-list 23 3))

(check-expect (last-positive/slow '(0 2 -4 1)) 1)
(check-expect (last-positive/slow '(0 2 -4 1 -3)) 1)

; By using the last-positive/slow function, it takes a long time to
; run. But it is much faster when we use the last-positive/fast
; function.

;; 1b
#|
(last-positive '(0 2 -4 1))
== (last-positive '(2 -4 1))
== (if (false? (last-positive '(-4 1)))
       (first '(2 -4 1))
       (last-positive (rest '(2 -4 1))))
== (if (false? (last-positive '(1)))
       (first '(2 -4 1))
       (last-positive (rest '(2 -4 1))))

== (if (false? (last-positive (rest '(1))))
       (first '(1))
       (last-positive (rest '(1))))
== (if (false? #false)
       (first '(1))
       (last-positive (rest '(1))))
== (first '(1))
== [(empty? '()) #false]
== 1
== (if (false? 1)
       (first '(2 -4 1))
       (last-positive (rest '(2 -4 1))))
== (last-positive '(-4 1))
== (last-positive '(1))
== (if (false? (last-positive (rest '(1))))
       (first '(1))
       (last-positive (rest '(1))))
== (if (false? #false)
       (first '(1))
       (last-positive (rest '(1))))
== (first '(1))
== 1
|#

;; 1c
; last-positive/fast : List -> Num
; (last-positive/fast nums) returns the last positive number in
; the list.
(define (last-positive/fast nums)
  (cond
    [(empty? nums) #false]
    [else 
     (local [(define ans (last-positive/fast (rest nums)))]
       (if (and
            (positive? (first nums))
            (false? ans))
           (first nums)
           ans))]))

(check-expect (last-positive/fast '(0 2 -4 1)) 1)
(check-expect (last-positive/fast '(0 2 -4 1 -3)) 1)
(check-expect (last-positive/fast nums) 3)

;;;;;;;;;;;;;;;
;; Problem 2
;;;;;;;;;;;;;;;
;; 2a
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

; collatz-steps : Int -> Int
; (collatz-steps num) returns the number of steps until the
; Collatz sequence starting with num reaches 1.
(define (collatz-steps num)
  (cond
    [(= num 1) 0]
    [else (add1 (collatz-steps (next-collatz num)))]))

(check-expect (collatz-steps 7) 16)
(check-expect (collatz-steps 1) 0)
(check-expect (collatz-steps 2000) 112)

;; 2b
; collatz-sequence : Int -> List
; (collatz-sequence num) returns a list of numbers in the
; Collatz sequence starting with num and ending with 1.
(define (collatz-sequence num)
  (cond
    [(= num 1) '(1)]
    [else (cons num
                (collatz-sequence (next-collatz num)))]))

(check-expect (collatz-sequence 7)
              (list 7 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1))
(check-expect (collatz-sequence 1) (list 1))

;;;;;;;;;;;;;;;
;; Problem 3
;;;;;;;;;;;;;;;
; make-column : Int Image Image -> Image
; (make-column n img1 img2) returns the image that results from
; stacking the given images one on top of the other in an
; alternating pattern of height n.
(define (make-column n img1 img2)
  (cond
    [(zero? n) empty-image]
    [else (above img1
                 (make-column (sub1 n) img2 img1))]))

; make-row : Int Image Image -> Image
; (make-row n img1 img2) returns the image that results from
; placing the given images one next to the other in an alternating
; pattern of width n.
(define (make-row n img1 img2)
  (cond
    [(zero? n) empty-image]
    [else (beside img1
                  (make-row (sub1 n) img2 img1))]))

;; 3a
; oscillate : Int Symbol Symbol -> List
; (oscillate n w1 w2) returns a list of length n items alternating
; between this and that, and starting with this.
(define (oscillate n w1 w2)
  (cond
    [(zero? n) '()]
    [else (cons w1 (oscillate (sub1 n) w2 w1))]))

(check-expect (oscillate 4 'boo 'hoo)
              (list 'boo 'hoo 'boo 'hoo))
(check-expect (oscillate 9 'X 'O)
              (list 'X 'O 'X 'O 'X 'O 'X 'O 'X))

;; 3b
; make-stack : Any Any Int Any Any -> Any
; (make-stack var base n bian1 bian2) returns the result of
; alternating the two items to obtain a stack of size.
(define (make-stack var base n bian1 bian2)
  (cond
    [(zero? n) base]
    [else (var bian1
               (make-stack var base (sub1 n) bian2 bian1))]))

(define img1 (square 20 "solid" "orange"))
(define img2 (square 20 "solid" "indigo"))
(check-satisfied (make-stack above empty-image 7 img1 img2) image?)
(check-satisfied (make-stack beside empty-image 16 img1 img2)
                 image?)

(check-expect (make-stack string-append "" 42 "0" "1")
              "010101010101010101010101010101010101010101")
(check-expect (make-stack + 0 1000 1 -1) 0)
(check-satisfied (local [(define (combine x y)
                           (overlay x (scale 1.1 y)))]
                   (make-stack combine empty-image 30 img1 img2))
                 image?)

;; 3c
; hugs-and-kisses : Nat -> ExpressionOfLove
; (hugs-and-kisses n) returns a list of n alternating
; symbols 'X and 'O, startng with 'X.
(define (hugs-and-kisses n)
  (make-stack cons '() n 'X 'O))

;;;;;;;;;;;;;;;
;; Problem 4
;;;;;;;;;;;;;;;
;; 4a
; any-greater-than-seven? : List -> Bool
; (any-greater-than-seven? ls) returns #true if any number in nums
; is greater than 7, and #false otherwise.
(define (any-greater-than-seven? ls)
  (cond
    [(empty? ls) #false]
    [(> (first ls) 7) #true]
    [else (any-greater-than-seven? (rest ls))]))

(check-expect (any-greater-than-seven? '()) #false)
(check-expect (any-greater-than-seven? '(3 8 10 3 1)) #true)
(check-expect (any-greater-than-seven? '(5 6 0 1 2 3 2 1)) #false)

;; 4b
; any-greater-than? : Nat List -> Bool
; (any-greater-than? n ls) returns #true if any number in nums is
; greater than x, and #false otherwise.
(define (any-greater-than? n ls)
  (cond
    [(empty? ls) #false]
    [(> (first ls) n) #true]
    [else (any-greater-than? n (rest ls))]))
    
(check-expect (any-greater-than? 7 '(3 6 7 2 1)) #false)
(check-expect (any-greater-than? 5 '(3 6 8 2 1)) #true)
(check-expect (any-greater-than? 0 '(-1.1 -2.2 -3.3 4.4 -5.5))
              #true)
(check-expect (any-greater-than? 5 '()) #false)

;; 4c
; <> : Num Num -> Bool
; (<> a b) returns #true if they are different from one another,
; and #false otherwise.
(define (<> a b)
  (if
   (equal? a b)
   #false
   #true))

(check-expect (<> 4 5) #true)
(check-expect (<> 1/2 3/4) #true)
(check-expect (<> 32 (* 2 2 2 2 2)) #false)

;; 4d
; A Rel? is a [Num Num -> Bool]
; any? : Rel? Int List -> Bool
; (any? sign n ls) returns #true if any number in nums is related
; to x (according to the given predicate), and #false otherwise.
(define (any? sign n ls)
  (cond
    [(empty? ls) #false]
    [else (or (sign (first ls) n)
              (any? sign n (rest ls)))]))

(check-expect (any? > 7 '(3 6 8 2 1)) #true)
(check-expect (any? > 8 '(3 6 8 2 1)) #false)
(check-expect (any? <= 0 '(-1.1 -2.2 -3.3 4.4 -5.5)) #true)
(check-expect (any? <> 0 '(0 0 0 0 0 0 0 0)) #false)
(check-expect (any? <> 1 '(1 1 1 1 0 1 1 1)) #true)