;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname a5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

(define handin "a5")
(define collaboration-statement "I worked alone.")

;;;;;;;;;;;;;;;
;; Problem 1
;;;;;;;;;;;;;;;
;; 1a
; b-o-b : Nat -> Nat
; (b-o-b num) takes a natural number and produces a string describing
; that number of beers
(define (b-o-b num)
  (cond
    [(= num 0) "no more bottles of beer"]
    [(= num 1) "1 bottle of beer"]
    [(> num 1) (string-append (number->string num)
                              " bottles of beer")]
    [else "unreachable"]))
  
(check-satisfied (b-o-b 1) string?)
(check-expect (b-o-b 0) "no more bottles of beer")
(check-expect (b-o-b 1) "1 bottle of beer")
(check-expect (b-o-b 2) "2 bottles of beer")
(check-expect (b-o-b 3) "3 bottles of beer")
(check-expect (b-o-b 99) "99 bottles of beer")

;; 1b
; verses : Num -> String
; (verses num) describes that number of verses of the song
; "99 Bottles of Beer on the Wall"
(define (verses numb)
  (cond
    [(= numb 0) ""]
    [(= numb 1) (string-append (b-o-b numb)
                               " on the wall, "
                               (b-o-b numb)
                               ".
Take one down and pass it around, no more bottles of beer on the wall.
")]
    [else (string-append (b-o-b numb)
                         " on the wall, "
                         (b-o-b numb)
                         ".\nTake one down and pass it around, "
          (b-o-b (sub1 numb))
                          " on the wall.\n"
                         (verses (sub1 numb)))]))

(check-expect (verses 0) "")
(check-expect (verses 1)"1 bottle of beer on the wall, 1 bottle of beer.
Take one down and pass it around, no more bottles of beer on the wall.
")
(check-expect (verses 2)
"2 bottles of beer on the wall, 2 bottles of beer.
Take one down and pass it around, 1 bottle of beer on the wall.
1 bottle of beer on the wall, 1 bottle of beer.
Take one down and pass it around, no more bottles of beer on the wall.
")
(check-expect (string-length (verses 2)) 233)
(check-expect (string-length (verses 99)) 11657)

;;;;;;;;;;;;;;;
;; Problem 2
;;;;;;;;;;;;;;;
; what-the? : Int -> Bool
; (what-the? n) returns ???
(define (what-the? n)
  (cond
    [(negative? n) #false]
    [(odd? n) (what-the? (- n 2))]
    [else (/ n 0)]))

#|
  (what-the? 5)
  == (cond
       [(negative? 5) #false]
       [(odd? 5) (what-the? (- 5 2))]
       [else (/ 5 0)])
  == (cond
       [#false #false]
       [(odd? 5) (what-the? (- 5 2))]
       [else (/ 5 0)])
  == (cond
       [#false #false]
       [#true (what-the? (- 5 2))]
       [else (/ 5 0)])
  == (what-the? (- 5 2))
  == (what-the? 3)
  == (cond
       [(negative? 3) #false]
       [(odd? 3) (what-the? (- 3 2))]
       [else (/ 3 0)])
  == (cond
       [#false #false]
       [(odd? 3) (what-the? (- 3 2))]
       [else (/ 3 0)])
  == (cond
       [#false #false]
       [#true (what-the? (- 3 2))]
       [else (/ 3 0)])
  == (what-the? (- 3 2))
  == (what-the? 1)
  == (cond
       [(negative? 1) #false]
       [(odd? 1) (what-the? (- 1 2))]
       [else (/ 1 0)]))
  == (cond
       [#false #false]
       [(odd? 1) (what-the? (- 1 2))]
       [else (/ 1 0)])
  == (cond
       [#true (what-the? (- 1 2))]
       [else (/ 1 0)])
  == (what-the? (- 1 2))
  == (what-the? -1)
  == (cond
       [(negative? -1) #false]
       [(odd? -1) (what-the? (- -1 2))]
       [else (/ -1 0)]))
  == (cond
       [#true #false]
       [(odd? -1) (what-the? (- -1 2))]
       [else (/ -1 0)]))

  == #false
|#

;;;;;;;;;;;;;;;
;; Problem 3
;;;;;;;;;;;;;;;
;; 3a
; process-nat : Nat -> ...
; (process-nat a-nat) returns ...
(define (process-nat a-nat)
  (cond
    [(zero? a-nat) ...]
    [else (... (process-nat (sub1 a-nat)))]))

;; 3b
; sum-of-squares : Num -> Num
; (sum-of-squares n) returns the sum of the squares of the
; first n integers
(define (sum-of-squares num)
  (cond
    [(= num 0) num]
    [else (+ (expt num 2) (sum-of-squares (sub1 num)))]))

(check-expect (sum-of-squares 0) 0)
(check-expect (sum-of-squares 3) 14)
(check-expect (sum-of-squares 4) 30)
(check-expect (sum-of-squares 100) 338350)


;;;;;;;;;;;;;;;
;; Problem 4
;;;;;;;;;;;;;;;
;; 4a

#|
 A ListOfNat is one of
 - '()
 - (cons Num ListOfNat)
|#

;; 4b
; process-nats : ListOfNat -> ...
; (process-nats nats) returns ...
(define (process-nats nats)
 (cond
 [(empty? nats) ...]
 [else (... (first nats) ...
 (process-nats (rest nats)))]))

;; 4c
; sweet? : Num -> Bool
; (sweet? con) returns true iff the given number can be
; divided by 3 or 5
(define (sweet? con)
  (or
   (integer? (/ con 3))
   (integer? (/ con 5))))

; count-sweets : ListOfNat -> Nat
; (count-sweets con) takes a ListOfNat and returns the number of
; numbers in the list that are multiples of either 3 or 5.
(define (count-sweets con)
  (cond
    [(empty? con) 0]
    [(sweet? (first con))
      (+ (count-sweets (rest con)) 1)]
    [else (count-sweets (rest con))]))

(check-expect (count-sweets '()) 0)
(check-expect (count-sweets (cons 12 '())) 1)
(check-expect (count-sweets
               (cons 55 (cons 22 (cons 0 (cons 7 (cons 42 '())
                                                    ))))) 3)

;; 4d
; sweets-only : ListOfNat -> List
; (sweets-only only) takes a ListOfNat and return the list with
; all non-sweet values removed.
(define (sweets-only only)
  (cond
    [(empty? only) '()]
    [(sweet? (first only)) (cons (first only)
                         (sweets-only (rest only)))]
    [else (sweets-only (rest only))]))

(check-expect (sweets-only '()) '())
(check-expect (sweets-only (cons 13 (cons 2 (cons 8 (cons 11 (cons 4
                                    (cons 7 '()))))))) '())
(check-expect (sweets-only
             (cons 55(cons 22 (cons 0 (cons 7 (cons 42 '()))))))
              (cons 55 (cons 0 (cons 42 '()))))

;;;;;;;;;;;;;;;
;; Problem 5
;;;;;;;;;;;;;;;
;; 5a

#|
 A Bit is one of
 - "0"
 - "1"
|#

;; 5b

#|
A BitString is one of
 - ""
 - (cons Num BitString)
|#

;; 5c
; pick-one-at-random : Any Any -> Any
; (pick-one-at-random any1 any2) takes two arguments and
; returns one of them
(define any1 1)
(define any2 2)
(define (pick-one-at-random any1 any2)
   (if
  (equal? (+ 1 (random 2)) 1)
  any1
  any2))

(check-satisfied (pick-one-at-random 1 -1) number?)
(check-satisfied (pick-one-at-random 'left 'right) symbol?)
(check-satisfied (pick-one-at-random "heads" "tails") string?)
(check-satisfied (pick-one-at-random #true #false) boolean?)

;; 5d
; make-noise : Nat -> BitString
; (make-noise num) takes a natural number n and returns a BitString
; of length n, where each Bit is selected at random and with equal
; probability.
(define (make-noise num)
  (cond
    [(= num 0) ""]
    [else (string-append (number->string (random 2))
                         (make-noise (sub1 num)))]))

(check-expect (make-noise 0) "")
(check-satisfied (make-noise 16) string?)
(check-satisfied (make-noise 50) string?)

;;;;;;;;;;;;;;;
;; Problem 6
;;;;;;;;;;;;;;;
;; 6a
#|
 A ListOfInt is one of
 - '()
 - (cons Int ListOfInt)
|#

; all-even? : ListOfInt -> Bool
; (all-even? function) returns #true iff every integer in the given
; list is even, and #false otherwise.
(define (all-even? function)
  (and(or
       (empty? function)
       (even? (first function)))
      (or
       (empty? function)
       (all-even? (rest function)))))

(check-expect (all-even? '()) #true)
(check-expect (all-even? (cons 3 '())) #false)
(check-expect (all-even? (cons 4 (cons 8 (cons -10
                                 (cons 0 '())))))#true)

;; 6b
; some-even? : ListOfInt -> Bool
; (some-even? a-function) takes a ListOfInt and returns #true
; iff at least one integer in the given list is even, and
; #false otherwise.
(define (some-even? a-function)
  (cond
    [(empty? a-function) #false]
    [(even? (first a-function)) #true]
    [else (some-even? (rest a-function))]))

(check-expect (some-even? (cons 3 '())) #false)
(check-expect (some-even? (cons 0 '())) #true)
(check-expect (some-even? (cons 5 (cons 9 (cons -11 (cons 1 '())))))
              #false)

;;;;;;;;;;;;;;;
;; Problem 7
;;;;;;;;;;;;;;;
;; 7a

#|
   A Nucleotide is one of
   - 'A
   - 'T
   - 'G
   - 'C
|#

#|
 A ListOfNucleotides is one of
 - '()
 - (cons Nucleotides ListOfNucleotides)
|#

;; 7b
; nucleotide num : Nat -> Symbol
; (nucleotide num) produces nucleotide symbol by using given number.
(define (nucleotide num)
  (cond
   [(equal? num 1) 'A]
   [(equal? num 2) 'C]
   [(equal? num 3) 'T]
   [(equal? num 4) 'G]))

; make-dna-strand : Nat -> List
; (make-dna-strand num) takes a natural number n and returns a
; list of n nucleotide symbols.
(define (make-dna-strand num)
  (cond
    [(= num 0) '()]
    [else (cons (nucleotide (+ 1 (random 4)))
                (make-dna-strand (sub1 num)))]))

(check-expect (make-dna-strand 0) '())
(check-satisfied (make-dna-strand 5) cons?)

;; 7c
; t-count : List -> Nat
; (t-count count) takes a DNA strand and returns the total number of
; occurrences of the T nucleotide.
(define (t-count count)
  (cond
    [(empty? count) 0]
    [(equal? (first count) 'T)
     (+ 1 (t-count (rest count)))]
    [else (t-count (rest count))]))

(check-expect (t-count '()) 0)
(check-expect (t-count
               (cons 'T (cons 'T (cons 'A (cons 'T '()))))) 3)
(check-within (t-count (make-dna-strand 10000)) 2500 100)

(define (g-count count)
  (cond
    [(empty? count) 0]
    [(equal? (first count) 'G)
     (+ 1 (g-count (rest count)))]
    [else (g-count (rest count))]))

(check-within (g-count (make-dna-strand 10000)) 2500 100)

;;;;;;;;;;;;;;;
;; Problem 8
;;;;;;;;;;;;;;;
;; 8a
#|
   A Coin is one of
   - 'penny
   - 'nickel
   - 'dime
   - 'quarter
|#

;; 8b
; coin->cents : Symbol -> Nat
; (coin->cents coin)  takes a Coin and returns the corresponding
; value of the coin in cents.
(define (coin->cents coin)
  (cond
   [(equal? coin 'penny) 1]
   [(equal? coin 'nickel) 5]
   [(equal? coin 'dime) 10]
   [(equal? coin 'quarter) 25]
   [else 'unreachable]))
  
(check-expect (coin->cents 'penny) 1)
(check-expect (coin->cents 'nickel) 5)
(check-expect (coin->cents 'dime) 10)
(check-expect (coin->cents 'quarter) 25)

;; 8c

#|
 A PiggyBank is one of
 - '()
 - (cons bank PiggyBank)
|#

;; 8d
; process-piggybank : ListOfNat -> ...
; (process-piggybank piggybank) returns ...
(define (process-piggybank piggybank)
 (cond
 [(empty? piggybank) ...]
 [else (... (first piggybank) ...
 (process-piggybank (rest piggybank)))]))

;; 8e
; count-coins : PiggyBank -> Nat
; (count-coins coin) returns the total sum of all coins in the bank.
(define (count-coins piggybank)
  (cond
    [(empty? piggybank) 0]
    [else (+ (coin->cents (first piggybank))
     (count-coins (rest piggybank)))]))

(check-expect (count-coins '()) 0)
(check-expect (count-coins (cons 'penny (cons 'penny '()))) 2)
(define bank (cons 'quarter (cons 'penny (cons 'nickel (cons 'penny
                                       (cons 'dime '()))))))
(check-expect (count-coins bank) 42)

;;;;;;;;;;;;;;;
;; Problem 9
;;;;;;;;;;;;;;;

#|
   A Dish is a (make-dish String Nat Num)
 
   A Menu is one of
   - '()
   - (cons Dish Menu)
|#

;; 9a
; A Dish is a (make-dish String Nat Num)
(define-struct dish [name calories cost])

;; 9b
; dish? : Any -> Bool
; make-dish : String Nat Num -> Dish
; dish-name : Dish -> String
; dish-calories : Dish -> Nat
; dish-cost : Dish -> Num

;; 9c
; process-dish : Dish -> ...
; (process-dish dish) returns ...
(define (process-dish dish)
 (cond
 [(empty? dish) ...]
 [else (... (first dish) ...
 (process-dish (rest dish)))]))

;; 9d
(define appetizer
  (make-dish "Deep Fried Wontons"
             760
             8.49))

(define soup
  (make-dish "Egg Drop"
             250
             5.29))

(define entree
  (make-dish "Salt and Pepper Squid"
             675
             12.82))

;; 9e
; count-calories : Dish -> Num
; (count-calories menu) takes a Menu and returns the total
; number of calories for all the items on the menu.
(define (count-calories menu)
  (cond
    [(empty? menu) 0]
    [else (+ (dish-calories (first menu))
             (count-calories (rest menu)))]))

(check-expect (count-calories '()) 0)
(define meal (cons appetizer (cons soup (cons entree '()))))
(check-expect (dish-name (first meal)) "Deep Fried Wontons")
(check-expect (count-calories meal) 1685)

;;;;;;;;;;;;;;;
;; Problem 10
;;;;;;;;;;;;;;;
;; 10a
; make-color-chart : String -> Image
; (make-color-chart color) returns image by using the given color.
(define (make-color-chart color)
(cond
  [(empty? color) empty-image]
  [else (beside (rectangle 30 100 "solid" (first color))
                (make-color-chart (rest color)))]))

(check-expect (image-width (make-color-chart '())) 0)
(check-expect (image-height (make-color-chart '())) 0)
(define chart (make-color-chart (cons "gold" '())))

(check-expect (image-width chart) 30)
(check-expect (image-height chart) 100)
(define rainbow (cons "red" (cons "orange" (cons "yellow" (cons "green"
         (cons "blue" (cons "indigo" (cons "violet" '()))))))))
(make-color-chart rainbow)
(make-color-chart (cons "pink" (cons "cyan" (cons "magenta" rainbow))))

;; 10b
; make-bullseye : Nat -> Image
; (make-bullseye num) returns Image by using the given number.
(define (make-bullseye num)
  (cond
    [(= num 0) (circle 10 "solid" "black")]
    [else (overlay
           (make-bullseye (sub1 num))
           (if (odd? num)
               (circle (* 10 (add1 num)) "solid" "red")
               (circle (* 10 (add1 num)) "solid" "black")))]))

(make-bullseye 0)
(make-bullseye 1)
(make-bullseye 2)
(check-expect (image-width (make-bullseye 1)) 40)
(check-expect (image-width (make-bullseye 2)) 60)
(make-bullseye 6)
(check-expect (image-width (make-bullseye 6)) 140)
(make-bullseye 7)
(check-expect (image-width (make-bullseye 7)) 160)