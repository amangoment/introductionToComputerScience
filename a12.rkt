;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname a12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define handin "a12")
(define collaboration-statement "I worked alone")


;;;;;;;;;;;;;;;
;; Problem 1
;;;;;;;;;;;;;;;
; give-away : [ListOf PosInt] -> [ListOf PosInt]
; (give-away pocket) returns the result of giving pocket coins
; to Alice and Bob, such that the child with less money always gets
; the next coin (with ties going to Alice)
(define (give-away pocket)
   (foldl ; PosInt [ListOf PosInt] -> [ListOf PosInt]
    (λ (denom accs)
           (if (<= (first accs) (second accs))
               (list (+ denom (first accs)) (second accs))
               (list (first accs) (+ denom (second accs)))))
          '(0 0) pocket))

(check-expect (give-away '()) '(0 0))
(check-expect (give-away '(25)) '(25 0))
(check-expect (give-away '(10 25)) '(10 25))
(check-expect (give-away '(10 5 1 5 25 10)) '(35 21))

;;;;;;;;;;;;;;;
;; Problem 2
;;;;;;;;;;;;;;;
; sierpinski-carpet : PosInt -> Image
; (sierpinski-carpet n) builds an image by using the given number.
(define (sierpinski-carpet n)
    (cond
    [(= n 1) (square 3 "solid" "black")]
    [else (local [(define squ (sierpinski-carpet (sub1 n)))]
            (above (beside squ squ squ)
                   (beside squ (square (image-width squ)
                                       "solid" "white") squ)
                   (beside squ squ squ)))]))

(check-expect (image-width (sierpinski-carpet 1)) 3)
;(scale 10 (sierpinski-carpet 2))
;(scale 10 (sierpinski-carpet 3))
;(sierpinski-carpet 6)
;(scale 1/3 (sierpinski-carpet 7))

;;;;;;;;;;;;;;;
;; Problem 3
;;;;;;;;;;;;;;;
; verify-rpn? : [ListOf X] -> Bool
; (verify-rpn? ls) returns #true if it is a valid RPN expression over
; the operators +, -, *, and /, and #false otherwise.
(define (verify-rpn? ls)
  (cond
    [(empty? ls) #false]
    [(empty? (rest ls)) (number? (first ls))]
    [(empty? (rest ls)) (procedure? (first ls))]
    [else (local [; n : [ListOf X] -> Nat
                  (define (n l)
                    (cond
                      [(empty? l) 0]
                      [else (if (number? (first l))
                                (add1 (n (rest l)))
                                (n (rest l)))]))
                  (; p : [ListOf X] -> Nat
                   define (p l)
                    (cond
                      [(empty? l) 0]
                      [else (if (procedure? (first l))
                                (add1 (p (rest l)))
                                (p (rest l)))]))]
           (equal? (- (n ls) (p ls)) 1))]))

(check-expect (verify-rpn? '()) #false)
(check-expect (verify-rpn? (list 42)) #true)
(check-expect (verify-rpn? (list 2 1 + 3 - 4 *)) #true)
(check-expect (verify-rpn? '(2 1 + 3 - 4 *)) #false)
(check-expect (verify-rpn? (list 1 2 3 - + * 2 1)) #false)
(check-expect (verify-rpn? (list 2 4 3 5 + - /)) #true)
(check-expect (verify-rpn? (list 3 5 + 2 / 3)) #false)

;;;;;;;;;;;;;;;
;; Problem 4
;;;;;;;;;;;;;;;
; spin-out : Nat -> [ListOf Nat]
; (spin-out n) returns a solution for a Spin Out puzzle.
(define (spin-out n)
  (local
    [(define (loop i n1 n2)
       (cond
         [(= i n) n2]
         [else (loop (+ i 1) n2 (append n1 (list (+ i 1))
                                        (reverse n1) n2))]))]
    (loop 0 '() '())))

(check-expect (spin-out 0) '())
(check-expect (spin-out 1) (list 1))
(check-expect (spin-out 2) (list 2 1))
(check-expect (spin-out 3) (list 1 3 1 2 1))
(check-expect (spin-out 4) (list 2 1 4 1 2 1 3 1 2 1))
(check-expect (length (spin-out 7)) 85)
(check-expect (length (spin-out 20)) 699050)

;;;;;;;;;;;;;;;
;; Problem 5
;;;;;;;;;;;;;;;
; count-shortest-paths : Nat Nat -> Num
; (count-shortest-paths n1 n2) returns the number of distinct
; shortest paths between the indicated square and the upper left
; corner.
(define (count-shortest-paths n1 n2)
  (cond
    [(or (zero? n1) (zero? n2)) 1]
    [else (+ (count-shortest-paths (sub1 n1) n2)
             (count-shortest-paths n1 (sub1 n2)))]))

(check-expect (count-shortest-paths 0 0) 1)
(check-expect (count-shortest-paths 0 1) 1)
(check-expect (count-shortest-paths 1 0) 1)
(check-expect (count-shortest-paths 1 1) 2)
(check-expect (count-shortest-paths 1 2) 3)
(check-expect (count-shortest-paths 2 1) 3)
(check-expect (count-shortest-paths 2 2) 6)
(check-expect (count-shortest-paths 2 3) 10)
(check-expect (count-shortest-paths 2 7) 36)
(check-expect (count-shortest-paths 6 5) 462)

;;;;;;;;;;;;;;;
;; Problem 6
;;;;;;;;;;;;;;;
;; 6a
; insert-in-order : X [X X -> Bool] [ListOf X] -> [ListOf X]
; (insert-in-order a f ls) returns the result of inserting x into the
; correct location in the given list such that the result is in
; sorted order.
(define (insert-in-order a f ls)
  (local
    [; help : [ListOf X] -> [ListOf X]
     (define (help ls)
       (cond
         [(empty? ls) (list a)]
         [else (if (f a (first ls))
                   (cons a ls)
                   (cons (first ls) (help (rest ls))))]))]
    (help ls)))

(check-expect (insert-in-order 3 < '(1 2 4 5 6 7 8))
              (list 1 2 3 4 5 6 7 8))
(check-expect (insert-in-order 5 > '(12 10 9 7 4 3 1 0))
              (list 12 10 9 7 5 4 3 1 0))
(check-expect (insert-in-order "dog" string<?
                               '("ape" "bat" "cat" "pig" "rat"))
              (list "ape" "bat" "cat" "dog" "pig" "rat"))

;; 6b
; insertion-sort : [X X -> Bool] [ListOf X] -> [ListOf X]
; (insertion-sort f ls) returns the result of sorting the list
; according to the given relation.
(define (insertion-sort f ls)
  (foldl
   ; X [ListOf X] -> [ListOf X]
   (λ (x acc)
           (insert-in-order x f acc)) '() ls))

(check-expect (insertion-sort < '(9 3 2 0 1 4 9 2 1))
              (list 0 1 1 2 2 3 4 9 9))
(check-expect (insertion-sort > '(9 3 2 0 1 4 9 2 1))
              (list 9 9 4 3 2 2 1 1 0))
(check-expect (insertion-sort string<?
                        '("cat" "bird" "ant" "frog" "cow" "dog" "gnu"))
              (list "ant" "bird" "cat" "cow" "dog" "frog" "gnu"))
(check-expect (insertion-sort string-ci>=?
                        '("Cat" "birD" "ant" "frog" "cOW" "dog" "GNU"))
              (list "GNU" "frog" "dog" "cOW" "Cat" "birD" "ant"))
(check-expect (insertion-sort
               (lambda (p1 p2)
                 (< (posn-x p1) (posn-x p2)))
               (list (make-posn 2 3) (make-posn 9 5)
                     (make-posn 7 4) (make-posn 3 2)))
              (list (make-posn 2 3) (make-posn 3 2)
                    (make-posn 7 4) (make-posn 9 5)))


;; 6c
#|
  INSERTION SORT
       n       sorted        reverse sorted        random
  ------------------------------------------------------------
      100        0                  0                0
     1000        641                0                610
    10000       63390               0               62688
   100000        ???                62               ???
 

(define (ignore x) #true)
(define list1 (build-list 100 add1))
(define list2 (reverse (build-list 100 add1)))
(define list3 (build-list 100 (λ (x) (+ x (random 100)))))
(ignore (time (insertion-sort <= list1)))
|#

;;;;;;;;;;;;;;;
;; Problem 7
;;;;;;;;;;;;;;;
;; 7a
; merge : [X X -> Bool] [ListOf X] [ListOf X] -> [ListOf X]
; (merge f ls1 ls2) returns the result of combining the list
; elements into a single sorted list.
(define (merge f ls1 ls2)
  (cond
    [(empty? ls1) ls2]
    [(empty? ls2) ls1]
    [else (local [(define x (first ls1))
                  (define y (first ls2))]
            (if (f x y)
                (cons x (merge f (rest ls1) ls2))
                (cons y (merge f ls1 (rest ls2)))))]))

(check-expect (merge < '() '()) '())
(check-expect (merge < '(1 2 3) '(4 5 6 7 8))
              (list 1 2 3 4 5 6 7 8))
(check-expect (merge <= '(1 1 3 3 5 5 7 7) '(0 0 2 2 4 4 6 6))
              (list 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7))
(check-expect (merge > '(9 8 7 3 2 1) '(13 12 11 10 6 5 4))
              (list 13 12 11 10 9 8 7 6 5 4 3 2 1))
(check-expect (merge string<? '("cat" "dog" "pig") '("bat" "cow" "rat"))
              (list "bat" "cat" "cow" "dog" "pig" "rat"))

;; 7b
; group-sorted-sequences : [X X -> Bool] [ListOf X] -> [ListOf X]
; (group-sorted-sequences f ls)returns the list with each
; sorted sequence grouped in a sublist
(define (group-sorted-sequences f ls)
  (local
    [; help : [ListOf X] -> [ListOf [ListOf X]]
     (define (help ls)
       (cond
         [(empty? ls) '()]
         [else (local
                 [(define ans (help (rest ls)))]
                 (cond
                   [(empty? ls) '()]
                   [(empty? (rest ls)) (list ls)]
                   [(f (first ls) (first (first ans)))
                    (cons (cons (first ls) (first ans)) (rest ans))]
                   [else (cons (list (first ls)) ans)]))]))]
    (help ls)))

(check-expect (group-sorted-sequences > '()) '())
(check-expect (group-sorted-sequences < '(1 2 3))
              (list (list 1 2 3)))
(check-expect (group-sorted-sequences >= '(2 2 2 2))
              (list (list 2 2 2 2)))
(check-expect (group-sorted-sequences < '(5 4 3 2 1))
              (list (list 5) (list 4) (list 3) (list 2) (list 1)))
(check-expect (group-sorted-sequences < '(1 2 3 4 2 3 4 5 6 1 2 9 8 7))
              (list (list 1 2 3 4) (list 2 3 4 5 6)
                    (list 1 2 9) (list 8) (list 7)))

;;;;;;;;;;;;;;;
;; Problem 8
;;;;;;;;;;;;;;;
;; 8a
; merge-adjacent-sequences : [X X -> Bool] [ListOf X] -> [ListOf X]
; (merge-adjacent-sequences f ls) returns the list after merging
; adjacent lists together.
(define (merge-adjacent-sequences f ls)
  (cond
    [(empty? ls) '()]
    [(empty? (rest ls)) ls]
    [else (cons (merge f (first ls) (second ls))
                (merge-adjacent-sequences f (rest (rest ls))))]))

(check-expect (merge-adjacent-sequences < '()) '())
(check-expect (merge-adjacent-sequences < '((1 2)))
              (list (list 1 2)))
(check-expect (merge-adjacent-sequences < '((2 5) (4 7 8) (3 7) (6)))
              (list (list 2 4 5 7 8) (list 3 6 7)))
(check-expect (merge-adjacent-sequences <
              '((2 5) (4 7 8) (3 7) (6) (0 1)))
              (list (list 2 4 5 7 8) (list 3 6 7) (list 0 1)))
(check-expect (merge-adjacent-sequences <
              (group-sorted-sequences < '(1 2 3 4 2 3 4 5 6 1 2 9 8 7)))
              (list (list 1 2 2 3 3 4 4 5 6) (list 1 2 8 9) (list 7)))

;; 8b
; mergesort : rel? [ListOf List] -> [ListOf List]
; (mergesort f ls) returns the result of sorting the elements in ls
; according to rel? using the Merge Sort algorithm.
(define (mergesort f ls)
  (local
    [(define (helper ls)
       (cond
         [(empty? ls) '()]
         [(empty? (rest ls)) (first ls)]
         [else (helper (merge-adjacent-sequences f ls))]))]
     (helper (group-sorted-sequences f ls))))

(check-expect (mergesort > '()) '())
(check-expect (mergesort > '(42)) (list 42))
(check-expect (mergesort string<?
              '("cow" "ant" "boa" "bat" "dog" "rat" "sow" "pig"))
              (list "ant" "bat" "boa" "cow" "dog" "pig" "rat" "sow"))
(check-expect (mergesort > '(1 2 3 4 5 6 7 8)) (list 8 7 6 5 4 3 2 1))
(check-expect (mergesort < '(8 7 6 5 4 3 2 1)) (list 1 2 3 4 5 6 7 8))
(check-expect (mergesort <
              '(-3 9 5 1 -4 15 -7 25 29 33 6 8 2 9 4 -3 9 10 29 55))
              (list -7 -4 -3 -3 1 2 4 5 6 8 9 9 9 10 15 25 29 29 33 55))

;; 8c
#|
  MERGE SORT
       n       sorted        reverse sorted        random
  ------------------------------------------------------------
      100         0                 0                0
     1000         0                 0                16
    10000         31               125              140
   100000        375               2547             4172

(define (ignore x) #true)
(define list1 (build-list 100 add1))
(define list2 (reverse (build-list 100 add1)))
(define list3 (build-list 100 (λ (x) (+ x (random 100)))))
(ignore (time (mergesort <= list1)))

  INSERTION SORT
       n       sorted        reverse sorted        random
  ------------------------------------------------------------
      100         0                 0                 0
     1000        641                0                610
    10000       63390               0               62688
   100000        ???                62               ???
|#

;; 8d
#|
 Comparing insertion-sort with mergesort, mergesort is 50 times faster
 than insertion-sorted when we input sorted number and mergesort is 500
 times faster than insertion-sorted when we input random number. But
 when we input reverse sorted number into mergesort, it is 40 times
 slower than insertion-sorted.
 Because when we input reverse sorted number, the number is from big to
 small and insertion-sorted sort number from big to small. But when we
 use the mergesort, we have to find the largest number one by one, which
 will take longer time than insertion-sorted.
|#