;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname a13) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

;;;;;;;;;;;;;;;
;; Problem 1
;;;;;;;;;;;;;;;
;; 1a
#|
  A Move is a Nat in the range [1..3].

  A Player is a (make-player Symbol [Nat -> Move]])

  A Game is one of
 - (cons Symbol '())
 - (cons Move Game)
|#

(define-struct player [name strategy])

(define greedy-witch
  (make-player 'Alice
               (lambda (tiles)
                 (min tiles 3))))

(define wicked-witch
  (make-player 'Elphaba
               (lambda (tiles)
                 (cond
                   [(<= tiles 5) 1]
                   [else 3]))))

(define good-witch
  (make-player 'Glinda
               (lambda (tiles)
                 (cond
                   [(<= tiles 2) 1]
                   [(<= tiles 4) (sub1 tiles)]
                   [else (add1 (random 3))]))))

(check-expect (player? wicked-witch) #true)
(check-expect (player-name wicked-witch) 'Elphaba)

;; 1b
; play : Nat Player Player -> Game
; (play n p1 p2) returns a list containing the number of tiles at each
; turn followed by the name of the winner.
(define (play n p1 p2)
  (local
    [(define (helper num m1 m2 name1 name2)
       (cond
         [(<= num 0) (list num name1)]
         [else (cons num
                     (helper (- num (m1 num)) m2 m1 name2 name1))]))]
    (helper n (player-strategy p1) (player-strategy p2)
            (player-name p1) (player-name p2))))

(check-expect (play 30 wicked-witch greedy-witch)
              (list 30 27 24 21 18 15 12 9 6 3 0 'Elphaba))
(check-expect (play 31 wicked-witch greedy-witch)
              (list 31 28 25 22 19 16 13 10 7 4 1 0 'Alice))
(check-expect (play 32 wicked-witch greedy-witch)
              (list 32 29 26 23 20 17 14 11 8 5 2 1 0 'Elphaba))

;; 1c
(define smart-witch 
  (make-player 'Dorothy
               (lambda (tiles)
                 (cond
                   [(or (= tiles 8)
                        (= tiles 4)) 3]
                   [(or (= tiles 7)
                        (= tiles 3)) 2]
                   [(or (= tiles 6)
                        (= tiles 2)) 1]
                   [else 1]))))

;;;;;;;;;;;;;;;
;; Problem 2
;;;;;;;;;;;;;;;
(check-expect (sort '(8 5 2 0 6 1 3) <)
              (list 0 1 2 3 5 6 8))
(check-expect (sort '("Dog" "cat" "bAT" "ANT" "elk") string-ci>?)
              (list "elk" "Dog" "cat" "bAT" "ANT"))

; jumble : [ListOf X] -> [ListOf X]
; (jumble ls) returns the result of sorting the list by using sort.
(define (jumble ls)
  (sort ls
        ; X Y -> Bool
        (λ (x y) (= (random 2) 1))))

(check-satisfied (jumble '(2 3 5 6)) cons?)
(check-satisfied (jumble '(8 5 2 0 6 1 3)) cons?)
(check-satisfied (jumble '("Dog" "cat" "bAT" "ANT" "elk"))
                 cons?)

;;;;;;;;;;;;;;;
;; Problem 3
;;;;;;;;;;;;;;;
;; 3a
#|
  A Packet is a (make-packet Symbol String Nat Nat])
|#
(define-struct packet (tag segment seq-number count))
(define a-packet (make-packet 'brian "red fish" 3 9))

; string->packets : String Symbol Nat -> [ListOf Packet]
; (string->packets word tag n) returns a list of packets.
(define (string->packets word tag n)
  (local
    [(define (helper word tag n1 n2)
      (cond
        [(<= (string-length word) n)
         (cons (make-packet tag word
               (add1 (- n2 (ceiling (/ (string-length word) n1)))) n2)
               '())]
    [(> (string-length word) n)
     (cons (make-packet tag
                        (substring word 0 n)
                        (add1 (- n2
                           (ceiling (/ (string-length word) n1))))
                        n2)
           (helper (substring word n) tag n1 n2))]))]
    (helper word tag n (ceiling (/ (string-length word) n)))))

(check-expect (packet-tag a-packet) 'brian)
(check-expect (packet-segment a-packet) "red fish")
(check-expect (packet-seq-number a-packet) 3)
(check-expect (packet-count a-packet) 9)

(check-expect (string->packets "How does it feel to be on your own"
                               'foo 10)
              (list (make-packet 'foo "How does i" 1 4)
                    (make-packet 'foo "t feel to " 2 4)
                    (make-packet 'foo "be on your" 3 4)
                    (make-packet 'foo " own" 4 4)))
(check-expect (string->packets "With no direction home" 'goo 5)
              (list (make-packet 'goo "With " 1 5)
                    (make-packet 'goo "no di" 2 5)
                    (make-packet 'goo "recti" 3 5)
                    (make-packet 'goo "on ho" 4 5)
                    (make-packet 'goo "me" 5 5)))
(check-expect (string->packets "Like a complete unknown" 'boo 7)
              (list (make-packet 'boo "Like a " 1 4)
                    (make-packet 'boo "complet" 2 4)
                    (make-packet 'boo "e unkno" 3 4)
                    (make-packet 'boo "wn" 4 4)))
(check-expect (string->packets "Like a rolling stone?" 'hoo 25)
              (list (make-packet 'hoo "Like a rolling stone?" 1 1)))

;; 3b
; reconstitute : Symbol [ListOf Packet] -> [Maybe String]
; (reconstitute tag ls) returns the message string corresponding to the
; tag, if all packets are present and accounted for. Otherwise, return
; #false.
(define (reconstitute tag ls)
  (local
    [(define
       (helper t l)
       (cond
         [(empty? l) '()]
         [(equal? t (packet-tag (first l)))
          (cons (first l)
                (helper t (rest l)))]
         [else (helper t (rest l))]))]
    (if (empty? (helper tag ls))
        #f
        (local
          [(define ans (sort (helper tag ls)
                             ; Packet Packet -> Packet Packet
                             (λ (x y)
                               (< (packet-seq-number x)
                                  (packet-seq-number y)))))]
          (if (= (packet-count (first ans)) (length ans))
              (foldr
               ; Packet String -> String
               (λ (x acc)
                 (string-append (packet-segment x) acc))
               ""
               ans)
              #f)))))

(define p1 (string->packets "Take that, my pretty!" 'toto 3))
(define p2 (rest (string->packets "There's no place like home."
                                  'tinman 4)))
(define p3 (string->packets "We're not in Kansas anymore." 'lion 5))
(define all-together (jumble (append p1 p2 p3)))

(check-expect (reconstitute 'toto all-together) "Take that, my pretty!")
(check-expect (reconstitute 'tinman all-together) #false)
(check-expect (reconstitute 'lion all-together)
              "We're not in Kansas anymore.")
;;;;;;;;;;;;;;;
;; Problem 4
;;;;;;;;;;;;;;;
;; 4a
#|
Peg is one of
 - 'A
 - 'B
 - 'C.
|#

;; 4b
#|
  A Move is a (make-move [PosInt Peg Peg])
|#

(define-struct move [disk from to])

(check-expect (make-move 5 'A 'C) (make-move 5 'A 'C))
(check-expect (move? (make-move 1 'A 'C)) #true)
(check-expect (move-disk (make-move 63 'A 'B)) 63)
(check-expect (move-from (make-move 15 'B 'C)) 'B)
(check-expect (move-to (make-move 31 'C 'A)) 'A)

;;4c
; hanoi : Disk -> [ListOf Move]
; (hanoi n) eturns a list of moves that will transfer all the disks to
; peg 'C.
(define (hanoi n)
  (local
    [(define (helper n from to other)
       (cond
         [(= n 1) (list (make-move 1 from to))]
         [else (append
                (helper (sub1 n) from other to)
                (list (make-move n from to))
                (helper (sub1 n) other to from))]))]
    (helper n 'A 'C 'B)))

(check-expect (hanoi 3) (list (make-move 1 'A 'C) (make-move 2 'A 'B)
                              (make-move 1 'C 'B) (make-move 3 'A 'C)
                              (make-move 1 'B 'A) (make-move 2 'B 'C)
                              (make-move 1 'A 'C)))
(check-expect (hanoi 4) (list (make-move 1 'A 'B) (make-move 2 'A 'C)
                              (make-move 1 'B 'C) (make-move 3 'A 'B)
                              (make-move 1 'C 'A) (make-move 2 'C 'B)
                              (make-move 1 'A 'B) (make-move 4 'A 'C)
                              (make-move 1 'B 'C) (make-move 2 'B 'A)
                              (make-move 1 'C 'A) (make-move 3 'B 'C)
                              (make-move 1 'A 'B) (make-move 2 'A 'C)
                              (make-move 1 'B 'C)))
(check-expect (hanoi 6) (list (make-move 1 'A 'B) (make-move 2 'A 'C)
                              (make-move 1 'B 'C) (make-move 3 'A 'B)
                              (make-move 1 'C 'A) (make-move 2 'C 'B)
                              (make-move 1 'A 'B) (make-move 4 'A 'C)
                              (make-move 1 'B 'C) (make-move 2 'B 'A)
                              (make-move 1 'C 'A) (make-move 3 'B 'C)
                              (make-move 1 'A 'B) (make-move 2 'A 'C)
                              (make-move 1 'B 'C) (make-move 5 'A 'B)
                              (make-move 1 'C 'A) (make-move 2 'C 'B)
                              (make-move 1 'A 'B) (make-move 3 'C 'A)
                              (make-move 1 'B 'C) (make-move 2 'B 'A)
                              (make-move 1 'C 'A) (make-move 4 'C 'B)
                              (make-move 1 'A 'B) (make-move 2 'A 'C)
                              (make-move 1 'B 'C) (make-move 3 'A 'B)
                              (make-move 1 'C 'A) (make-move 2 'C 'B)
                              (make-move 1 'A 'B) (make-move 6 'A 'C)
                              (make-move 1 'B 'C) (make-move 2 'B 'A)
                              (make-move 1 'C 'A) (make-move 3 'B 'C)
                              (make-move 1 'A 'B) (make-move 2 'A 'C)
                              (make-move 1 'B 'C) (make-move 4 'B 'A)
                              (make-move 1 'C 'A) (make-move 2 'C 'B)
                              (make-move 1 'A 'B) (make-move 3 'C 'A)
                              (make-move 1 'B 'C) (make-move 2 'B 'A)
                              (make-move 1 'C 'A) (make-move 5 'B 'C)
                              (make-move 1 'A 'B) (make-move 2 'A 'C)
                              (make-move 1 'B 'C) (make-move 3 'A 'B)
                              (make-move 1 'C 'A) (make-move 2 'C 'B)
                              (make-move 1 'A 'B) (make-move 4 'A 'C)
                              (make-move 1 'B 'C) (make-move 2 'B 'A)
                              (make-move 1 'C 'A) (make-move 3 'B 'C)
                              (make-move 1 'A 'B) (make-move 2 'A 'C)
                              (make-move 1 'B 'C)))
(check-expect (length (hanoi 15)) 32767)

;;;;;;;;;;;;;;;
;; Problem 5
;;;;;;;;;;;;;;;
;; 5a
; subset-sum? : [ListOf Nat] Nat -> Bool
; (subset-sum? ls n) returns true if there is some subset of elements
; from ls that sums to the target, and #false otherwise.
(define (subset-sum? ls n)
  (cond
    [(empty? ls) (zero? n)]
    [else (or (subset-sum? (rest ls) n)
              (subset-sum? (rest ls) (- n (first ls))))]))

(check-expect (subset-sum? '() 0) #true)
(check-expect (subset-sum? '(5 9) -2) #false)
(check-expect (subset-sum? '(5 9) 5) #true)
(check-expect (subset-sum? '(5 9) 9) #true)
(check-expect (subset-sum? '(5 9) 8) #false)
(check-expect (subset-sum? '(5 9) 14) #true)
(check-expect (subset-sum? '(1 8 2 4) 7) #true)
(check-expect (subset-sum? '(15 22 14 26 32 9 16 8) 53) #true)
(check-expect (subset-sum? '(15 22 14 26 32 9 16 8) 28) #false)

;; 5b
; subset-sum : [ListOf Nat] Nat -> Bool
; (subset-sum ls n) returns #false if there is no subset of elements
; from ls that sums to the target.
(define (subset-sum ls n)
  (cond
    [(empty? ls) (if (zero? n)
                     '()
                     #f)]
    [else (local
            [(define a1 (subset-sum (rest ls) n))
             (define a2 (subset-sum (rest ls) (- n (first ls))))]
            (if (false? a1)
                (if (false? a2)
                    #f
                    (cons (first ls) a2))
                a1))]))

(check-expect (subset-sum '() 0) '())
(check-expect (subset-sum '(5 9) 5) (list 5))
(check-expect (subset-sum '(5 9) 9) (list 9))
(check-expect (subset-sum '(5 9) 8)  #false)
(check-expect (subset-sum '(5 9) 14) (list 5 9))
(check-expect (subset-sum '(-5 -9) -14) (list -5 -9))
(check-expect (subset-sum '(1 8 2 4) 7) (list 1 2 4))
(check-expect (subset-sum '(1 3 -2 9 2 5) 6) (list 3 -2 5))
(check-expect (subset-sum '(1 3 -2 9 2 5 -8) -4) (list 1 -2 5 -8))
(check-expect (subset-sum '(15 22 14 26 32 9 16 8) 53) (list 22 14 9 8))
(check-expect (subset-sum '(15 22 14 26 32 9 16 8) 28)
              #false)

;;;;;;;;;;;;;;;
;; Problem 6
;;;;;;;;;;;;;;;
(define-struct empty-tree [])
(define-struct node [data left right])

;; 6a
(define et (make-empty-tree))
(define tr1 (make-node 5 et et))
(define tr2 (make-node 7 tr1 et))
(define tr3 (make-node 4 tr1 tr2))
(define tr4 (make-node 8
                       (make-node 9
                                  (make-node 8 et et)
                                  et)
                       (make-node 3
                                  et
                                  (make-node 2 et et))))
(define tr5 (make-node 3
                       (make-node 4 et et)
                       (make-node 1
                                  (make-node 6 et et)
                                  et)))
(define tr6 (make-node 7 tr4 tr5))

(check-expect et (make-empty-tree))
(check-expect (node-data tr4) 8)
(check-expect (node-data tr5) 3)
(check-expect (node-data tr6) 7)
(check-expect (node-data (node-left (node-right tr6))) 4)

;; 6b
; tree-of? : [X -> Bool] [ListOf Tree] -> Bool
; (tree-of? f tr) decides whether the given input is or is not a tree
; whose elements satisify the given type predicate.
(define (tree-of? f tr)
  (or
   (empty-tree? tr)
   (and
    (node? tr)
    (f (node-data tr))
    (tree-of? f (node-left tr))
    (tree-of? f (node-left tr)))))

(define (x? any) #true)

(check-expect (tree-of? x? 5) #false)
(check-expect (tree-of? x? '()) #false)
(check-expect (tree-of? number? tr6) #true)
(check-expect (tree-of? string? tr6) #false)
(check-expect (tree-of? number? (make-node 1 2 3)) #false)
(check-expect (tree-of? number? (make-node 1 (make-node 'a tr4
              (make-node 4 5 tr6)) tr5)) #false)

;; 6c
; make-leaf : X -> Tree
; (make-leaf item) returns a tree with exactly one node, and that node
; contains the given item in its data field.
(define (make-leaf item)
  (make-node item et et))

(define tr7 (make-node "dog"
                       (make-node "cat" (make-leaf "ant") et)
                       (make-node "rat" (make-leaf "gnu") et)))

(check-expect (make-leaf "green")
              (make-node "green" (make-empty-tree) (make-empty-tree)))
(check-expect (node? (make-leaf 'a)) #true)
(check-expect (node-data (make-leaf '(a b c))) (list 'a 'b 'c))
(check-expect (node-left (make-leaf 7)) (make-empty-tree))
(check-expect (node-right (make-leaf #true)) (make-empty-tree))
(check-expect (tree-of? string? tr7) #true)

;; 6d
; leaf? : X -> Bool
; (leaf? item) returns #true if it is a leaf node, and #false otherwise.
(define (leaf? item)
  (if
   (equal? item et)
   #f
   (and (empty-tree? (node-left item))
        (empty-tree? (node-right item)))))

(check-expect (leaf? (make-leaf "green")) #true)
(check-expect (leaf? (node-left tr4)) #false)
(check-expect (leaf? (node-left (node-left tr4))) #true)
(check-expect (leaf? (node-right (node-left tr4))) #false)

;; 6e
; tree-size : Tree -> Nat
; (tree-size tr) returns the number of nodes in the tree.
(define (tree-size tr)
  (cond
    [(empty-tree? tr) 0]
    [else (local
       [(define ansl (tree-size (node-left tr)))
        (define ansr (tree-size (node-right tr)))]
            (+ ansl ansr 1))]))

(check-expect (tree-size (make-empty-tree)) 0)
(check-expect (tree-size (make-leaf 'a)) 1)
(check-expect (tree-size tr4) 5)
(check-expect (tree-size tr5) 4)
(check-expect (tree-size tr6) 10)

;;;;;;;;;;;;;;;
;; Problem 7
;;;;;;;;;;;;;;;
; height : Tree -> Nat
; (height tr) returns the height of the tree.
(define (height tr)
  (cond
    [(empty-tree? tr) 0]
    [else (local
       [(define ansl (height (node-left tr)))
        (define ansr (height (node-right tr)))]
            (if (< ansl ansr)
                (add1 ansr)
                (add1 ansl)))]))

(check-expect (height (make-empty-tree)) 0)
(check-expect (height (make-leaf 5)) 1)
(check-expect (height (make-node 8 (make-empty-tree) (make-leaf 2))) 2)
(check-expect (height (make-node 8  (make-leaf 2) (make-empty-tree))) 2)
(check-expect (height tr4) 3)
(check-expect (height tr5) 3)
(check-expect (height tr6) 4)
(check-expect (height (make-node 0 (make-empty-tree) tr6)) 5)
(check-expect (height (make-node 0 tr6 (make-empty-tree))) 5)

;;;;;;;;;;;;;;;
;; Problem 8
;;;;;;;;;;;;;;;
;; 8a
#|
  An [ExprOf X] is one of
  - X
  - (make-expr [X X -> X] Expr Expr)
|#

(define-struct expr [op rand1 rand2])

;; 8b
; evaluate : [ExprOf X] -> X
; (evaluate e) returns the result of applying all operations in
; expr and reducing it down to a single value
(define (evaluate e)
  (cond
    [(not (expr? e)) e]
    [else ((expr-op e)
           (evaluate (expr-rand1 e))
           (evaluate (expr-rand2 e)))]))

(define e1 (make-expr string-append
                   (make-expr string-append
                              (make-expr string-append "age" "-")
                              (make-expr string-append "of" "-"))
                   "wisdom"))
(define e2
    (make-expr above
               (rectangle 200 50 'solid 'orange)
               (make-expr beside
                          (make-expr above
                                     (make-expr beside
                                     (square 50 'solid 'red)
                                     (square 50 'solid 'violet))
                                     (make-expr beside
                                     (square 50 'solid 'blue)
                                     (square 50 'solid 'green)))
                          (rectangle 100 100 'solid 'yellow))))
(check-expect (evaluate e1) "age-of-wisdom")

;; 8c
; flip-expr : [ExprOf X] -> X
; (flip-expr e) returns the expression formed by swapping each
; operator’s operands in the given expression.
(define (flip-expr e)
  (cond
    [(not (expr? e)) e]
    [else ((expr-op e)
           (flip-expr (expr-rand2 e))
           (flip-expr (expr-rand1 e)))]))

(check-expect (evaluate (flip-expr e1)) "wisdom-of-age")
(check-satisfied (evaluate (flip-expr e2)) image?)

;;;;;;;;;;;;;;;
;; Problem 9
;;;;;;;;;;;;;;;
;; 9a
; infect-op : [Num Num -> Num] PosInt String -> [Num Num -> Num]
; (infect-op f n w) returns an operator value that has been infected
; with a virus.
(define (infect-op f n w)
  ; Num Num -> Num
  (λ (x y)
    (if (zero? (random n))
        (error w)
        (f x y))))

(define op+ (infect-op + 4 "Gotcha!!"))
(check-satisfied op+ procedure?)

;; 9b
; infect-expr : [ExprOf X] PosInt String -> [ExprOf X]
; (infect-expr e n w) returns an expression value that appears to be
; the same as the given expression except that each operator has been
; infected with a virus, as specified by infect-op.
(define (infect-expr e n w)
  (if
   (not (expr? e))
   e
   (make-expr (infect-op (expr-op e) n w)
              (infect-expr (expr-rand1 e) n w)
              (infect-expr (expr-rand2 e) n w))))

(define e3 (make-expr + (make-expr - 5 3) (make-expr * 4 6)))
(define e4 (infect-expr e3 10 "ILOVEYOU"))

(check-satisfied e3 expr?)
(check-satisfied e4 expr?)