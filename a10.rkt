;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname a10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

(define handin "a10")
(define collaboration-statement "I worked alone")


;;;;;;;;;;;;;;;
;; Problem 1
;;;;;;;;;;;;;;;
; join-together : [ListOf X] [ListOf X] -> [ListOf X]
; (join-together ls1 ls2) returns the list consisting of all
; top-level elements of ls1 followed by the top-level elements
; of ls2. 
(define (join-together ls1 ls2)
  (local [(define (connect ls)
            (append ls ls2))]
    (connect ls1)))
            

(check-expect (join-together '(a b c) '(d e f g h))
              (list 'a 'b 'c 'd 'e 'f 'g 'h))
(check-expect (join-together '() '(7 2 0 1 8 3 4))
              (list 7 2 0 1 8 3 4))

;;;;;;;;;;;;;;;
;; Problem 2
;;;;;;;;;;;;;;;
;; 2a
; flatten/v1 : [ListOf X] -> [ListOf Y]
; (flatten/v1 lls) returns a list containing all the elements in
; the small list which belongs to the big list.
(define (flatten/v1 lls)
  (cond
   [(empty? lls) '()]
   [else (join-together (first lls)
                        (flatten/v1 (rest lls)))]))

(check-expect (flatten/v1 '()) '())
(check-expect (flatten/v1 '((a a a a) (b b) (c c c c c c) () (d)
                                      (e e e)))
           (list 'a 'a 'a 'a 'b 'b 'c 'c 'c 'c 'c 'c 'd 'e 'e 'e))
(check-expect (flatten/v1 '((3 4 5) (0 0) (2 5 6) (a d)))
                          '(3 4 5 0 0 2 5 6 a d))
(check-expect (flatten/v1 '((100 A) (80 B) (70 C) (60 D)))
                          '(100 A 80 B 70 C 60 D))
(check-satisfied (flatten/v1 '((100 A) (80 B) (70 C) (60 D)))
                 cons?)

; flatten/v2 : [ListOf X] -> [ListOf Y]
; (flatten/v2 lls) returns a list containing all the elements in
; the small list which belongs to the big list with higher speed.
(define (flatten/v2 lls)
  (cond
   [(empty? lls) '()]
   [(empty? (rest lls)) (first lls)]
   [else (flatten/v2 (cons (join-together (first lls) (second lls))
                           (rest (rest lls))))]))

(check-expect (flatten/v2 '()) '())
(check-expect (flatten/v2 '((a a a a) (b b) (c c c c c c) () (d)
                                      (e e e)))
           (list 'a 'a 'a 'a 'b 'b 'c 'c 'c 'c 'c 'c 'd 'e 'e 'e))
(check-expect (flatten/v1 '((1 24) (red yellow) (2 5 6) (a d)))
                          '(1 24 red yellow 2 5 6 a d))
(check-expect (flatten/v1 '((100 A) (80 B) (70 C) (60 D)))
                          '(100 A 80 B 70 C 60 D))
(check-satisfied (flatten/v1 '((100 A) (80 B) (70 C) (60 D)))
                 cons?)

;; 2b
#|
(flatten/v1 '((a b c) (d e) (f g h i)))
== (join-together '(a b c) (flatten/v1 '((d e) (f g h i))))
== (join-together (flatten/v1 '(a b c) '(d e f g h i)))
== (flatten/v1 '((a b c) (d e f g h i)))
== (join-together (flatten/v1 '((a b c) (d e f g h i))))
== (flatten/v1 '() '(a b c d e f g h i))
== (((empty? lls) '()) '(a b c d e f g h i))
== '(a b c d e f g h i)

;; 2c
(flatten/v2 '((a b c) (d e) (f g h i)))
== (flatten/v2 (cons (join-together '(a b c) '(d e)) '((f g h i))))
== (flatten/v2 '((a b c d e) (f g h i)))
== (flatten/v2 (cons (join-together '(a b c d e) '(f g h i)) '()))
== (flatten/v2 '((a b c d e f g h i) '()))
== (flatten/v2 (cons '(a b c d e f g h i) '()))
== ((empty? '()) '(a b c d e f g h i))
== '(a b c d e f g h i)
|#

;;;;;;;;;;;;;;;
;; Problem 3
;;;;;;;;;;;;;;;
;; 3a
(define (ignore x) #true)
(check-expect (make-list 5 (make-list 3 'a))
  (list (list 'a 'a 'a) (list 'a 'a 'a) (list 'a 'a 'a) (list 'a 'a 'a)
        (list 'a 'a 'a)))
(check-expect (ignore (flatten/v1 (make-list 5 (make-list 3 'a))))
              #true)
(check-expect (ignore (flatten/v2 (make-list 5 (make-list 3 'a))))
              #true)

(define ls1 (make-list 20 (make-list 3 'a)))
(define ls2 (make-list 50 (make-list 3 'a)))
(ignore (time (flatten/v1 ls1)))
(ignore (time (flatten/v2 ls1)))
(ignore (time (flatten/v1 ls2)))
(ignore (time (flatten/v2 ls2)))

#|
   n    |  flatten/v1  | flatten/v2
====================================
   20         0              0
   50         16             0
  100         16             0
  200         0              0
  300         0              0
   :          :              :
 1000         15            110
 2000         15            234
 3000         16            281
|#

;; 3b
(define flatten flatten/v1)

;; 3c
; Because faltten/v1 has less process for getting the answer and
; flatten/v2 needs more requirements, it will takes longer time to get
; the answer.
; Flatten/v1 are almost the same speed as flatten/v2 when the given
; list length is small. However, when the list length becomes big,
; flatten/v1 runs ten times faster than flatten/v2.

;;;;;;;;;;;;;;;
;; Problem 4
;;;;;;;;;;;;;;;
; list-head : Nat [ListOf X] -> [ListOf X]
; (list-head n ls) returns a new list contains the first n elements
; in the old list.
(define (list-head init-n init-ls)
  (local [(define (list-head/help n ls)
            (cond
              [(zero? n) '()]
              [(empty? ls)
               (error 'list-head
                      (format "~s is too large for ~s"
                              init-n init-ls))]
              [else (cons (first ls)
                          (list-head/help (sub1 n) (rest ls)))]))]
    (list-head/help init-n init-ls)))

(check-error (list-head 5 '(a b c))
             "list-head: 5 is too large for (a b c)")
(check-expect (list-head 3 '(a b c d)) '(a b c))

; list-tail : Nat [ListOf X] -> [ListOf X]
; (list-tail n ls) returns a new list contains the last (1-n) elements
; in the old list.
(define (list-tail init-n init-ls)
  (local [(define (list-tail/help n ls)
            (cond
              [(zero? n) ls]
              [(empty? ls)
               (error 'list-tail
                      (format "~s is too large for ~s"
                              init-n init-ls))]
              [else (list-tail/help (sub1 n) (rest ls))]))]
    (list-tail/help init-n init-ls)))

(check-error (list-tail 5 '(a b c))
             "list-tail: 5 is too large for (a b c)")
(check-expect (list-tail 3 '(a b c d)) '(d))
(check-expect (list-tail 1 '(a b c d)) '(b c d))

;;;;;;;;;;;;;;;
;; Problem 5
;;;;;;;;;;;;;;;
; pop-up : Nat [ListOf X] -> [ListOf X]
; (pop-up n ls) returns the result of grouping runs of length n
; in ls into sublists.
(define (pop-up n ls)
  (cond
    [(empty? ls) '()]
    [else (cons (list-head n ls)
                (pop-up n (list-tail n ls)))]))

(check-expect (pop-up 2 '(a b c d e f g h))
              (list (list 'a 'b) (list 'c 'd)
                    (list 'e 'f) (list 'g 'h)))
(check-expect (pop-up 3 '(a b c d e f))
              (list (list 'a 'b 'c) (list 'd 'e 'f)))
(check-expect (pop-up 5 (make-list 20 0))
              (list (list 0 0 0 0 0) (list 0 0 0 0 0)
                    (list 0 0 0 0 0) (list 0 0 0 0 0)))

;;;;;;;;;;;;;;;
;; Problem 6
;;;;;;;;;;;;;;;
; overwrite/slow : [ListOf X] Nat X -> [ListOf X]
; (overwrite/slow ls i x) returns the list that results from
; replacing the element at index i in ls with x.
 
(define (overwrite/slow ls i x)
  (join-together (list-head i ls)
    (join-together (list x) (list-tail (add1 i) ls))))
 
(check-expect (overwrite/slow '(a) 0 'b) '(b))
(check-expect (overwrite/slow '(a b c) 0 '_) '(_ b c))
(check-expect (overwrite/slow '(a b c) 1 '_) '(a _ c))
(check-expect (overwrite/slow '(a b c) 2 '_) '(a b _))
(check-expect (overwrite/slow '(x x x _ x x x x) 3 'x)
              (make-list 8 'x))

;; 6a
; overwrite : [ListOf X] Nat X -> [ListOf X]
; (overwrite ls i x) returns the list that results from
; replacing the element at index i in ls with x.
(define (overwrite ls i x)
  (local
    [(define (faster current-ls current-i current-x)
       (cond
         [(or (empty? current-ls)
              (negative? current-i))
          (error 'overwrite
                 (format "~s is out of bounds for ~s"
                         i ls))]
         [else (if (zero? current-i)
                   (cons current-x (rest current-ls))
                   (cons (first current-ls)
                         (faster (rest current-ls) (sub1 current-i)
                                 current-x)))]))]
    (faster ls i x)))


(check-expect (overwrite '(a b c) 1 '_) (list 'a '_ 'c))
(check-error (overwrite '(5 4 3 2 1) 5 6)
              "overwrite: 5 is out of bounds for (5 4 3 2 1)")
(check-error (overwrite '(5 4 3 2 1) -3 6)
              "overwrite: -3 is out of bounds for (5 4 3 2 1)")

;; 6b
(local [(define n 10000)
        (define ls (make-list n 0))]
  (list
   (ignore (time (overwrite ls (- n 1) 1)))
   (ignore (time (overwrite/slow ls (- n 1) 1)))))
#|
   n    | overwrite | overwrite/slow
====================================
 10000        0             16
 20000        31            31
 30000        47            47
 40000        78            93
   :          :              :
 100000      203            222
|#
; Because overwrite takes less process, the recursion
; goes faster than overwrite/slow. The speed given by
; overwrite is a little bit faster than overwrite.

;;;;;;;;;;;;;;;
;; Problem 7
;;;;;;;;;;;;;;;
;; 7a
; i.
#|
 TileValue is One Of
 - BLANK
 - 2
 - 4
 - 8
 - 16
 - 32
 - 64
 - 128
 - 256
 - 512
 - 1024
 - 2048
|#
(define BLANK '_)

; ii.
(define (blank? item) (equal? item BLANK))

(check-expect (blank? BLANK) #true)
(check-expect (blank? 2048) #false)
(check-expect (blank? blank?) #false)

;; 7b
#|
  Board is one of
 - (cons (cons Nat '()) '())
 - (cons (cons Nat '()) Board)
|#

(define b1 (list (list 64 32) (list 16 16)))
(define b2 (list (list 2 2 2 2)
                 (list 4 '_ 4 '_)
                 (list '_ 8 8 8)
                 (list 16 '_ '_ 16)))

(define b3 (list (list 16 64 8 256 4)
                 (list 1024 1024 1024 32 128)
                 (list 64 32 128 '_ '_)
                 (list 4 4 32 '_ '_)
                 (list 2 '_ '_ 512 '_)))

;; 7c
; board-full? : Board -> Bool
; (board-full? bd) returns #true if the board has no blank
; tiles, and #false otherwise.
(define (board-full? bd)
  (= (length (filter blank? (flatten bd))) 0))

(check-expect (board-full? '((4))) #true)
(check-expect (board-full? b1) #true)
(check-expect (board-full? b2) #false)

;; 7d
; add-new-tile : Board -> Board
; (add-new-tile ls) attempts to replace one of the blank tiles with a
; new tile.
(define (add-new-tile ls)
  (cond
    [(board-full? ls) ls]
    [else (local [(define add/helper (flatten/v1 ls))
                  (define num (length add/helper))
                  (define (index l num)
                    (local [(define n (random num))]
                      (if (blank? (list-ref l n))
                          n
                          (index l num))))]
            (pop-up (sqrt num)
                    (overwrite add/helper (index add/helper num)
                               (if
                     (<= 3 (random 5))
                     2
                     4))))]))

(check-expect (add-new-tile '((4))) (list (list 4)))
(check-expect (add-new-tile b1) (list (list 64 32) (list 16 16)))
(check-satisfied (add-new-tile b2) cons?)
(check-satisfied (add-new-tile (add-new-tile (add-new-tile b3))) cons?)

;; 7e
(define (iterate f n x)
  (cond
    [(zero? n) x]
    [else (iterate f (sub1 n) (f x))]))

; i.
; iterate : [X -> X] Nat X -> X
; (iterate f n x) returns a result of applying f n times starting with
; x.
(check-expect (iterate add1 2 2) 4)
(check-expect (iterate sub1 4 3) -1)
(check-expect (iterate rest 4 '(a b c d e f g))
              (list 'e 'f 'g))

; ii.
; Yes. Because the recursive call is the last thing in the function
; and it does returns the value. If there is nothing to do after the
; function returns except return its value.


; iii.
; make-board : Int Nat -> Board
; (make-board n m) returns an nxn board with m non-blank tiles.
(define (make-board n m)
  (iterate add-new-tile m (pop-up n (make-list (* n n) BLANK))))

(check-expect (make-board 1 0) (list (list '_)))
(check-expect (make-board 0 0) '())
(check-satisfied (make-board 3 5) cons?)
(check-satisfied (make-board 3 5) cons?)
(check-satisfied (make-board 7 25) cons?)

;; 7f
; board-square? : Board -> Bool
; (board-square? bd) verifies that it is of dimension n x n for
; some positive integer n.
(define (board-square? bd)
  (cond
    [(empty? bd) #false]
    [else
     (local [(define n (length bd))
             (define (dengyu current ans)
               (and ans (equal? (length current) n)))]
       (foldr dengyu #true bd))]))

(check-expect (board-square? '()) #false)
(check-expect (board-square? b3) #true)
(check-expect (board-square? (make-board 123 0)) #true)
(check-expect (board-square? '((2 4) (8 16 32))) #false)
(check-expect (board-square? '((2 2 2 2) (4 4 4 4) (2 2 2)
                                         (4 4 4 4))) #false)

;; 7g
; game-won? : Board -> Bool
; (game-won? bd) returns #true if the board contains a 2048 tile,
; and #false otherwise.
(define (game-won? bd)
  (cond
    [(empty? bd) #false]
    [(member? 2048 (first bd)) #true]
    [else (game-won? (rest bd))]))

(check-expect (game-won? (list (list 2 2 2) (list 2 2048 2)
                               (list BLANK BLANK BLANK))) #true)
(check-expect (game-won? b3) #false)

;;;;;;;;;;;;;;;
;; Problem 8
;;;;;;;;;;;;;;;
;; 8a
; TILE-SIZE
(define TILE-SIZE 90)

; FONT-SIZE : the size of the text font used to label a one or
; two-digit tile, equal to one half the TILE-SIZE
(define FONT-SIZE (quotient TILE-SIZE 2))

; GRID-SPACING : the number of pixels framing each tile.
(define GRID-SPACING 10)

; GRID-COLOR : the color of the outer frame and grid lines on the
; board, equal to a color with R=186, G=172, and B=160.
(define GRID-COLOR (make-color 186 172 160))

;; 8b
; tile->image : TileValue Nat Color Color -> Image
; (tile->image n1 n2 foreground-color background-color) produces
; an image of the tile with the given properties.
(define (tile->image n1 font-size foreground-color background-color)
  (overlay
   (text (number->string n1)
         font-size
         foreground-color)
  (square (- TILE-SIZE GRID-SPACING) "solid" background-color)
  (square TILE-SIZE "solid" GRID-COLOR)))

(tile->image 64 FONT-SIZE
    (make-color 255 255 255) (make-color 246 94 59))
(tile->image 2048 (- FONT-SIZE 8)
    (make-color 255 255 255) (make-color 237 194 46))

;; 8c
; RGB : Nat -> Image
; (RGB n) returns a image by using given number.
(define (RGB n)
  (cond
    [(equal? n 2)
     (tile->image n FONT-SIZE (make-color 105 105 105)
                  (make-color 238 228 218))]
    [(equal? n 4)
     (tile->image n FONT-SIZE (make-color 105 105 105)
                  (make-color 237 224 200))]
    [(equal? n 8)
     (tile->image n FONT-SIZE (make-color 255 255 255)
                  (make-color 242 177 121))]
    [(equal? n 16)
     (tile->image n (- FONT-SIZE 4) (make-color 255 255 255)
                  (make-color 245 149 99))]
    [(equal? n 32)
     (tile->image n (- FONT-SIZE 4) (make-color 255 255 255)
                  (make-color 246 124 95))]
    [(equal? n 64)
     (tile->image n (- FONT-SIZE 4) (make-color 255 255 255)
                  (make-color 246 94 59))]
    [(equal? n 128)
     (tile->image n (- FONT-SIZE 8) (make-color 255 255 255)
                  (make-color 237 207 114))]
    [(equal? n 256)
     (tile->image n (- FONT-SIZE 8) (make-color 255 255 255)
                  (make-color 237 204 97))]
    [(equal? n 512)
     (tile->image n (- FONT-SIZE 8) (make-color 255 255 255)
                  (make-color 237 200 80))]
    [(equal? n 1024)
     (tile->image n (- FONT-SIZE 16) (make-color 255 255 255)
                  (make-color 237 197 63))]
    [(equal? n 2048)
     (tile->image n (- FONT-SIZE 16) (make-color 255 255 255)
                  (make-color 237 194 46))]
    [else #false]))

; val->image : Nat -> Image
; (val->image num) produces the corresponding image, using the
; game sizes and colors.
(define (val->image n)
  (cond
   [(equal? n BLANK) (overlay
      (square (- TILE-SIZE GRID-SPACING) "solid"
              (make-color 204 192 179))
      (square TILE-SIZE "solid" GRID-COLOR))]
    [(not (member? n '(2 4 8 16 32 64 128 256 512 1024 2048)))
     (error 'val->image
             (format "unknown tile value ~s" n))]
    [else (RGB n)]))

(val->image BLANK)
(val->image 64)
(check-error (val->image 255) "val->image: unknown tile value 255")
(check-satisfied (map val->image (list BLANK 2 4 8 16 32)) cons?)
(check-satisfied (map val->image (list 64 128 256 512 1024 2048)) cons?)

;; 8d
; board->image : Board -> Image
; (board->image bd) returns the corresponding image.
(define (board->image bd)
  (overlay
   (local [(define (my-function l)
            (foldr beside empty-image l))]
    (foldr above empty-image
         (map my-function
              (pop-up 4 (map val->image (flatten bd))))))
   (square (+ GRID-SPACING (* 4 TILE-SIZE))
           "solid" GRID-COLOR)))

(board->image (list (list BLANK 2 4 8)
                      (list 16 BLANK 32 64)
                      (list 128 256 BLANK 512)
                      (list BLANK 1024 2048 BLANK)))