;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname a11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define handin "a11")
(define collaboration-statement "I worked alone")

;;;;;;;;;;;;;;;
;; Needed
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

; flatten : [ListOf X] -> [ListOf Y]
; (flatten lls) returns a list containing all the elements in
; the small list which belongs to the big list.
(define (flatten lls)
  (cond
   [(empty? lls) '()]
   [else (join-together (first lls)
                        (flatten (rest lls)))]))

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

; iterate : [X -> X] Nat X -> X
; (iterate f n x) returns a result of applying f n times starting with
; x.
(define (iterate f n x)
  (cond
    [(zero? n) x]
    [else (iterate f (sub1 n) (f x))]))

(check-expect (iterate add1 2 2) 4)
(check-expect (iterate sub1 4 3) -1)
(check-expect (iterate rest 4 '(a b c d e f g))
              (list 'e 'f 'g))

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

(define BLANK '_)

(define (blank? item) (equal? item BLANK))

(check-expect (blank? BLANK) #true)
(check-expect (blank? 2048) #false)
(check-expect (blank? blank?) #false)

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

; board-full? : Board -> Bool
; (board-full? bd) returns #true if the board has no blank
; tiles, and #false otherwise.
(define (board-full? bd)
  (= (length (filter blank? (flatten bd))) 0))

(check-expect (board-full? '((4))) #true)
(check-expect (board-full? b1) #true)
(check-expect (board-full? b2) #false)

; add-new-tile : Board -> Board
; (add-new-tile ls) attempts to replace one of the blank tiles with a
; new tile.
(define (add-new-tile ls)
  (cond
    [(board-full? ls) ls]
    [else (local [(define add/helper (flatten ls))
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

; make-board : Int Nat -> Board
; (make-board n m) returns an nxn board with m non-blank tiles.
(define (make-board n m)
  (iterate add-new-tile m (pop-up n (make-list (* n n) BLANK))))

(check-expect (make-board 1 0) (list (list '_)))
(check-expect (make-board 0 0) '())
(check-satisfied (make-board 3 5) cons?)
(check-satisfied (make-board 3 5) cons?)
(check-satisfied (make-board 7 25) cons?)

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

;;;;;;;;;;;;;;;
;; Problem 1
;;;;;;;;;;;;;;;
; invert/v1 : [ListOf X] -> [ListOf X]
; (invert/v1 ls) returns the result of reversing the top-level
; elements of ls
(define (invert/v1 ls)
  (local [; help : [ListOf X] [ListOf X] -> [ListOf X]
          (define (help ls acc)
            (cond
              [(empty? ls) acc]
              [else (help (rest ls) (cons (first ls) acc))]))]
    (help ls '())))

; invert/v2 : [ListOf X] -> [ListOf X]
; (invert/v2 ls) returns the result of reversing the top-level
; elements of ls
(define (invert/v2 ls)
  (cond
    [(empty? ls) '()]
    [else (append (invert/v2 (rest ls)) (list (first ls)))]))

;; 1a
#|
(invert/v1 '(a b c d e))
== (help '(a b c d e) '())
== (help '(b c d e) (cons '(a) '()))
== (help '(b c d e) '(a))
== (help '(c d e) (cons 'b '(a)))
== (help '(c d e) '(b a))
== (help '(d e) (cons 'c '(b a)))
== (help '(d e) '(c b a))
== (help '(e) (cons 'd '(c b a)))
== (help '(e) '(d c b a))
== (help '() (cons 'e '(d c b a)))
== (help '() '(e d c b a))
== '(e d c b a)
|#

;; 1b
#|
(invert/v2 '(a b c d e))
== (append (invert/v2 '(b c d e)) '(a))
== (append (append (invert/v2 '(c d e)) '(b)) '(a))
== (append (append (append (invert/v2 '(d e)) '(c)) '(b)) '(a)) 
== (append (append (append (append (invert/v2 '(e)) '(d))
                   '(c)) '(b)) '(a))
== (append (append (append (append (append (invert/v2 '()) '(e))
                              '(d)) '(c)) '(b)) '(a))
== (append (append (append (append (append (invert/v2 '()) '(e))
                              '(d)) '(c)) '(b)) '(a))
== (append (append (append (append (append '() '(e)) '(d)) '(c))
                   '(b)) '(a))
== (append (append (append (append '(e) '(d)) '(c)) '(b)) '(a))
== (append (append (append (append '(e d)) '(c)) '(b)) '(a))
== (append (append (append '(e d c)) '(b)) '(a))
== (append (append '(e d c b)) '(a))
== (append '(e d c b) '(a))
== '(e d c b a)
|#

;; 1c
; Yes. Because the recursive call is the last thing being evaluated in
; the function. The local help functions recursive call in invert/v1
; results in the returned calue of the function, os no more work has to
; be done after it is called.

;; 1d
; No. The last thing is not evaluated, and the recursive call is no
; longer being called. There still needs to be completionsteps in the
; order to arrive at the correct answer.

;; 1e
#|
     n       invert/v1     invert/v2     reverse
-------------------------------------------------
    100           0           0             0
   1000           0           16            0
  10000           16          906           0
 100000           94          138500        0
   :          
|#

;; 1f
; The slowest funcion is invert/v2 which runs a long time when we
; input n=100000. Because invert/v2 needs more process for getting
; the answer. At the same time, invert/v1 has local which helps it
; increase its speed and reverse only take one step to get the
; answer.

;; 1g
(define invert invert/v1)

;;;;;;;;;;;;;;;
;; Problem 2
;;;;;;;;;;;;;;;

;; 2a
#|
(check-expect (slide-row-left '()) '())
(check-expect (slide-row-left '(_)) (list '_))
(check-expect (slide-row-left '(2)) (list 2))
(check-expect (slide-row-left '(_ _)) (list '_ '_))
(check-expect (slide-row-left '(2 _)) (list 2 '_))
(check-expect (slide-row-left '(_ 2)) (list 2 '_))
(check-expect (slide-row-left '(2 2)) (list 4 '_))
(check-expect (slide-row-left '(2 4)) (list 2 4))
(check-expect (slide-row-left '(4 4)) (list 8 '_))
(check-expect (slide-row-left '(_ _ _)) (list '_ '_ '_))
(check-expect (slide-row-left '(2 _ _)) (list 2 '_ '_))
(check-expect (slide-row-left '(_ 2 _)) (list 2 '_ '_))
(check-expect (slide-row-left '(_ _ 2)) (list 2 '_ '_))
(check-expect (slide-row-left '(2 2 _)) (list 4 '_ '_))
(check-expect (slide-row-left '(2 _ 2)) (list 4 '_ '_))
(check-expect (slide-row-left '(_ 2 2)) (list 4 '_ '_))
(check-expect (slide-row-left '(2 4 _)) (list 2 4 '_))
(check-expect (slide-row-left '(2 _ 4)) (list 2 4 '_))
(check-expect (slide-row-left '(_ 2 4)) (list 2 4 '_))
(check-expect (slide-row-left '(2 2 2)) (list 4 2 '_))
(check-expect (slide-row-left '(4 2 2)) (list 4 4 '_))
(check-expect (slide-row-left '(2 4 2)) (list 2 4 2))
(check-expect (slide-row-left '(2 2 4)) (list 4 4 '_))
(check-expect (slide-row-left '(2 4 8)) (list 2 4 8))
(check-expect (slide-row-left '(2 8 4 4 2 8 8))
              (list 2 8 8 2 16 '_ '_))
(check-expect (slide-row-left '(2 2 4 8 4 2 8))
              (list 4 4 8 4 2 8 '_))
(check-expect (slide-row-left '(2 2 4 4 16 8 4 2 32 8 2 2))
              (list 4 8 16 8 4 2 32 8 4 '_ '_ '_))
(check-expect (slide-row-left '(4 8 4 2 2 16 16 32 2 4 2 4))
              (list 4 8 4 4 32 32 2 4 2 4 '_ '_))
|#

;; 2b
; slide-row-left : [ListOf TailValue] -> [ListOf TailValue]
; (slide-row-left row) returns a list which rows to left. If the
; numbers are the same, it will returns the accumulation of them,
; if not, it will stay as it used to be.

(define (slide-row-left row)
  (local [; help : [ListOf TailValue] [Maybe Nat] [ListOf TailValue]
          ; -> [ListOf TailValue]
          (define (help row last-number blanks)
            (cond
              [(empty? row) (if (number? last-number)
                                (cons last-number blanks)
                                blanks)]
              [(blank? (first row)) (help (rest row) last-number
                                           (cons BLANK blanks))]
              [(false? last-number)
               (help (rest row) (first row) blanks)]
              [(equal? (first row) last-number)
               (cons (+ (first row) last-number)
                     (help (rest row) #false (cons BLANK blanks)))]
              [else (cons last-number (help (rest row)
                                            (first row)
                                            blanks))]))]
    (help row #false '())))

(check-expect (slide-row-left '()) '())
(check-expect (slide-row-left '(_)) (list '_))
(check-expect (slide-row-left '(2)) (list 2))
(check-expect (slide-row-left '(_ _)) (list '_ '_))
(check-expect (slide-row-left '(2 _)) (list 2 '_))
(check-expect (slide-row-left '(_ 2)) (list 2 '_))
(check-expect (slide-row-left '(2 2)) (list 4 '_))
(check-expect (slide-row-left '(2 4)) (list 2 4))
(check-expect (slide-row-left '(4 4)) (list 8 '_))
(check-expect (slide-row-left '(_ _ _)) (list '_ '_ '_))
(check-expect (slide-row-left '(2 _ _)) (list 2 '_ '_))
(check-expect (slide-row-left '(_ 2 _)) (list 2 '_ '_))
(check-expect (slide-row-left '(_ _ 2)) (list 2 '_ '_))
(check-expect (slide-row-left '(2 2 _)) (list 4 '_ '_))
(check-expect (slide-row-left '(2 _ 2)) (list 4 '_ '_))
(check-expect (slide-row-left '(_ 2 2)) (list 4 '_ '_))
(check-expect (slide-row-left '(2 4 _)) (list 2 4 '_))
(check-expect (slide-row-left '(2 _ 4)) (list 2 4 '_))
(check-expect (slide-row-left '(_ 2 4)) (list 2 4 '_))
(check-expect (slide-row-left '(2 2 2)) (list 4 2 '_))
(check-expect (slide-row-left '(4 2 2)) (list 4 4 '_))
(check-expect (slide-row-left '(2 4 2)) (list 2 4 2))
(check-expect (slide-row-left '(2 2 4)) (list 4 4 '_))
(check-expect (slide-row-left '(2 4 8)) (list 2 4 8))
(check-expect (slide-row-left '(2 8 4 4 2 8 8))
              (list 2 8 8 2 16 '_ '_))
(check-expect (slide-row-left '(2 2 4 8 4 2 8))
              (list 4 4 8 4 2 8 '_))
(check-expect (slide-row-left '(2 2 4 4 16 8 4 2 32 8 2 2))
              (list 4 8 16 8 4 2 32 8 4 '_ '_ '_))
(check-expect (slide-row-left '(4 8 4 2 2 16 16 32 2 4 2 4))
              (list 4 8 4 4 32 32 2 4 2 4 '_ '_))

;;;;;;;;;;;;;;;
;; Problem 3
;;;;;;;;;;;;;;;
; slide-row-right : [ListOf TailValue] -> [ListOf TailValue]
; (slide-row-right row) returns a list which rows to right. If the
; numbers are the same, it will returns the accumulation of them,
; if not, it will stay as it used to be.
(define (slide-row-right row)
  (local [(define (srr-helper row n spaces)
            (cond
              [(empty? row)
               (local
                 [(define (slide-help l ans space)
                    (cond
                      [(empty? l) (append space ans)]
                      [(empty? (rest l))
                       (append space (cons (first l) ans))]
                      [(= (first l) (second l))
                       (slide-help (rest (rest l))
                              (cons (* 2 (first l)) ans)
                              (cons BLANK space))]
                      [else (slide-help (rest l)
                                   (cons (first l) ans) space)]))]
                 (append spaces (slide-help n '() '())))]
              [(blank? (first row))
               (srr-helper (rest row) n (cons BLANK spaces))]
              [else (srr-helper (rest row)
                                (cons (first row) n) spaces)]))]
    (srr-helper row '() '())))

(check-expect (slide-row-left '()) '())
(check-expect (slide-row-left '(_)) (list '_))
(check-expect (slide-row-left '(2)) (list 2))
(check-expect (slide-row-left '(8)) (list 8))
(check-expect (slide-row-left '(_ _)) (list '_ '_))
(check-expect (slide-row-right '(_ _ 2 2 _ 2 4 _ _ 4))
              (list '_ '_ '_ '_ '_ '_ '_ 2 4 8))

;;;;;;;;;;;;;;;
;; Problem 4
;;;;;;;;;;;;;;;
; slide-left : Board -> Board
; (slide-right bd) returns it after sliding all rows in the
; indicated direction.
(define (slide-left bd)
  (map slide-row-left bd))

(check-expect (slide-left b1) (list (list 64 32) (list 32 '_)))
(check-expect (slide-left b2) (list (list 4 4 '_ '_)
                                    (list 8 '_ '_ '_)
                                    (list 16 8 '_ '_)
                                    (list 32 '_ '_ '_)))
 
(define (slide-right bd)
  (map slide-row-right bd))

(check-expect (slide-right b1) (list (list 64 32) (list '_ 32)))
(check-expect (slide-right b2) (list (list '_ '_ 4 4 )
                                     (list '_ '_ '_ 8)
                                     (list '_ '_ 8 16)
                                     (list '_ '_ '_ 32)))

;;;;;;;;;;;;;;;
;; Problem 5
;;;;;;;;;;;;;;;
; transpose : Board ->  Board
; (transpose bd) returns the result of reflecting the elements
; along the main diagonal as shown below and in this gif.
(define (transpose bd)
  (cond
    [(empty? (first bd)) '()]
    [else (cons (map first bd)
                (transpose (map rest bd)))]))

(check-expect (transpose '(())) '())
(check-expect (transpose '((2))) (list (list 2)))
(check-expect (transpose '((2 4 8) (16 32 64) (128 256 512)))
              (list (list '2 '16 '128)
                    (list '4 '32 '256)
                    (list '8 '64 '512)))
(check-expect (equal? (transpose (transpose b1)) b1) #true)
(check-expect (equal? (transpose (transpose b2)) b2) #true)

;;;;;;;;;;;;;;;
;; Problem 6
;;;;;;;;;;;;;;;
; slide-up : Board -> Board
; (slide-up bd) returns it after sliding all rows in the indicated
; direction.
(define (slide-up bd) (transpose (slide-left (transpose bd))))

(check-expect (slide-up (slide-right b1))
              (list (list 64 64)
                    (list '_ '_)))
(check-expect (slide-up b2)
              (list (list 2 2 2 2 )
                    (list 4 8 4 8)
                    (list 16 '_ 8 16)
                    (list '_ '_ '_ '_)))

; slide-down : Board -> Board
; (slide-down bd) returns it after sliding all rows in the
; indicated direction.
(define (slide-down bd) (transpose (slide-right (transpose bd))))

(check-expect (slide-down (slide-right b1)) (list (list '_ '_)
                                                  (list 64 64)))
(check-expect (slide-down b2) (list (list '_ '_ '_ '_ )
                                    (list 2 '_ 2 2)
                                    (list 4 2 4 8)
                                    (list 16 8 8 16)))

;;;;;;;;;;;;;;;
;; Problem 7
;;;;;;;;;;;;;;;
;; 7a
; game-lost? : Board -> Bool
; (game-lost? bd) returns #true if it represents a losing
; configuration, and #false otherwise.
(define (game-lost? bd)
  (and
   (equal? bd (slide-right bd)) (equal? bd (slide-left bd))
   (equal? bd (slide-up bd)) (equal? bd (slide-down bd))))

(check-expect (game-lost? '((2))) #true)
(check-expect (game-lost? '((2048))) #true)
(check-expect (game-lost? '((2 4) (8 4))) #false)
(check-expect (game-lost? '((2 4 8) (16 8 4) (8 4 2))) #true)
(check-expect (game-lost? b1) #false)

;; 7b
; game-over? : Board -> Bool
; (game-over? bd) returns #true if it represents either a winning
; or a losing configuration, and #false otherwise.
(define (game-over? bd)
  (or (game-won? bd) (game-lost? bd)))

(check-expect (game-over? '((1024))) #true)
(check-expect (game-over? '((2048 4) (8 4))) #true)
(check-expect (game-over? '((2 4 8) (16 8 4) (8 4 2))) #true)

;;;;;;;;;;;;;;;
;; Problem 8
;;;;;;;;;;;;;;;
; key-handler : Board X -> Board
; (key-handler bd word) returns the board that results from
; processing the keyboard input.
(define (key-handler bd key)
  (local
    [(define (helper bd)
       (cond 
         [(equal? key "right") (slide-right bd)]
         [(equal? key "left") (slide-left bd)]
         [(equal? key "up") (slide-up bd)]
         [(equal? key "down") (slide-down bd)]
         [else bd]))
     (define ans (helper bd))]
    (if (equal? bd ans)
        bd
        (add-new-tile ans))))

(define b4 (key-handler b1 "right"))
(define b5 (key-handler b4 "up"))
(define b6 (key-handler b5 "a"))

; (map board->image (list b1 b4 b5 b6))

(define b7 (key-handler b2 "left"))
(define b8 (key-handler b7 "down"))
; (map board->image (list b2 b7 b8))

(define b9 '((2 4) (_ _)))
(check-expect (key-handler b9 "up") (list (list 2 4) (list '_ '_)))
(check-expect (key-handler b9 "left") (list (list 2 4) (list '_ '_)))
(check-expect (key-handler b9 "right") (list (list 2 4) (list '_ '_)))

;;;;;;;;;;;;;;;
;; Problem 9
;;;;;;;;;;;;;;;
; play : Nat -> [ListOf Board]
; (play n) returns 2048 game.
(define (play n)
  (big-bang (add-new-tile
             (add-new-tile
              (make-list n (make-list n '_))))
           [to-draw board->image]
           [on-key key-handler]
           [stop-when game-over?]
           [name "2048"]))
