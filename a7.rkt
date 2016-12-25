;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname a7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; logical constants
(define WIDTH 7)    ; the number of blocks horizontally
(define HEIGHT 10)  ; the number of blocks vertically
 
; graphical constants
(define BLOCK-SIZE 50)  ; size of a rendered block (blocks are
; square)
(define SCENE-WIDTH (* WIDTH BLOCK-SIZE))
; scaled width of scene
(define SCENE-HEIGHT (* HEIGHT BLOCK-SIZE))
; scaled height of scene
 
; pool of possible block colors
(define POOL
  (list "red" "orange" "yellow" "green" "blue" "indigo" "violet"))
 
#|
   A Hue is a Color in POOL
 
   A Block is a (make-block [1..HEIGHT] [1..WIDTH] Hue)
 
   A Landscape is one of:
   – '()
   – (cons Block Landscape)
 
   A World is a (make-world Block Landscape)
|#
 
(define-struct block [row col color])
(define-struct world [piece land])

;;;;;;;;;;;;;;;
;; Problem 1
;;;;;;;;;;;;;;;
(define w0
  (make-world (make-block 1 4 "indigo")
              (list (make-block (sub1 HEIGHT) 2 "red")
                    (make-block HEIGHT 1 "green")
                    (make-block HEIGHT 2 "blue")
                    (make-block HEIGHT WIDTH "yellow"))))

(define w1
  (make-world (make-block 3 6 "red")
              (list (make-block 6 4 "green")
                    (make-block 7 4 "blue")
                    (make-block 8 3 "orange")
                    (make-block 8 4 "green")
                    (make-block 9 3 "violet")
                    (make-block 9 4 "yellow")
                    (make-block 9 7 "indigo")
                    (make-block 10 1 "red")
                    (make-block 10 2 "red")
                    (make-block 10 3 "red")
                    (make-block 10 4 "red")
                    (make-block 10 5 "red")
                    (make-block 10 7 "red"))))

(define w2
  (make-world (make-block 1 4 "green")
              (list (make-block 2 4 "yellow")
                    (make-block 3 4 "indigo")
                    (make-block 4 4 "orange")
                    (make-block 5 4 "indigo")
                    (make-block 6 3 "yellow")
                    (make-block 6 4 "indigo")
                    (make-block 7 1 "blue")
                    (make-block 7 3 "green")
                    (make-block 7 4 "green")
                    (make-block 8 1 "violet")
                    (make-block 8 3 "indigo")
                    (make-block 8 4 "blue")
                    (make-block 8 7 "yellow")
                    (make-block 9 1 "orange")
                    (make-block 9 2 "red")
                    (make-block 9 3 "indigo")
                    (make-block 9 4 "green")
                    (make-block 9 5 "indigo")
                    (make-block 9 7 "green")
                    (make-block 10 1 "yellow")
                    (make-block 10 2 "blue")
                    (make-block 10 3 "yellow")
                    (make-block 10 4 "violet")
                    (make-block 10 5 "red")
                    (make-block 10 6 "yellow")
                    (make-block 10 7 "red"))))

(check-satisfied (block-color (world-piece w2)) string?)
(check-expect (block-color (world-piece w2)) "green")
(check-satisfied (length (world-land w2)) number?)
(check-expect (length (world-land w2)) 26)

;;;;;;;;;;;;;;;
;; Problem 2
;;;;;;;;;;;;;;;
;; 2a
#|
 A Speed is one of
- 'fast
- 'medium
- 'slow
|#

;; 2b
; speed->rate : Speed -> Int
; (speed->rate s) returns the corresponding frame rate as a number
; in the interval (0..1].
(define (speed->rate s)
  (cond
    [(equal? s 'fast) .2]
    [(equal? s 'medium) .6]
    [(equal? s 'slow) 1]
    [else 'unreachable]))

(check-within (speed->rate 'fast) .2 .1)
(check-within (speed->rate 'medium) .6 .1)
(check-within (speed->rate 'slow) 1 .1)

;;;;;;;;;;;;;;;
;; Problem 3
;;;;;;;;;;;;;;;
;; 3a
; block->image : Block -> Image
; (block->image b) returns a solid square of dimension BLOCK-SIZE
; filled in with the block’s color and surrounded by a black
; outline.
(define (block->image bl)
  (overlay
   (square (- BLOCK-SIZE 1) "outline" "black")
   (square BLOCK-SIZE "solid" (block-color bl))))

(check-satisfied (block->image (make-block 1 2 "violet")) image?)
(check-satisfied (block->image (make-block 8 3 "yellow")) image?)
(check-expect (- (image-height (block->image (make-block 8 3 "yellow")))
                 BLOCK-SIZE) 0)
(check-expect (- (image-width (block->image (make-block 8 3 "yellow")))
                 BLOCK-SIZE) 0)

;; 3b
; place-block : Block Image -> Scene
; (place-block block scene) returns the result of placing the
; block on the scene at the block’s grid coordinates.
(define (place-block block scene)
  (place-image/align
   (block->image block)
   (* (block-col block) BLOCK-SIZE)
   (* (block-row block) BLOCK-SIZE)
   "right"
   "bottom"
   scene))

(check-satisfied (place-block
                  (make-block 1 2 "blue")
                  (empty-scene SCENE-WIDTH SCENE-HEIGHT)) image?)
(check-satisfied (place-block (make-block 1 2 "blue")
    (place-block (make-block 8 3 "yellow")
      (place-block (make-block 9 3 "indigo")
        (place-block (make-block 10 3 "red")
          (place-block (make-block 10 6 "yellow")
            (place-block (make-block 10 4 "orange")
              (empty-scene SCENE-WIDTH SCENE-HEIGHT)))))))
                 image?)
                                           

;; 3c
; world->image : World -> Image
; (world->image w) returns the corresponding scene.
(define (world->image w)
  (cond
    [(empty? (world-land w)) (place-block (world-piece w)
                (empty-scene SCENE-WIDTH SCENE-HEIGHT))]
    [else (place-block (world-piece w)
                    (world->image (make-world
                    (first (world-land w))
                    (rest (world-land w)))))]))

(check-satisfied (world->image w0) image?)
(check-satisfied (world->image w1) image?)
(check-satisfied (world->image w2) image?)

;;;;;;;;;;;;;;;
;; Problem 4
;;;;;;;;;;;;;;;
;; 4a
#|
 A Direction is one of
- 'left
- 'right
- 'down
|#

;; 4b
; next-block : Block Direction -> Block
; (next-block block direct) returns the result of moving the
; block in the indicated direction.
(define (next-block block direct)
  (cond
    [(equal? direct "down")
     (if(< (block-row block) HEIGHT)
        (make-block  (+ (block-row block)1)
                     (block-col block)
                     (block-color block)) block)]
    [(equal? direct "left")
     (if (> (block-col block) 1)
         (make-block (block-row block)
                     (- (block-col block) 1)
                     (block-color block)) block)]
    [(equal? direct "right")
     (if (< (block-col block) WIDTH)
         (make-block (block-row block)
                     (add1 (block-col block))
                     (block-color block)) block)]
    [else block]))

(define b1 (make-block 5 4 "yellow"))
(check-expect (next-block b1 "down") (make-block 6 4 "yellow"))
(check-expect (next-block b1 "left") (make-block 5 3 "yellow"))
(check-expect (next-block b1 "right") (make-block 5 5 "yellow"))
(check-expect (next-block b1 "up") (make-block 5 4 "yellow"))

(define b2 (make-block 10 7 "red"))
(check-expect (next-block b2 "down") (make-block 10 7 "red"))
(check-expect (next-block b2 "right") (make-block 10 7 "red"))

(define b3 (make-block 5 1 "blue"))
(check-expect (next-block b3 "left") (make-block 5 1 "blue"))
(check-expect (next-block b3 "right") (make-block 5 2 "blue"))

;; 4c
; same-location? : Block Block -> Bool
; (same-location? block1 block2) returns #true iff they occupy
; the same grid location.
(define (same-location? b1 b2)
 (and (= (block-row b1) (block-row b2))
      (= (block-col b1) (block-col b2))))

(check-expect (same-location? (make-block 10 7 "red")
                              (make-block 10 7 "yellow")) #true)

;; 4d
; exists? : Block Land -> Bool
; (exists? block land) returns #true iff the block has the same
; location as some other block in the landscape, and #false
; otherwise.
(define (exists? block land)
  (cond
    [(empty? land) #false]
    [(same-location? block (first land)) #true]
    [else (exists? block (rest land))]))

(check-expect (exists? (make-block 6 4 "green") (world-land w1))
              #true)

;; 4e
; at-rest? : Block Land -> Bool
; (at-rest? block land) returns #true iff the block cannot move
; any further down.
(define (at-rest? block land)
  (or (equal? (block-row block) HEIGHT)
      (exists? (next-block block "down") land)))

(check-expect (at-rest? (make-block 3 2 "green")
                        (world-land w0)) #false)
(check-expect (at-rest? (make-block 9 3 "green")
                        (world-land w1)) #true)
(check-expect (at-rest? (make-block 10 3 "green")
                        (world-land w2)) #true)

;;;;;;;;;;;;;;;
;; Problem 5
;;;;;;;;;;;;;;;
;; 5a

#|
   A NonEmptyList is one of
   -(cons Any empty)
   -(cons Any NonEmptyList)
|#

;; 5b
; helper-pick : List Num -> Any
; (helper-pick ls num) returns the element in the num position.
(define (helper-pick ls num)
  (cond
    [(empty? ls)'()]
    [(= num 0) (first ls)]
    [else (helper-pick (rest ls) (- num 1))]))
(check-expect (helper-pick (list 1 2 3 4) 3) 4)

; pick-one : NonEmptyList -> Any
; (pick-one ls) returns an element of the list selected
; at random and with equal probability.
(define (pick-one ls)
  (helper-pick ls (random (length ls))))

(check-satisfied (pick-one '(me)) symbol?)
(check-satisfied (pick-one POOL) string?)
(check-satisfied (pick-one (range -8 24 3)) number?)

;; 5c
; make-piece : Nat -> Block
; (make-piece n) returns a Block, positioned in the center of the
; given row, with a randomly selected Hue.
(define (make-piece n)
  (make-block n (ceiling (/ WIDTH 2)) (pick-one POOL)))

(check-satisfied (make-piece 1) block?)
(check-satisfied (make-piece 2) block?)

;;;;;;;;;;;;;;;
;; Problem 6
;;;;;;;;;;;;;;;
;; 6a
; helper : Land -> Land
; (helper land) returns the position number in the world
; of the given land.
(define (helper land)
  (cond
    [(empty? land) 0]
    [(= (block-row (first land)) HEIGHT) (+ (helper (rest land)) 1)]
    [else (helper (rest land))]))

(check-expect (helper (cons (make-block 10 6 "green")
                                      (world-land w1))) 7) 

; bottom-row-full? : Land -> Bool
; (bottom-row-full? land) returns #true iff and only iff the bottom
; row is completely full of blocks.
(define (bottom-row-full? land)
  (= (helper land) WIDTH))

(check-expect (bottom-row-full? (world-land w0)) #false)
(check-expect (bottom-row-full? (world-land w1)) #false)
(check-expect (bottom-row-full? (world-land w2)) #true)
(check-expect (bottom-row-full? (cons (make-block 10 6 "green")
                                      (world-land w1))) #true)

;; 6b
; remove-bottom-row : Land -> Land
; (remove-bottom-row land) returns it with every block on the
; bottom row removed.
(define (remove-bottom-row land)
  (cond
    [(empty? land) '()]
    [(= (block-row (first land)) HEIGHT)
     (remove-bottom-row (rest land))]
    [else (cons (first land) (remove-bottom-row (rest land)))]))

(check-expect (remove-bottom-row (world-land w0))
              (list (make-block 9 2 "red")))
(check-expect (length (remove-bottom-row (world-land w1))) 7)
(check-expect (length (remove-bottom-row (world-land w2))) 19)
(check-satisfied (world->image
                  (make-world (world-piece w2)
                   (remove-bottom-row (world-land w2)))) image?)

;; 6c
; com-helper : Landscape -> Landscape
; (com-helper land) returns the whole landscape with all the blocks
; move down.
(define (com-helper land)
  (cond
    [(empty? land) '()]
    [else (cons (next-block (first land) "down")
          (com-helper (rest land)))]))

(check-expect (com-helper (world-land w1))
              (list
               (make-block 7 4 "green")
               (make-block 8 4 "blue")
               (make-block 9 3 "orange")
               (make-block 9 4 "green")
               (make-block 10 3 "violet")
               (make-block 10 4 "yellow")
               (make-block 10 7 "indigo")
               (make-block 10 1 "red")
               (make-block 10 2 "red")
               (make-block 10 3 "red")
               (make-block 10 4 "red")
               (make-block 10 5 "red")
               (make-block 10 7 "red")))

; compress : World -> World
; (compress w) returns it unchanged if a compression is not
; possible, or returns the world that removes the blocks on the
; bottom.
(define (compress w)
  (if
   (bottom-row-full? (world-land w))
   (make-world (world-piece w)
               (com-helper (remove-bottom-row (world-land w))))
   w))

(check-expect (length (world-land w1)) 13)
(check-expect (length (world-land (compress w1))) 13)
(check-satisfied (world->image (compress w1)) image?)
(check-expect (length (world-land w2)) 26)
(check-expect (length (world-land (compress w2))) 19)
(check-satisfied (world->image (compress w2)) image?)

;;;;;;;;;;;;;;;
;; Problem 7
;;;;;;;;;;;;;;;
;; 7a
; game-over? : World -> Bool
; (game-over? world) returns #true iff the piece is at rest in the
; top row.
(define (game-over? world)
  (and
   (at-rest? (world-piece world) (world-land world))
   (same-location? (world-piece world)
   (make-block 1 (ceiling (/ WIDTH 2)) "red"))))

(check-expect (game-over? w1) #false)
(check-expect (game-over? w0) #false)
(check-expect (game-over? w2) #true)

;; 7b
; tick-handler : World -> World 
; (tick-handler w) returns the World that results after one tick
; of the clock.
(define (tick-handler w)
  (if
   (at-rest? (world-piece w) (world-land w))
   (compress (make-world (make-piece 1) (cons (world-piece w)
                                              (world-land w)))) 
   (make-world (next-block (world-piece w) "down")
               (world-land w))))

(check-expect (tick-handler w1)
              (make-world
               (make-block 4 6 "red")
               (list
                (make-block 6 4 "green")
                (make-block 7 4 "blue")
                (make-block 8 3 "orange")
                (make-block 8 4 "green")
                (make-block 9 3 "violet")
                (make-block 9 4 "yellow")
                (make-block 9 7 "indigo")
                (make-block 10 1 "red")
                (make-block 10 2 "red")
                (make-block 10 3 "red")
                (make-block 10 4 "red")
                (make-block 10 5 "red")
                (make-block 10 7 "red"))))
(check-satisfied (tick-handler w2) world?)
(check-satisfied (tick-handler w1) world?)

;; 7c
; key-handler : World Direction -> World
; (key-handler w direct) returns the world that results from
; attempting to move the tetris piece in the indicated direction.
(define (key-handler w direct)
  (cond
    [(at-rest? (world-piece w) (world-land w))
     w]
    [(exists? (next-block (world-piece w) direct) (world-land w))
     w]
    [else (make-world (next-block (world-piece w) direct)
                                        (world-land w))]))
(check-satisfied (key-handler w2 "left") world?)

;; 7d
; tetris : Speed -> World
; (tetris speed) returns an animation of Simple Tetris.
(define (tetris speed)
  (big-bang (make-world (make-piece 1) '())
   [to-draw world->image]
   [on-tick tick-handler (speed->rate speed)]
   [on-key key-handler]
   [stop-when game-over?]
   [name "Simple Tetris"]))