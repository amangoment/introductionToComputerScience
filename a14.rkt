;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a14) (read-case-sensitive #t) (teachpacks ((lib "convert.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.rkt" "teachpack" "htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define handin "a14")
(define collaboration-statement "I worked alone")

;;;;;;;;;;;;;;;
;; Problem 1
;;;;;;;;;;;;;;;
#|
  A Byte is a Nat in the range [0..255]

  A Color is a (make-color Byte Byte Byte)
|#

;; create some standard colors
(define white (make-color 255 255 255))
(define black (make-color 0 0 0))
(define red (make-color 255 0 0))
(define green (make-color 0 255 0))
(define blue (make-color 0 0 255))
(define yellow (make-color 255 255 0))
(define purple (make-color 160 32 240))
(define orange (make-color 255 165 0))

;(define tiny-dino (bitmap "tiny-dino.png"))

; image-recolor : Image Color Color -> Image
; (image-recolor image old-color new-color) returns the image that
; results from replacing all occurrences of old-color with
; new-color in image
(define (image-recolor image old-color new-color)
  (color-list->bitmap
   (map
    ; Color -> Color
    (lambda (color)
      (if (equal? color old-color)
               new-color
               color))
    (image->color-list image))
   (image-width image)
   (image-height image)))

#|
(define neon-dino
  (image-recolor (image-recolor tiny-dino black green)
                 white purple))

(check-expect (image-recolor (square 4 'solid 'red) red green)
              (square 4 'solid 'green))
(check-expect (first (image->color-list
                      (image-recolor tiny-dino white blue)))
              blue)
(check-expect (list-ref (image->color-list
                         (image-recolor tiny-dino black green))
                        3360)
              green)
|#

;; 1a
; image-map : [Color -> Color] Image -> Image
; (image-map f img) returns the image that results from applying the
; given function on each pixel in the given image.
(define (image-map f img)
  (color-list->bitmap
   (map f (image->color-list img))
   (image-width img)
   (image-height img)))

#|
(define prince (bitmap "prince.png"))
(check-satisfied (image-map (lambda (color)
                              (if (equal? color (make-color 0 0 0))
                                  (make-color 160 32 240)
                                  color))
                            prince) image?)
(check-satisfied (list prince
                       (image-map
                        (lambda (color)
                          (if (equal? color (make-color 0 0 0))
                              (make-color 160 32 240)
                              color))
                        prince)) cons?)
|#

;; 1b
; image-negative : Image -> Image
; (image-negative img) returns an image that inverts each
; color value (red, green, and blue) by subtracting it from 255.
(define (image-negative img)
  (image-map
   ; Color -> Color
   (lambda (color)
     (make-color (- 255 (color-red color))
                 (- 255 (color-green color))
                 (- 255 (color-blue color))))
   img))

#|
(define marilyn (bitmap "marilyn.png"))
(check-satisfied (list marilyn (image-negative marilyn)) cons?)

(define baboon (bitmap "baboon.png"))
(check-satisfied (list baboon (image-negative baboon)) cons?)

(check-satisfied (list tiny-dino (image-negative tiny-dino)) cons?)
|#

;; 1c
; image-spin : Image -> Image 
; (image-spin img) returns an image that rotates the color values
; (red, green, and blue) in each pixel.
(define (image-spin img)
  (image-map
   ; Color -> Color
   (lambda (color)
     (make-color (color-blue color)
                 (color-red color)
                 (color-green color)))
   img))

#|
(check-satisfied (image-spin marilyn) image?)
(check-satisfied (list marilyn (image-spin marilyn)) cons?)
(check-satisfied (image-spin baboon) image?)
(check-satisfied (list baboon (image-spin baboon)) cons?)

(define soup (bitmap "soup.png"))
(check-satisfied (image-spin soup) image?)
(check-satisfied (list soup (image-spin soup)
                       (image-spin (image-spin soup))) cons?)
(check-satisfied ((compose image-spin image-negative) soup) image?)
|#

;;;;;;;;;;;;;;;
;; Problem 2
;;;;;;;;;;;;;;;
; warhol : Image Nat -> Image
; (warhol img n) returns the given name if n is zero. Otherwise, a new
; image of the same size is formed from four smaller images in a 2x2
; grid pattern.
(define (warhol img n)
  (cond
    [(zero? n) img]
    [else (above (beside (scale 1/2 img)
                         (warhol (scale 1/2 img) (sub1 n)))
                 (beside (scale 1/2 img)
                         (scale 1/2 img)))]))

#|
(check-satisfied (warhol soup 0) image?)
(check-satisfied (warhol soup 1) image?)
(check-satisfied (warhol soup 2) image?)
(check-satisfied (warhol marilyn 3) image?)
|#

;;;;;;;;;;;;;;;
;; Problem 3
;;;;;;;;;;;;;;;
; interleave : [ListOf X] [ListOf X] -> [ListOf X]
; (interleave ls1 ls2) returns the result of meshing the two lists
; together by alternating items, starting with the first list. 
(define (interleave ls1 ls2)
  (local
    [(define (loop l1 l2 acc)
       (cond
         [(empty? l1) (append acc l2)]
         [(empty? l2) (append acc l1)]
         [else (loop (rest l1) (rest l2)
                     (append acc (list (first l1) (first l2))))]))]
    (loop ls1 ls2 '())))

(check-expect (interleave '() '()) '())
(check-expect (interleave '(X X X X X X) '(O O O O O))
              (list 'X 'O 'X 'O 'X 'O 'X 'O 'X 'O 'X))
(check-expect (interleave '(1 2 3) '(a b c d e f))
              (list 1 'a 2 'b 3 'c 'd 'e 'f))
(check-expect (interleave '(1 2 3 4 5 6) '(a b))
              (list 1 'a 2 'b 3 4 5 6))

;;;;;;;;;;;;;;;
;; Problem 4
;;;;;;;;;;;;;;;
; list-head : Nat [ListOf X] -> [ListOf X]
; (list-head n ls) returns a new list contains the first n elements
; in the old list.
(define (list-head init-n init-ls)
  (local
    [(define (list-head/help n ls)
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
  (local
    [(define (list-tail/help n ls)
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
  (local
    [(define (connect ls)
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

; image-interleave : Image Image -> Image
; (image-interleave img1 img2) returns the result of meshing the two
; images together by alternating rows, starting with the first image.
(define (image-interleave img1 img2)
  (color-list->bitmap
   (flatten
    (interleave (pop-up (image-width img1) (image->color-list img1))
                (pop-up (image-width img2) (image->color-list img2))))
   (image-width img2)
   (+ (image-height img2)
      (image-height img1))))

#|
(check-satisfied (image-interleave (rectangle 200 100 'solid 'red)
                                   (rectangle 200 100 'solid 'green))
                 image?)
(check-satisfied (image-interleave (bitmap "profile.png")
                                   (bitmap "thinker.png")) image?)
|#

;;;;;;;;;;;;;;;
;; Problem 5
;;;;;;;;;;;;;;;
; flip : [X X -> X] -> [X X -> X]
; (flip f) returns a function of two arguments and the returned
; function should work exactly the same as f except that the order of
; the arguments is reversed.
(define (flip f)
  (lambda (element1 element2)
    (f element2 element1)))

(check-expect ((flip -) 5 6) 1)
(check-expect ((flip quotient) 5 23) 4)
(check-expect ((flip cons) '(b c d e f g h) 'a)
              (list 'a 'b 'c 'd 'e 'f 'g 'h))
(check-expect ((flip (flip string-append)) "basket" "ball")
              "basketball")
(check-satisfied ((flip beside) (circle 25 'solid 'red)
                                (square 50 'solid 'yellow)) image?)

;;;;;;;;;;;;;;;
;; Problem 6
;;;;;;;;;;;;;;;
; unique-only : [ListOf X] -> [ListOf X]
; (unique-only ls) returns those elements that appear only once in the
; order in which they first appear in the given list.
(define (unique-only ls)
  (local
    [(define (helper ls element)
       (filter (lambda (x) (not (equal? x element))) ls))]
    (cond
      [(empty? ls) '()]
      [else (cons (first ls)
                  (unique-only (helper (rest ls) (first ls))))])))

(check-expect (unique-only '()) '())
(check-expect (unique-only '(4)) (list 4))
(check-expect (unique-only '(a a a b c b b c b a a a d a b c d d e))
              (list 'a 'b 'c 'd 'e))

;;;;;;;;;;;;;;;
;; Problem 7
;;;;;;;;;;;;;;;
; seeing-double : [ListOf X] -> Nat
; (seeing-double ls) returns the number of occurrences of exactly two
; consecutive identical elements and sequences longer than two are not
; included in the count.
(define (seeing-double ls)
  (local
    [(define (helper ls state last count)
       (cond
         [(empty? ls)
          (if (= state 2)
              (add1 count)
              count)]
         [(equal? (first ls) last)
          (helper (rest ls) (add1 state) last count)]
         [(= state 2)
          (helper (rest ls) 1 (first ls) (add1 count))]
         [else (helper (rest ls) 1 (first ls) count)]))]
    (if (empty? ls)
        0
        (helper (rest ls) 1 (first ls) 0))))

(check-expect (seeing-double '()) 0)
(check-expect (seeing-double '(a b c d e f)) 0)
(check-expect (seeing-double '(a a b b c)) 2)
(check-expect (seeing-double '(a a b b c c)) 3)
(check-expect (seeing-double '(a a b b c c c)) 2)
(check-expect (seeing-double '(a a)) 1)
(check-expect (seeing-double '(a a a)) 0)
(check-expect (seeing-double '(a a a a)) 0)
(check-expect (seeing-double '(a a a b b a)) 1)
(check-expect (seeing-double '(a a b a a)) 2)
(check-expect (seeing-double '(a a b b c c d d e e f f g g h h)) 8)
(check-expect (seeing-double '(a a a b a a a b b b c c c c a b c)) 0)
(check-expect (seeing-double '(a b c d e f g)) 0)

;;;;;;;;;;;;;;;
;; Problem 8
;;;;;;;;;;;;;;;
; divide : [x -> Bool] [ListOf X] -> [ListOf [ListOf X]]
; (divide pred? ls) returns a list with two sublists: those elements in
; ls that satisfy the predicate and those that don’t.
(define (divide pred? ls)
  (local
    [(define (helper ls)
       (cond
         [(empty? ls) (list '() '())]
         [else (local
                 [(define ans (helper (rest ls)))]
                 (cond
                   [(pred? (first ls))
                    (list (cons (first ls)
                                (first ans))
                          (second ans))]
                   [else (list (first ans)
                               (cons (first ls) (second ans)))]))]))]
    (helper ls)))

(check-expect (divide zero? '(1 1 0 1 0 1 0 0 1 1))
              (list (list 0 0 0 0) (list 1 1 1 1 1 1)))
(check-expect (divide negative? '(9 3 -5 8 -7 2 9))
              (list (list -5 -7) (list 9 3 8 2 9)))
(check-expect (divide (lambda (x) (not (equal? x 'snow)))
                      '(snow rain hail snow snow sleet))
              (list (list 'rain 'hail 'sleet) (list 'snow 'snow 'snow)))

;;;;;;;;;;;;;;;
;; Problem 9
;;;;;;;;;;;;;;;
#|
  A [TreeOf X] is one of
  - (make-empty-tree)
  - (make-node X [TreeOf X] [TreeOf X])
|#

(define-struct empty-tree [])
(define-struct node [data left right])

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

; make-leaf : X -> [TreeOf X]
; (make-leaf item) returns a tree with exactly one node, and that node
; contains the given item in its data field.
(define (make-leaf item)
  (make-node item et et))

(check-expect (make-leaf "green")
              (make-node "green" (make-empty-tree) (make-empty-tree)))

; leaf? : X -> Bool
; (leaf? item) returns #true if it is a leaf node, and #false otherwise.
(define (leaf? item)
  (if
   (equal? item et)
   #false
   (and (empty-tree? (node-left item))
        (empty-tree? (node-right item)))))

(check-expect (leaf? (make-leaf "green")) #true)
(check-expect (leaf? (node-left tr4)) #false)
(check-expect (leaf? (node-left (node-left tr4))) #true)
(check-expect (leaf? (node-right (node-left tr4))) #false)

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
(check-expect (height (make-node 8  (make-leaf 2)
                                 (make-empty-tree))) 2)
(check-expect (height tr4) 3)
(check-expect (height tr5) 3)
(check-expect (height tr6) 4)
(check-expect (height (make-node 0 (make-empty-tree) tr6)) 5)
(check-expect (height (make-node 0 tr6 (make-empty-tree))) 5)

; longest-path-help : [TreeOf X] -> [ListOf X]
; (longest-path-help tr) returns a list containing the height of the
; tree, followed by the data along the longest path.
(define (longest-path-help tr)
  (cond
    [(empty-tree? tr) (list 0)]
    [else
     (local
       [(define ansl (longest-path-help (node-left tr)))
        (define ansr (longest-path-help (node-right tr)))]
       (if (>= (first ansl) (first ansr))
           (cons (add1 (first ansl))
                 (cons (node-data tr) (rest ansl)))
           (cons (add1 (first ansr))
                 (cons (node-data tr) (rest ansr)))))]))

(check-expect (longest-path-help (make-empty-tree)) (list 0))
(check-expect (longest-path-help (make-leaf 'a)) (list 1 'a))
(check-expect (longest-path-help (make-node 'a (make-leaf 'b)
                                            (make-leaf 'c)))
              (list 2 'a 'b))
(define tr (make-node 'a (make-leaf 'b)
                      (make-node 'c (make-empty-tree) (make-leaf 'd))))
(check-expect (longest-path-help tr) (list 3 'a 'c 'd))
(check-expect (longest-path-help
               (make-node 'x tr
                          (make-node 'y (make-empty-tree) tr)))
              (list 5 'x 'y 'a 'c 'd))

; longest-path : [TreeOf X] -> [ListOf X]
; (longest-path tr) returns a list consisting of the data in the tree
; along the longest root to leaf path.
(define (longest-path tr)
  (rest (longest-path-help tr)))

(check-expect (longest-path (make-empty-tree)) '())
(check-expect (longest-path (make-leaf 'a)) (list 'a))
(check-expect (longest-path (make-node 'a (make-leaf 'b)
                                       (make-leaf 'c))) (list 'a 'b))
(check-expect (longest-path
               (make-node 'a (make-leaf 'b)
                          (make-node 'c (make-empty-tree)
                                     (make-leaf 'd))))
              (list 'a 'c 'd))

;;;;;;;;;;;;;;;
;; Problem 10
;;;;;;;;;;;;;;;
;; 10a
; get-index : Nat Nat Nat -> Nat
; (get-index x y width) returns the index of the pixel in the
; corresponding flat list of colors.
(define (get-index x y width)
  (+ (* y width) x))

(check-expect (get-index 0 0 1000) 0)
(check-expect (get-index 0 20 100) 2000)
(check-expect(get-index 20 0 100) 20)
(check-expect (get-index 23 55 30) 1673)
(check-expect (get-index 23 55 50) 2773)

;; 10b
; out-of-bounds? : Nat Nat Nat Nat -> Bool
; (out-of-bounds? x y width height) returns #true iff the coordinate is
; outside the perimeter of the image.
(define (out-of-bounds? x y width height)
  (not (and (>= x 0)
            (>= y 0)
            (< x width)
            (< y height))))

(check-expect (out-of-bounds? -1 5 10 20) #t)
(check-expect (out-of-bounds? 1 -5 10 20) #t)
(check-expect (out-of-bounds? 10 4 10 20) #t)
(check-expect (out-of-bounds? 3 30 10 30) #t)
(check-expect (out-of-bounds? 3 35 10 30) #t)
#|
;; 10c
; flood-fill : Image Nat Nat Color -> Image
; (flood-fill img x y new-color) return the result of filling the
; region accessible from the coordinate with the color.
(define (flood-fill img x y new-color)
  (local
    [(define color-list (image->color-list img))
     (define width (image-width img))
     (define height (image-height img))
     (define position (get-index x y width))
     ; find-color : Nat [ListOf Color] -> Color
     (define (find-color n ls)
       (cond
         [(zero? n) (first ls)]
         [else (find-color (sub1 n) (rest ls))]))
     (define old-color (find-color position color-list))
     ; change : Nat [ListOf Color] -> [ListOf Color]
     (define (change n ls)
       (cond
         [(zero? n) (cons new-color (rest ls))]
         [else (cons (first ls) (change (sub1 n) (rest ls)))]))
     ; fill : Nat Nat [ListOf Color] -> [ListOf Color]
     (define (fill x y ls)
       (cond
         [(out-of-bounds? x y width height) ls]
         [(equal? (find-color (get-index x y width) ls)
                  (make-color 0 0 0))
          ls]
         [(not
           (equal? (find-color (get-index x y width) ls) old-color))
          ls]
         [(equal? (find-color (get-index x y width) ls) new-color) ls]
         [else (fill (sub1 x)
                     y
                     (fill (add1 x)
                           y
                           (fill x
                                 (sub1 y)
                                 (fill x
                                       (add1 y)
                                       (change (get-index x y width)
                                               ls)))))]))]
    (color-list->bitmap
     (fill x y color-list)
     width
     height)))

(check-satisfied (flood-fill (triangle 100 'outline 'black) 10 10
                             (make-color 255 255 0)) image?)
(check-satisfied (flood-fill (triangle 100 'outline 'black) 50 50
                             (make-color 255 255 0)) image?)

(define d1 (flood-fill tiny-dino 133 16 (make-color 135 206 235)))
(define d2 (flood-fill d1 25 119 (make-color 34 139 34)))
(define d3 (flood-fill d2 123 77 (make-color 210 180 140)))
(check-satisfied (list d1 d2 d3) cons?)

(define d4 (flood-fill d3 46 69 (make-color 210 78 152)))
(define d5 (flood-fill d4 64 54 (make-color 67 119 201)))
(define d6 (flood-fill d5 37 42 (make-color 255 20 30)))
(check-satisfied (list d4 d5 d6) cons?)

(define d7 (flood-fill d6 47 35 (make-color 255 165 0)))
(define d8 (flood-fill d7 89 107 (make-color 99 10 200)))
(define d9 (flood-fill d8 133 117 (make-color 250 128 114)))
(check-satisfied (list d7 d8 d9) cons?)
|#

;; 10d
; draw-image : Image -> Image
; (draw-image image) returns a image which can be used in paint.
(define (draw-image image) image)

; paint : Image -> Image ; World -> World
; (paint img) returns a image which can be used to draw image by
; pressing mouse.
(define (paint image)
  (big-bang image
            [to-draw draw-image]
            [on-mouse (lambda (image x y mouse-event)
                        (if (equal? mouse-event "button-down")
                            (flood-fill image
                                        x
                                        y
                                        (make-color (random 256)
                                                    (random 256)
                                                    (random 256)))
                            image))]
            [name "Paint"]))
