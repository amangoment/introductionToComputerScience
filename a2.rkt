;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname a2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;
;; Problem 1
;;;;;;;;;;;;;;;
;;;;; 1a
; f : Num Image Bool -> String
(define (f x img b) "rubbish")

;;;;; 1b
; nonsense : Nat String Bool -> String
(define (nonsense x string b) "lol")
;;;;; 1c
; palindrome? : String -> Bool
(define (palindrome? string) #true)

;;;;; 1d
; image-straighten : Num Image -> Image
(define (image-straighten x img) img)

;;;;; 1e
; exclusive-or : Bool Bool -> Bool
(define (exclusive-or boo bool) #false)

;;;;; 1f
; date->day-of-week : String Int Int -> String
(define (date->day-of-week str int num) "new")

;;;;;;;;;;;;;;;
;; Problem 2
;;;;;;;;;;;;;;;
;;;;; 2a
; number-square : Num -> Num
; (number-square x) produces the result of squaring x

(define (number-square x)
  (expt x 2))

(check-expect (number-square 8) 64)
(check-within (number-square -2.3) 5.3 .1)

;;;;; 2b
; euclidean-distance : Num Num Num Num-> Nat
; (euclidean-distance x1 y1 x2 y2) produces the result of distance
; between two points
(define (euclidean-distance x1 y1 x2 y2)
  (sqrt (+ (number-square (- x1 x2)) (number-square (- y1 y2)))))

(check-expect (euclidean-distance 5 4 5 2) 2)
(check-within (euclidean-distance 9 5 2 4) 8 1)
(check-within (euclidean-distance 2 2 1 1) 2 1)
(check-within (euclidean-distance 5 4 5 2) 2 1)
(check-satisfied (euclidean-distance 5 4 5 2) number?)

;;;;; 2c
; circle-area : Num -> Num
; (circle-area radius) produces the area of a circle
(define (circle-area radius)
  (round (* pi (number-square radius))))

(check-within (circle-area 1)4 1)

;;;;; 2d
; image-area : Img -> Nat
; (image-area pic) produces the area of the picture
(define (image-area pic)
  (* (image-width pic) (image-height pic)))

(check-expect (image-area (square 20 "solid" "red")) 400)
(check-within (image-area (circle 2 "solid" "red")) 17 1)

;;;;; 2e
; make-name : String String String -> String
; (make-name first middle-initial last) produces another writing
; type of name
(define (make-name first middle-initial last)
  (string-append last ", " first " " middle-initial "."))

(check-expect (make-name "alex" "d" "chen") "chen, alex d.")
(check-expect (make-name  "jane" "a" "ci") "ci, jane a.")

;;;;;;;;;;;;;;;
;; Problem 3
;;;;;;;;;;;;;;;
; number-crop Num Num Num -> Num
; (number-crop x a b) : produces the number of x by depending its
; position
(define (number-crop x a b)
  (min b (max x a)))

(check-expect (number-crop 2 1 3) 2)
(check-expect (number-crop -4 -2 -1) -2)

;;;;;;;;;;;;;;;
;; Problem 4
;;;;;;;;;;;;;;;
; disjunction String String -> String
; (disjunction n1 n2) : produces the selection between n1 and n2
(define (disjunction n1 n2)
  (string-append n1 " or " n2))

(check-expect (disjunction "paper" "plastic") "paper or plastic")
(check-satisfied (disjunction "paper" "hair") string?)

;;;;;;;;;;;;;;;
;; Problem 5
;;;;;;;;;;;;;;;
;;;;; 3a
; string-left : String -> String
; (string-left n3) : produces the leftmost half of the string

(define (string-left n3)
  (substring n3 0 (quotient (string-length n3) 2)))

(check-expect (string-left "thanksgiving") "thanks")
(check-expect (string-left "pretty") "pre")

;;;;; 5b
; string-right String -> String
; (string-right n4) : produces the rightmost half of the string

(define (string-right n4)
  (substring n4 (quotient (+ 1(string-length n4)) 2)))

(check-expect (string-right "thanksgiving") "giving")
(check-satisfied (string-right "thanksgiving") string?)

;;;;; 5c
; string-middle String -> String
; (string-middle n5) : produces the middle character of the string
(define (string-middle n5)
  (substring n5 (quotient (string-length n5)2)
             (quotient (+ 1(string-length n5))2)))

(check-expect (string-middle "red") "e")
(check-satisfied (string-middle "hesilings") string?)

;;;;;;;;;;;;;;;
;; Problem 6
;;;;;;;;;;;;;;;
; roll : Img -> Img
; (roll imm) : produces the image rotating clockwise 90 degrees

(define (roll img)
  (beside img
          (rotate 90 img)
          (rotate 180 img)
          (rotate 270 img)))

;;;;;;;;;;;;;;;
;; Problem 7
;;;;;;;;;;;;;;;
;; 7a
; horizontal-line : Num -> Img
; (horizontal-line n1) : produces a horizontal line which longs x.
(define (horizontal-line n1)
  (line (- n1 1) 0 "black"))

;; 7b
; vertical-line : Num -> Img
; (vertical-line n2) produces a vertical line.
(define (vertical-line n2)
  (line 0 (- n2 1) "black"))

;;;;;;;;;;;;;;;
;; Problem 8
;;;;;;;;;;;;;;;
;; 8a
; add-left-border  : Img -> Img
; (add-left-border img) produeces a image attaching a
; one-pixel black border to the left side
(define (add-left-border img)
  (beside (line 1 (image-height img) "black") img))

;; 8b
; add-right-border : Img -> Img
; (add-left-border img) produeces a image attaching a one-pixel
; black border to the right side
(define (add-right-border img)
  (beside img (line 1 (image-height img) "black")))

;; 8c
; add-top-border : Img -> Img
; (add-top-border tu) : produeces a image having a one-pixel black
;border above it
(define (add-top-border tu)
  (above (line (image-width tu) 1 "black") tu))

;; 8d
; add-bottom-border : Img -> Img
; (add-bottom-border img) : produeces a image having a one-pixel
;black border under it
(define (add-bottom-border img
  (above img (line (image-width img) 1 "black")))

;;;;;;;;;;;;;;;
;; Problem 9
;;;;;;;;;;;;;;;
; add-frame : Img -> Img
; (add-frame img) : makes one-pixel black border on all four
; sides of the image
(define (add-frame img)
  (above (line (image-width img) 1 "black")
         (beside (line 1 (image-height img) "black")
                 img (line 1 (image-height img) "black"))
         (line (image-height img) 1 "black")))

;;;;;;;;;;;;;;;
;; Problem 10
;;;;;;;;;;;;;;;
; tetris-t-piece : Num String -> Img
; (tetris-t-piece length color)  produces a T-shaped tetris piece
(define (tetris-t-piece length color)
  (above
   (beside(add-frame (square length "solid" color))
          (above
           (line length 1 "black")
           (square length "solid" color))
          (add-frame (square length "solid" color)))
   (add-frame (square length "solid" color))))

(tetris-t-piece 30 "green")
(tetris-t-piece 50 "sky blue")
(check-expect(image-width(tetris-t-piece 50 "sky blue")) 159)
(check-expect(image-height(tetris-t-piece 50 "sky blue")) 110)

;;;;;;;;;;;;;;;
;; Problem 11
;;;;;;;;;;;;;;;
; draw-piece : Img Num Img -> Img
; (draw-piece imgg numm img) produces a moving piece in Tetris
(define (draw-piece img1 num img2)
  (place-image/align
   img1
   (/ (image-width img2) 2)
   num "center" "top"
   img2))

(draw-piece (tetris-t-piece 50 "sky blue") 0 (empty-scene 300 400))

;;;;;;;;;;;;;;;
;; Problem 12
;;;;;;;;;;;;;;;
; tetris-frame : Nat Img -> Img
; (tetris-frame time) shows the movement of the piece in Tetris
(define (tetris-frame timee)
  (place-image/align
   (tetris-t-piece 50 "magenta")150 timee "center" "top"
   (empty-scene 300 400)))

; (animate tetris-frame)