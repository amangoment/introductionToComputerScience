;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname a8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define RADIUS 30)
(define SCENE-WIDTH 860)
(define SCENE-HEIGHT 680)

#|
   A PALETTE-COLORS is one of
   - "orange"
   - "red"
   - "green"
   - "violet"
   - "yellow"
   - "blue"
   - "pink"
   - "cyan"

   A Dot is a (make-dot Color Nat Nat)

   A World is one of
   - '()
   -  (cons Dot World)

   A KeyStroke is one of
   - "right"
   - "left"

|#

(define-struct dot [color size x y])
(define-struct world [color size ls hold])

(define HIGHLIGHT? #false)
(define SHADOW? #false)

(define CANVAS-WIDTH 860)
(define CANVAS-HEIGHT 680)

(define PALETTE-COLORS '(orange red green yellow violet blue
                                pink cyan))
(define PALETTE-SIZE (length PALETTE-COLORS))

(define BAR-HEIGHT 5)
(define DOT-RADIUS 20)

;; Example worlds
(define w0 (make-world "orange"
                       DOT-RADIUS
                       '()
                       '()))
(define w1 (make-world
            "yellow"
            DOT-RADIUS
            (list (make-dot "violet" 20 100 200)
                  (make-dot "yellow" 20 200 100))
            '()))

; palette
(define palette
  (overlay
   (beside
    (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
               "solid" (first PALETTE-COLORS))
    (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
               "solid" (second PALETTE-COLORS))
    (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
               "solid" (third PALETTE-COLORS))
    (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
               "solid" (fourth PALETTE-COLORS))
    (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
               "solid" (fifth PALETTE-COLORS))
    (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
               "solid" (sixth PALETTE-COLORS))
    (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
               "solid" (seventh PALETTE-COLORS))
    (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
               "solid" (eighth PALETTE-COLORS)))
   (rectangle SCENE-WIDTH (+ (/ SCENE-HEIGHT 8) BAR-HEIGHT)
              "solid" "white")))

#|
;highlight
(define kuang (rectangle (- (/ SCENE-WIDTH 8) BAR-HEIGHT)
                              (- (/ SCENE-HEIGHT 8) BAR-HEIGHT)
                              "outline" "white"))

(define (highlight world)
  (cond
    [(equal? (world-color world) (first PALETTE-COLORS))
     (overlay
      (beside
       (overlay kuang
                (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                           "solid" (first PALETTE-COLORS)))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (second PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (third PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (fourth PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (fifth PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (sixth PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (seventh PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (eighth PALETTE-COLORS)))
      (rectangle SCENE-WIDTH (+ (/ SCENE-HEIGHT 8) BAR-HEIGHT)
                 "solid" "white"))]
    [(equal? (world-color world) (second PALETTE-COLORS))
     (overlay
      (beside
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (first PALETTE-COLORS))
       (overlay kuang (rectangle (/ SCENE-WIDTH 8)
                                 (/ SCENE-HEIGHT 8)
                                 "solid" (second PALETTE-COLORS)))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (third PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (fourth PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (fifth PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (sixth PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (seventh PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (eighth PALETTE-COLORS)))
      (rectangle SCENE-WIDTH (+ (/ SCENE-HEIGHT 8) BAR-HEIGHT)
                 "solid" "white"))]
    [(equal? (world-color world) (third PALETTE-COLORS))
     (overlay
      (beside
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (first PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                                 "solid" (second PALETTE-COLORS))
       (overlay kuang (rectangle (/ SCENE-WIDTH 8)
                                 (/ SCENE-HEIGHT 8)
                  "solid" (third PALETTE-COLORS)))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (fourth PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (fifth PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (sixth PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (seventh PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (eighth PALETTE-COLORS)))
      (rectangle SCENE-WIDTH (+ (/ SCENE-HEIGHT 8) BAR-HEIGHT)
                 "solid" "white"))]
    [(equal? (world-color world) (fourth PALETTE-COLORS))
     (overlay
      (beside
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (first PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                                 "solid" (second PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (third PALETTE-COLORS))
       (overlay kuang (rectangle (/ SCENE-WIDTH 8)
                                 (/ SCENE-HEIGHT 8)
                  "solid" (fourth PALETTE-COLORS)))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (fifth PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (sixth PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (seventh PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (eighth PALETTE-COLORS)))
      (rectangle SCENE-WIDTH (+ (/ SCENE-HEIGHT 8) BAR-HEIGHT)
                 "solid" "white"))]
    [(equal? (world-color world) (fifth PALETTE-COLORS))
     (overlay
      (beside
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (first PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                                 "solid" (second PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (third PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (fourth PALETTE-COLORS))
       (overlay kuang (rectangle (/ SCENE-WIDTH 8)
                                 (/ SCENE-HEIGHT 8)
                  "solid" (fifth PALETTE-COLORS)))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (sixth PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (seventh PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (eighth PALETTE-COLORS)))
      (rectangle SCENE-WIDTH (+ (/ SCENE-HEIGHT 8) BAR-HEIGHT)
                 "solid" "white"))]
    [(equal? (world-color world) (sixth PALETTE-COLORS))
     (overlay
      (beside
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (first PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                                 "solid" (second PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (third PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (fourth PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (fifth PALETTE-COLORS))
       (overlay kuang (rectangle (/ SCENE-WIDTH 8)
                                 (/ SCENE-HEIGHT 8)
                  "solid" (sixth PALETTE-COLORS)))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (seventh PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (eighth PALETTE-COLORS)))
      (rectangle SCENE-WIDTH (+ (/ SCENE-HEIGHT 8) BAR-HEIGHT)
                 "solid" "white"))]
    [(equal? (world-color world) (seventh PALETTE-COLORS))
     (overlay
      (beside
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (first PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                                 "solid" (second PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (third PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (fourth PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (fifth PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (sixth PALETTE-COLORS)))
       (overlay kuang (rectangle (/ SCENE-WIDTH 8)
                                 (/ SCENE-HEIGHT 8)
                  "solid" (seventh PALETTE-COLORS)))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (eighth PALETTE-COLORS))
      (rectangle SCENE-WIDTH (+ (/ SCENE-HEIGHT 8) BAR-HEIGHT)
                 "solid" "white"))]
    [(equal? (world-color world) (eighth PALETTE-COLORS))
     (overlay
      (beside
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (first PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                                 "solid" (second PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (third PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (fourth PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (fifth PALETTE-COLORS))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (sixth PALETTE-COLORS)))
       (rectangle (/ SCENE-WIDTH 8) (/ SCENE-HEIGHT 8)
                  "solid" (seventh PALETTE-COLORS))
       (overlay kuang (rectangle (/ SCENE-WIDTH 8)
                                 (/ SCENE-HEIGHT 8)
                  "solid" (eighth PALETTE-COLORS)))
      (rectangle SCENE-WIDTH (+ (/ SCENE-HEIGHT 8) BAR-HEIGHT)
                 "solid" "white"))]))
|#

; dot->image : Dot -> Image
; (dot->image dot) returns a image of circle.
(define (dot->image dot)
  (circle (dot-size dot) "solid" (dot-color dot)))

(check-satisfied (dot->image (make-dot "violet" 20 100 200))
                 image?)

; world->image : World -> Image
; (world->image world) returns a image of dots on a blackboard by
; using the given world.
;(define initial-image
(define (world->image world)
  (if (empty? (world-ls world))
      (place-image/align
       palette (/ SCENE-WIDTH 2) 0 "center" "top"
       (rectangle SCENE-WIDTH SCENE-HEIGHT "solid" "black"))
      (place-image/align
       palette
       (/ SCENE-WIDTH 2) 0
       "center"
       "top"
       (place-image
        (dot->image (first (world-ls world)))
        (dot-x (first (world-ls world)))
        (dot-y (first (world-ls world)))
        (world->image
         (make-world
          (world-color world)
          (world-size world)
          (rest (world-ls world))
          (world-hold world)))))))

(check-satisfied (world->image w1) image?)

; key-handler : World KeyStroke -> World
; (key-handler world key) undo the last dot iff press right, redo
; the last dot iff press left.
(define (key-handler world key)
  (cond
    [(and (empty? (world-ls world))
          (equal? key "left")) world]
    [(and (empty? (world-hold world))
         (equal? key "right")) world]
    [(equal? key "right") (make-world
                           (world-color world)
                           (world-size world)
                           (cons (first (world-hold world))
                                 (world-ls world))
                           (rest (world-hold world)))]
    [(equal? key "left") (make-world
                          (world-color world)
                          (world-size world)
                          (rest (world-ls world))
                          (cons (first (world-ls world))
                                (world-hold world)))]
    [(equal? key " ") w0]
    [(equal? key "+") (make-world
                       (world-color world)
                       (+ (world-size world) 5)
                       (world-ls world)
                       (world-hold world))]
    [(equal? key "-") (make-world
                       (world-color world)
                       (if (= (world-size world) 5)
                          (world-size world)
                          (- (world-size world) 5))
                       (world-ls world)
                       (world-hold world))]
    [else world]))

(check-expect (key-handler w1 "-") (make-world
            "yellow"
            15
            (list (make-dot "violet" 20 100 200)
                  (make-dot "yellow" 20 200 100))
            '()))
(check-expect (key-handler w1 "+") (make-world
            "yellow"
            25
            (list (make-dot "violet" 20 100 200)
                  (make-dot "yellow" 20 200 100))
            '()))

; mouse-handler : World Nat Nat String -> World
; (mouse-handler world x y mouse) returns a dot on a picture by
; pressing the mouse, or drag a line by pressing the mouse.
(define (mouse-handler world x y mouse)
  (cond
    [(equal? mouse "button-down")
     (make-world (if (< y (/ SCENE-HEIGHT 8))
                     (pick-one x y)
                     (world-color world))
                 (world-size world)
                 (cons (make-dot (if (< y (/ SCENE-HEIGHT 8))
                                     (pick-one x y)
                                     (world-color world))
                                 (world-size world)
                                 x y)
                       (world-ls world))
                 (world-hold world))]
    [(equal? mouse "drag")
     (make-world (if (< y (/ SCENE-HEIGHT 8))
                     (pick-one x y)
                     (world-color world))
                 (world-size world)
                 (cons (make-dot (if (< y (/ SCENE-HEIGHT 8))
                                     (pick-one x y)
                                     (world-color world))
                                 (world-size world)
                                 x y)
                       (world-ls world))
                 (world-hold world))]
    [else world]))

(check-expect (mouse-handler w1 1 1 "button-down")
              (make-world
               'orange
               20
               (list
                (make-dot 'orange 20 1 1)
                (make-dot "violet" 20 100 200)
                (make-dot "yellow" 20 200 100))
               '()))
(check-expect (mouse-handler w1 10 10 "button-down")
              (make-world
               'orange
               20
               (list
                (make-dot 'orange 20 10 10)
                (make-dot "violet" 20 100 200)
                (make-dot "yellow" 20 200 100))
               '()))

; pick-one : Any Any -> Any
; (pick-one x y) selects color iff click the palette.
(define (pick-one x y)
  (cond
    [(and (< x (/ SCENE-WIDTH 8)) (< y (/ SCENE-HEIGHT 8)))
     (first PALETTE-COLORS)]
    [(and (< x (* (/ SCENE-WIDTH 8) 2)) (< y (/ SCENE-HEIGHT 8)))
     (second PALETTE-COLORS)]
    [(and (< x (* (/ SCENE-WIDTH 8) 3)) (< y (/ SCENE-HEIGHT 8)))
     (third PALETTE-COLORS)]
    [(and (< x (* (/ SCENE-WIDTH 8) 4)) (< y (/ SCENE-HEIGHT 8)))
     (fourth PALETTE-COLORS)]
    [(and (< x (* (/ SCENE-WIDTH 8) 5)) (< y (/ SCENE-HEIGHT 8)))
     (fifth PALETTE-COLORS)]
    [(and (< x (* (/ SCENE-WIDTH 8) 6)) (< y (/ SCENE-HEIGHT 8)))
     (sixth PALETTE-COLORS)]
    [(and (< x (* (/ SCENE-WIDTH 8) 7)) (< y (/ SCENE-HEIGHT 8)))
     (seventh PALETTE-COLORS)]
    [(and (< x (* (/ SCENE-WIDTH 8) 8)) (< y (/ SCENE-HEIGHT 8)))
     (eighth PALETTE-COLORS)]))

(check-expect (pick-one 1 2) 'orange)

; confetti : World -> World
; (confetti world) returns a blackboard can be used to draw image
; by pressing mouse.
(define (confetti world)
  (big-bang world
            [to-draw world->image]
            [on-key key-handler]
            [on-mouse mouse-handler]
            [name "Confetti"]))