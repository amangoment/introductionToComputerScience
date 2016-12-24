;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname a4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

(define handin "a4")
(define collaboration-statement "I worked alone")

;;;;;;;;;;;;;;;
;; Problem 1
;;;;;;;;;;;;;;;
;; 1a
; nat? : Nat -> Boolean
; (nat? n) finds out whether the type is Nat or not.
(define (nat? n)
   (and
    (integer? n)
    (>= n 0)))

(check-expect (nat? 'a) #false)
(check-expect (nat? 1) #true)
(check-expect (nat? 0) #true)
(check-expect (nat? "1") #false)
(check-expect (nat? -3) #false)
(check-expect (nat? 1.23) #false)

;; 1b
; 1String : 1String -> String
; (1string? n) finds out whether the type is string in one sign.
(define (1string? n)
  (and
   (string? n)
   (= (string-length n) 1)))

(check-expect (1string? 'a) #false)
(check-expect (1string? 1) #false)
(check-expect (1string? "A") #true)
(check-expect (1string? "") #false)
(check-expect (1string? "1") #true)
(check-expect (1string? "xx") #false)

;;;;;;;;;;;;;;;
;; Problem 2
;;;;;;;;;;;;;;;
;; 2a
#|
   A Nucleotide is one of
   - 'A
   - 'T
   - 'G
   - 'C
|#

;; 2b
; nucleotide? : Any -> Boolean
; (nucleotide? DNAF) checks whether DNAF is a one type Nucleotide
; or not.
(define (nucleotide? DNAF)
  (or (equal? DNAF 'A)
      (equal? DNAF 'T)
      (equal? DNAF 'G)
      (equal? DNAF 'C)))

(check-expect (nucleotide? 'a) #false)
(check-expect (nucleotide? 1) #false)
(check-expect (nucleotide? "A") #false)
(check-expect (nucleotide? 'T) #true)
(check-expect (nucleotide? "ATGC") #false)

;; 2c
; process-nucleotide : Nucleotide -> ...
; (process-nucleotide DNAF) checks whether the input is
; nucleotide or not.
(define (process-nucleotide DNAF)
  (cond
    [(equal? DNAF 'A) ...]
    [(equal? DNAF 'T) ...]
    [(equal? DNAF 'G) ...]
    [(equal? DNAF 'C) ...]
    [else (error "unknown DNA")]))

;; 2d
; dna-complement : String -> String
; (dna-complement DNAF) takes a nucleotide and returns the nucleotide
; that appears opposite it on the second strand.
(define (dna-complement DNAF)
  (cond
    [(equal? DNAF 'A) 'T]
    [(equal? DNAF 'T) 'A]
    [(equal? DNAF 'G) 'C]
    [(equal? DNAF 'C) 'G]
    [else (error "unknown nucleotide")]))

(check-expect (dna-complement 'A) 'T)
(check-expect (dna-complement 'G) 'C)
(check-error (dna-complement 'X) "unknown nucleotide")

;;;;;;;;;;;;;;;
;; Problem 3
;;;;;;;;;;;;;;;
; planet-radiust : Planet -> Nat
; (planet-radius a-planet) gives the radius of the input planet.
(define (planet-radius a-planet)
  (cond
    [(equal? a-planet 'mercury) #i1524.6875]
    [(equal? a-planet 'venus) #i3782.5]
    [(equal? a-planet 'earth) #i3986.25]
    [(equal? a-planet 'mars) #i2122.5]
    [(equal? a-planet 'jupiter) #i44682.5]
    [(equal? a-planet 'saturn) #i37667.5]
    [(equal? a-planet 'uranus) #i15974.375]
    [(equal? a-planet 'neptune) #i15477.5]
    [else (error "unknown planet")]))

(check-within (planet-radius 'mercury) #i1524 #i1515)
(check-within (planet-radius 'venus) #i3782 #i3783)
(check-within (planet-radius 'earth) #i3986 #i3987)
(check-within (planet-radius 'mars) #i2122 #i2123)
(check-within (planet-radius 'jupiter) #i44682 #i44683)
(check-within (planet-radius 'saturn) #i37667 #i37668)
(check-within (planet-radius 'uranus) #i15974 #i15975)
(check-within (planet-radius 'neptune) #i15477 #i15478)

;;;;;;;;;;;;;;;
;; Problem 4
;;;;;;;;;;;;;;;
; choose-one : String -> String
; (choose-one word) return a string consisting of the
; character at the selected position.
(define (choose-one word)
  (string-ith word (random (string-length word))))

; (check-expect (choose-one "") #false)

;;;;;;;;;;;;;;;
;; Problem 5
;;;;;;;;;;;;;;;
; euclidean-distance : Number -> Number
; (euclidean-distance function1 function2) takes two arguments,
; both of which are of type Posn and computes the
; between two points as 2b of a2.
(define (euclidean-distance posn1 posn2)
  (sqrt (+ (sqr (- (posn-x posn1) (posn-x posn2)))
           (sqr (- (posn-y posn1) (posn-y posn2))))))

(define the-origin (make-posn 0 0))
(define some-point (make-posn 3 7))

(check-within (euclidean-distance the-origin some-point) #i6 #i7)
(check-expect (euclidean-distance (make-posn 1 1)
                                  (make-posn 4 5)) 5)

;;;;;;;;;;;;;;;
;; Problem 6
;;;;;;;;;;;;;;;
;; 6a
; A Domino is a (make-domino Image-Color Image-Color Nat String)
(define-struct domino [color1 color2 size orientation])

; make-domino : Image-Color Image-Color Nat String -> Domino
; domino? : Any -> Boolean
; domino-color1 : Domino -> Image-Color
; domino-color2 : Domino -> Image-Color
; domino-size : Domino -> Nat
; domino-orientation : Domino -> String

;; 6b
(define d1 (make-domino "red" "black" 30 "hor"))
(define d2 (make-domino "blue" "green" 50 "ver"))

(check-satisfied d1 domino?)
(check-satisfied d2 domino?)
(check-expect (domino-color1 d1) "red")
(check-expect (domino-color2 d2) "green")
(check-expect (domino-size d1) 30)
(check-expect (domino-size d2) 50)
(check-expect (domino-orientation d1) "hor")
(check-expect (domino-orientation d2) "ver")
(check-satisfied (domino-size d1) number?)
(check-satisfied (domino-size d2) number?)
(check-satisfied (domino-color1 d1) string?)
(check-satisfied (domino-color2 d2) string?)

;; 6c
; process-domino : Domino -> ...
(define (process-domino a-domino)
  (... (domino-color1 a-domino) ...
       (domino-color2 a-domino) ...
       (domino-size a-domino) ...
       (domino-orientation a-domino) ...))

;; 6d
; domino-scale : Num Num -> Num
; (domino-scale n) takes a domino and a positive number,
; and returns a new domino

(define (domino-scale domino n)
  (make-domino
   (domino-color1 domino)
   (domino-color2 domino)
   (round (* (domino-size domino) n))
   (domino-orientation domino)))

(check-expect (domino-color1 d1) "red")
(check-expect (domino-color2 d2) "green")
(check-expect (domino-size (domino-scale d1 5)) 150)
(check-expect (domino-size (domino-scale d2 1/3)) 17)

;; 6e
; domino->image : String Domino -> Image
; (domino->image color1 color2 size orientation) takes a
; domino and returns the corresponding image

(define (domino->image a-domino)
  (cond
   [(equal? (domino-orientation a-domino) "hor")
    (beside
     (square (domino-size a-domino)
          "solid"
          (domino-color1 a-domino))
     (square (domino-size a-domino)
          "solid"
          (domino-color2 a-domino)))]
   [(equal? (domino-orientation a-domino) "ver")
    (above
     (square (domino-size a-domino)
          "solid"
          (domino-color1 a-domino))
     (square (domino-size a-domino)
          "solid"
          (domino-color2 a-domino)))]
   [else #false]))

(domino->image d1)
(domino->image d2)
(check-expect (image-width (domino->image d1)) 60)
(check-expect (image-height (domino->image d1)) 30)
(check-expect (image-height (domino->image d2)) 100)
(domino->image (domino-scale d2 4.1275))
(domino->image (domino-scale (domino-scale d1 2) 3))

;; 6f
; domino-reverse : String -> Image
; (domino-reverse a-domino) : takes a domino and returns the result
; of swapping its two color fields
(define (domino-reverse a-domino)
  (make-domino
       (domino-color2 a-domino)
       (domino-color1 a-domino)
       (domino-size a-domino)
       (domino-orientation a-domino)))

(domino->image (domino-reverse d1))
(domino->image (domino-reverse d2))
(domino->image (domino-reverse (domino-reverse d1)))

;; 6g
; domino-turn : String -> Image
; (domino-turn a-domino) : takes a domino and returns
; the result of flipping its orientation
(define (domino-turn a-domino)
    (make-domino
       (domino-color1 a-domino)
       (domino-color2 a-domino)
       (domino-size a-domino)
       (if
         (equal? (domino-orientation a-domino) "ver")
         "hor"
         "ver")))

(domino->image (domino-turn d1))
(domino->image (domino-turn d2))
(domino->image (domino-reverse (domino-scale (domino-turn d2)
                                             3)))

;; 6h
; domino-valid? : Any -> Boolean
; (domino-valid? a-domino) take one input and returns #true if and
; only if it represents a properly formed domino.

(define (domino-valid? a-domino)
  (and
   (domino? a-domino)
   (image-color? (domino-color1 a-domino))
   (image-color? (domino-color2 a-domino))
   (not (equal? (domino-color1 a-domino) (domino-color2 a-domino)))
   (number? (domino-size a-domino))
   (> (domino-size a-domino) 0)
   (or
     (equal? (domino-orientation a-domino) "hor")
     (equal? (domino-orientation a-domino) "ver"))))


(check-satisfied d1 domino-valid?)
(check-satisfied d2 domino-valid?)
(check-expect (domino-valid? 5) #false)
(check-expect (domino-valid? (make-domino 1 2 3 4)) #false)
(check-expect (domino-valid? (make-domino "red" "red" 10
                                          "hor")) #false)
(check-expect (domino-valid? (make-domino "red" "blue" 0
                                          "ver")) #false)
(check-expect (domino-valid? (make-domino "red" "blue"
                                          20 "vor")) #false)


;;;;;;;;;;;;;;;
;; Problem 7
;;;;;;;;;;;;;;;
;; 7a
; An Interval is a (make-interval a b).
; a is less than or equal to b.

;; 7b
; Interval : Num Num -> Boolean
; (Interval [a b]) gives the range of number.
(define-struct interval [a b])

;; 7c
(define (process-interval an-interval)
  (... (interval-a an-interval) ...
       (interval-b an-interval) ...))

;; 7d
; interval-valid? : Interval -> Boolean
; (interval-valid? num) takes one input and returns #true
; iff it represents a valid interval.

(define (interval-valid? num)
  (and
   (interval? num)
   (number? (interval-a num))
   (number? (interval-b num))
   (<= (interval-a num) (interval-b num))))

(check-expect (interval-valid? 5) #false)
(check-expect (interval-valid? (make-interval 4 4)) #true)
(check-expect (interval-valid? (make-interval 8 2)) #false)
(check-expect (interval-valid? (make-interval 'a 'b)) #false)

;; 7e
; neighbors : Ing -> Ing
; (neighbors num) takes an integer and returns the Interval
; starting at the integer to its left and ending with the
; integer to its right on the number line.
(define (neighbors num)
  (make-interval
  (-  num 1)
  (+  num 1)))

(check-satisfied (neighbors 3) interval?)
(check-expect (interval-a (neighbors 1)) 0)
(check-expect (interval-b (neighbors -7)) -6)


;; 7f
; overlaps? : Interval Interval -> Boolean
; (overlaps? int1 int2) takes two Intervals and returns #true iff
; the intervals overlap
(define (overlaps? int1 int2)
  (or
  (< (interval-a int2) (interval-b int1))
  (< (interval-a int1) (interval-b int2))))
   

(define area (make-interval 0 5))
(check-expect (overlaps? area (make-interval 3 10)) #true)
(check-expect (overlaps? area (make-interval -3 10)) #true)
(check-expect (overlaps? area (make-interval -3 4)) #true)
(check-expect (overlaps? area (make-interval 2 3)) #true)

;;;;;;;;;;;;;;;
;; Problem 8
;;;;;;;;;;;;;;;
;; 8a
(define IND (make-posn 39.7669106 -86.1499634))
(define JHM (make-posn 21.0014801 -156.6556854))

(check-satisfied IND posn?)
(check-satisfied JHM posn?)

;; 8b
; haversin start end : Num Num -> Num
; (haversin start end) compute the distance between the two
; airports by plugging their GPS coordinates into the Haversine
; Formula for computing distances between two points on a sphere.
(define (haversin start end)
  (* 2 (planet-radius 'earth)
     (asin
      (sqrt
       (+ (sqr (sin (/ (- (posn-x end) (posn-x start)) 2)))
          (* (cos (posn-x start))
             (cos (posn-x end))
             (sqr (sin (/ (- (posn-y end) (posn-y start)) 2)))))))))
 
;; 8c
; degrees->radians : Num -> Num
; (degrees->radians degrees) returns the result of converting
; a number from degrees into radians
(define (degrees->radians degrees)
  (/ (* degrees pi) 180))

; spherical-distance : Posn Posn -> Num
; (spherical-distance point) find the distance between IND
; and the vacation destination.
(define (spherical-distance point1 point2)
  (degrees->radians (haversin point1 point2)))

(define so-far-away (spherical-distance IND JHM))
; My vacation destination is about #i2684.9599952321855 miles away

;; 8d
; haversin-helper : Num Num -> Num
; (haversin-helper num1 num2) performs the common computation.
(define (haversin-helper num1 num2)
  (sqr (sin (/ (- num1 num2) 2))))

;;;;;;;;;;;;;;;
;; Problem 9
;;;;;;;;;;;;;;;

;; 9a
; Airport is a (make-airport String Nat Nat)

;; 9b
; 
(define-struct airport [location code gps])

;; 9c
(define indy
  (make-airport "Indianapolis, IN"
                'IND
                IND))

(define maui
  (make-airport "Kapalua, HI"
                'JHM
                JHM))

(check-satisfied indy airport?)
(check-satisfied maui airport?)
(check-satisfied (airport-location indy) string?)
(check-satisfied (airport-code indy) symbol?)
(check-satisfied (airport-gps indy) posn?)

;; 9d
; get-latitude : Airport -> Number
; (get-latitude an-airport) returns the latitude (in degrees)
; corresponding to its GPS coordinates.
(define (get-latitude an-airport)
  (round (posn-x (airport-gps an-airport))))

(check-satisfied (get-latitude indy) number?)
(check-satisfied (get-latitude maui) number?)
(check-within (round (get-latitude indy)) #i40 #i1)

;; 9e
; get-longitude : Airport -> Number
; (get-latitude an-airport) returns the longitude (in degrees)
; corresponding to its GPS coordinates.
(define (get-longitude an-airport)
  (posn-y (airport-gps an-airport)))

(check-satisfied (get-longitude indy) number?)
(check-satisfied (get-longitude maui) number?)
(check-within (round (get-longitude indy)) #i-86 #i1)

;; 9f
; airport-distance : Airport -> Number
; (airport-distance a) returns the number of spherical
; miles separating them
(define (airport-distance a1 a2)
  (spherical-distance (airport-gps a1) (airport-gps a2)))

(check-within (round (airport-distance indy maui)) #i2685.0 #i2684.0)
(check-within (round (airport-distance maui indy)) #i2685.0 #i2684.0)