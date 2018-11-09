;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assign3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CMPU-101 
; Fall 2018
; Assign 3
; aeisert 
;
; Description: Uses a list of bouncing balls to animate many balls
; of different sizes and colors, all moving in the same scene at 
; different speeds.

(require 2htdp/image) 
(require 2htdp/universe)

(define RADIUS 25)

; Scene dimensions
(define WIDTH 500)
(define HEIGHT 300)

; Create the background scene image
(define BACKGROUND
  (place-image (rectangle WIDTH HEIGHT "solid" "lightgray")
               (/ WIDTH 2) (/ HEIGHT 2)
               (empty-scene WIDTH HEIGHT)))

; Data Definitions 
(define-struct ball (im x y dx dy))
; A ball is a (make-ball im p dx dy) where
; im is an image (of the ball), 
; x and y are numbers representing the ball's position, and
; dx and dy are numbers representing the ball's horizontal and 
;   vertical velocity

; Data Definition for a list-of-balls:
; A list-of-balls is either:
; 1. '(), or
; 2. (cons b lob), where b is a ball
;    and lob is a list-of-balls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define four (4) example ball CONSTANTS:
;   one touching each edge of the scene (top, bottom, left, right)
;   These will help you test bounce conditions.

; here's one of my ball CONSTANTS, which you may use or modify
; if you like to define the rest.
(define BALL-AT-LEFT 
  (make-ball (circle (+ RADIUS 4) "solid" "green")
             (+ RADIUS 4) (/ HEIGHT 2) -4 4)) 

(define BALL-AT-RIGHT
  (make-ball (circle (- RADIUS 6) "solid" "orange")
             (- WIDTH (- RADIUS 6)) (/ HEIGHT 3) 7 -2))

(define BALL-AT-TOP
  (make-ball (circle (+ RADIUS 2) "solid" "purple")
             (/ WIDTH 3) (+ RADIUS 2) 2 -6))

(define BALL-AT-BOTTOM
  (make-ball (circle RADIUS "solid" "yellow")
             (/ WIDTH 2) (- HEIGHT RADIUS) 3 3))
             
; Define INIT-LOB to be a list-of-balls:
; You will use this to be the initial state of the world.
; I've defined it to be the empty list, but you should define it
; to contain the four example ball CONSTANTS you just defined. 
(define INIT-LOB
  (list BALL-AT-LEFT BALL-AT-RIGHT BALL-AT-TOP BALL-AT-BOTTOM)) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Templates for a ball and a list-of-balls.
; Use these to help you get started with the functions below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ball -> ???
; Template for a function that consumes a ball
(define (fun-for-ball b) 
  (...(ball-im b)...
   ...(ball-x b)...(ball-y b)...
   ...(ball-dx b)...(ball-dy b)...))

; list-of-balls -> ???
; Template for a function that consumes a list-of-balls
(define (fun-for-list-of-balls lob) 
  (cond
    [(empty? lob)...] 
    [else (...(fun-for-ball (first lob))...
           ...(fun-for-lob (rest lob))...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Design the functions below, in order. I've supplied the
; signature, purpose statement, and header for each function.
;
; You provide the check-expect examples, and using the appropriate
; template, complete the function bodies.
;
; I recommend you proceed in order, and complete each function,
; with passing tests, before going on to the next.
;
; The reason for completing the functions in the order they appear
; is earlier functions can be used as helper functions for the
; later functions.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ball -> number
; computes the radius of given ball
(define (ball-radius b)
  (/ (image-width (ball-im b)) 2))

(check-expect (ball-radius BALL-AT-LEFT) (+ RADIUS 4))
(check-expect (ball-radius BALL-AT-RIGHT) (- RADIUS 6))
(check-expect (ball-radius BALL-AT-TOP) (+ RADIUS 2))
(check-expect (ball-radius BALL-AT-BOTTOM) RADIUS)

; ball -> boolean
; determines whether the ball reached the top edge of scene
(define (top-edge? b)
  (>= (ball-radius b) (ball-y b)))

(check-expect (top-edge? (make-ball (circle 27 "solid" "purple") 50 13.5 50 20)) #true)
(check-expect (top-edge? (make-ball (circle 25 "solid" "blue") 20 20 75 30)) #true)
(check-expect (top-edge? (make-ball (circle 20 "solid" "orange") 200 200 85 100)) #false)

; ball -> boolean
; determines whether the ball reached the bottom edge of scene
(define (bottom-edge? b)
  (<= (- HEIGHT (ball-radius b)) (ball-y b)))

(check-expect (bottom-edge? (make-ball (circle 27 "solid" "purple") 50 75 50 20)) #false)
(check-expect (bottom-edge? (make-ball (circle 25 "solid" "blue") 20 275 75 30)) #true)
(check-expect (bottom-edge? (make-ball (circle 20 "solid" "orange") 200 281 85 100)) #true)

; ball -> boolean
; determines whether the ball reached the left edge of scene
(define (left-edge? b)
  (>= (ball-radius b) (ball-x b)))

(check-expect (left-edge? (make-ball (circle 27 "solid" "purple") 25 13.5 50 20)) #true)
(check-expect (left-edge? (make-ball (circle 25 "solid" "blue") 25 20 75 30)) #true)
(check-expect (left-edge? (make-ball (circle 20 "solid" "orange") 200 200 85 100)) #false)

; ball -> boolean
; determines whether the ball reached the right edge of scene
(define (right-edge? b)
  (<= (- WIDTH (ball-radius b)) (ball-x b)))

(check-expect (right-edge? (make-ball (circle 27 "solid" "purple") 474 50 50 20)) #true)
(check-expect (right-edge? (make-ball (circle 25 "solid" "blue") 475 20 75 30)) #true)
(check-expect (right-edge? (make-ball (circle 20 "solid" "orange") 200 200 85 100)) #false)

; ball -> ball
; reverse ball's up-down direction   
(define (reverse-up-down b)
  (make-ball (ball-im b) (ball-x b) (ball-y b) (ball-dx b) (* (ball-dy b) -1)))

(check-expect (reverse-up-down (make-ball (circle 27 "solid" "purple") 50 13.5 50 20))
              (make-ball (circle 27 "solid" "purple") 50 13.5 50 -20))
(check-expect (reverse-up-down (make-ball (circle 25 "solid" "blue") 20 20 75 -30))
              (make-ball (circle 25 "solid" "blue") 20 20 75 30))
(check-expect (reverse-up-down (make-ball (circle 20 "solid" "orange") 200 200 85 0))
              (make-ball (circle 20 "solid" "orange") 200 200 85 0))

; ball -> ball
; reverse ball's left-right direction   
(define (reverse-left-right b)
  (make-ball (ball-im b) (ball-x b) (ball-y b) (* (ball-dx b) -1) (ball-dy b)))

(check-expect (reverse-left-right (make-ball (circle 27 "solid" "purple") 50 13.5 50 20))
              (make-ball (circle 27 "solid" "purple") 50 13.5 -50 20))
(check-expect (reverse-left-right (make-ball (circle 25 "solid" "blue") 20 20 -75 30))
              (make-ball (circle 25 "solid" "blue") 20 20 75 30))
(check-expect (reverse-left-right (make-ball (circle 20 "solid" "orange") 200 200 0 100))
              (make-ball (circle 20 "solid" "orange") 200 200 0 100))

; ball -> ball
; changes direction of given ball if it hit the top or bottom edge
(define (bounce-up-down b)
  (cond
    [(or (top-edge? b) (bottom-edge? b))
     (reverse-up-down b)]
    [else
     b]))

(check-expect (bounce-up-down (make-ball (circle 27 "solid" "purple") 50 27 50 20))
              (make-ball (circle 27 "solid" "purple") 50 27 50 -20))
(check-expect (bounce-up-down (make-ball (circle 25 "solid" "blue") 20 20 75 30))
              (make-ball (circle 25 "solid" "blue") 20 20 75 -30))
(check-expect (bounce-up-down (make-ball (circle 20 "solid" "orange") 200 19 85 0))
              (make-ball (circle 20 "solid" "orange") 200 19 85 0))
(check-expect (bounce-left-right (make-ball (circle 40 "solid" "yellow") 50 200 5 50))
              (make-ball (circle 40 "solid" "yellow") 50 200 5 50))
(check-expect (bounce-up-down (make-ball (circle 27 "solid" "purple") 50 273 50 -20))
              (make-ball (circle 27 "solid" "purple") 50 273 50 20))
(check-expect (bounce-up-down (make-ball (circle 25 "solid" "blue") 20 276 75 -30))
              (make-ball (circle 25 "solid" "blue") 20 276 75 30))
    
; ball -> ball
; changes direction of given ball if it hit the left or right edge
(define (bounce-left-right b)
    (cond
    [(or (left-edge? b) (right-edge? b))
     (reverse-left-right b)]
    [else
     b]))

(check-expect (bounce-left-right (make-ball (circle 27 "solid" "purple") 27 90 -50 20))
              (make-ball (circle 27 "solid" "purple") 27 90 50 20))
(check-expect (bounce-left-right (make-ball (circle 25 "solid" "blue") 24 20 -75 30))
              (make-ball (circle 25 "solid" "blue") 24 20 75 30))
(check-expect (bounce-left-right (make-ball (circle 20 "solid" "orange") 19 200 0 85))
              (make-ball (circle 20 "solid" "orange") 19 200 0 85))
(check-expect (bounce-left-right (make-ball (circle 40 "solid" "yellow") 50 200 5 50))
              (make-ball (circle 40 "solid" "yellow") 50 200 5 50))
(check-expect (bounce-left-right (make-ball (circle 27 "solid" "purple") 473 50 50 20))
              (make-ball (circle 27 "solid" "purple") 473 50 -50 20))
(check-expect (bounce-left-right (make-ball (circle 25 "solid" "blue") 476 20 75 30))
              (make-ball (circle 25 "solid" "blue") 476 20 -75 30))

; ball -> ball
; moves the given ball by its dx and dy amounts
(define (move-ball b)
  (make-ball (ball-im b) (+ (ball-dx b) (ball-x b))
             (+ (ball-dy b) (ball-y b)) (ball-dx b) (ball-dy b)))

(check-expect (move-ball (make-ball (circle 27 "solid" "purple") 27 90 50 20))
              (make-ball (circle 27 "solid" "purple") 77 110 50 20))
(check-expect (move-ball (make-ball (circle 25 "solid" "blue") 50 20 -15 30))
              (make-ball (circle 25 "solid" "blue") 35 50 -15 30))
(check-expect (move-ball (make-ball (circle 20 "solid" "orange") 30 200 0 -85))
              (make-ball (circle 20 "solid" "orange") 30 115 0 -85))
(check-expect (move-ball (make-ball (circle 27 "solid" "purple") 400 50 -50 -20))
              (make-ball (circle 27 "solid" "purple") 350 30 -50 -20))
                         
; list-of-balls -> list-of-balls
; moves (and possibly bounces) each ball in given list
(define (move-list-of-balls lob)
  (cond
    [(empty? lob) '()]
    [(cons? lob)
     (cons (move-ball (bounce-up-down (bounce-left-right (first lob))))
           (move-list-of-balls (rest lob)))]))

(check-expect (move-list-of-balls (list (make-ball (circle 20 "solid" "orange") 30 200 0 -85)
                                   (make-ball (circle 27 "solid" "purple") 400 50 -50 -20)
                                   (make-ball (circle 25 "solid" "blue") 476 30 75 30)
                                   (make-ball (circle 21 "solid" "red") 21 30 -30 75)))
              (list (make-ball (circle 20 "solid" "orange") 30 115 0 -85)
                    (make-ball (circle 27 "solid" "purple") 350 30 -50 -20)
                    (make-ball (circle 25 "solid" "blue") 401 60 -75 30)
                    (make-ball (circle 21 "solid" "red") 51 105 30 75))) 

; ball image -> image
; renders given ball b on given background bg
(define (render-ball b bg)
  (place-image (ball-im b) (ball-x b) (ball-y b) bg))

(check-expect (render-ball (make-ball (circle 20 "solid" "blue") 401 60 -75 30) BACKGROUND)
              (place-image (circle 20 "solid" "blue") 401 60 BACKGROUND))
  
; list-of-balls -> image 
; produces image of each ball at each given current position on
; background.
; (Yes, I provided this function for you! You shouldn't have to
;  touch it if you've correctly implemented the functions above.)
(define (render-balls lob) 
  (cond [(empty? lob) BACKGROUND]
        [else (render-ball (first lob)
                           (render-balls (rest lob)))]))

(check-expect (render-balls (list (make-ball (circle 20 "solid" "orange") 30 115 0 -85)
                    (make-ball (circle 27 "solid" "purple") 350 30 -50 -20)
                    (make-ball (circle 25 "solid" "blue") 401 60 -75 30)
                    (make-ball (circle 21 "solid" "red") 51 105 30 75)))
              (place-images (list (circle 20 "solid" "orange")
                                  (circle 27 "solid" "purple")
                                  (circle 25 "solid" "blue")
                                  (circle 21 "solid" "red"))
                            (list (make-posn 30 115)
                                  (make-posn 350 30)
                                  (make-posn 401 60)
                                  (make-posn 51 105)) BACKGROUND))

; Here's the main function with the big-bang expression!
; Once you've implemented move-list-of-balls, uncomment on-tick below.
(define (main w)
  (big-bang w
            (on-tick move-list-of-balls 1/28) 
            (to-draw render-balls)))

; Run program automatically, or type this in Interactions Pane:
; Use INIT-LOB as the initial state of the world...
(main INIT-LOB)