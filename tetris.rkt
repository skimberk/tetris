;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tetris) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;; Tetris!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Definitions

(define cell-size   30) ; Size of cell in pixels
(define game-width  10) ; Width in cells
(define game-height 20) ; Height in cells

(define bg (empty-scene (* cell-size game-width)
                        (* cell-size game-height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A Block is a (make-block Number Number Color)
(define-struct block (x y color))

;; A Tetra is a (make-tetra Posn BSet)
;; The center point is the point around which the tetra rotates
;; when it spins.
(define-struct tetra (center blocks))

;; A Set of Blocks (BSet) is one of:
;; - empty
;; - (cons Block BSet)
;; Order does not matter.

;; A Set of Tetra (TSet) is one of:
;; - empty
;; - (cons Tetra TSet)
;; Order does not matter.

;; A List of Numbers (LON) is one of:
;; - empty
;; - (cons Number LON)

;; A Score is an integer number [0, infinity)

;; A World is a (make-world Tetra BSet Score)
;; The BSet represents the pile of blocks at the bottom of the screen.
(define-struct world (tetra pile score))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tetra Definitions

;; "O" tetra: xx
;;            xx
(define o-tetra-color 'green)
(define o-tetra (make-tetra (make-posn 0.5 0.5)
                            (list (make-block 0 0 o-tetra-color)
                                  (make-block 1 0 o-tetra-color)
                                  (make-block 0 1 o-tetra-color)
                                  (make-block 1 1 o-tetra-color))))

;; "I" tetra: xxxx
(define i-tetra-color 'blue)
(define i-tetra (make-tetra (make-posn 1 1)
                            (list (make-block 0 1 i-tetra-color)
                                  (make-block 1 1 i-tetra-color)
                                  (make-block 2 1 i-tetra-color)
                                  (make-block 3 1 i-tetra-color))))

;; "L" tetra:   x
;;            xxx
(define l-tetra-color 'purple)
(define l-tetra (make-tetra (make-posn 1 1)
                            (list (make-block 3 0 l-tetra-color)
                                  (make-block 1 1 l-tetra-color)
                                  (make-block 2 1 l-tetra-color)
                                  (make-block 3 1 l-tetra-color))))

;; "J" tetra: x
;;            xxx
(define j-tetra-color 'turquoise)
(define j-tetra (make-tetra (make-posn 1 1)
                            (list (make-block 0 0 j-tetra-color)
                                  (make-block 0 1 j-tetra-color)
                                  (make-block 1 1 j-tetra-color)
                                  (make-block 2 1 j-tetra-color))))

;; "T" tetra:  x
;;            xxx
(define t-tetra-color 'orange)
(define t-tetra (make-tetra (make-posn 1 1)
                            (list (make-block 1 0 t-tetra-color)
                                  (make-block 0 1 t-tetra-color)
                                  (make-block 1 1 t-tetra-color)
                                  (make-block 2 1 t-tetra-color))))

;; "Z" tetra: xx
;;             xx
(define z-tetra-color 'pink)
(define z-tetra (make-tetra (make-posn 1 1)
                            (list (make-block 0 0 z-tetra-color)
                                  (make-block 1 0 z-tetra-color)
                                  (make-block 1 1 z-tetra-color)
                                  (make-block 2 1 z-tetra-color))))

;; "S" tetra:  xx
;;            xx
(define s-tetra-color 'red)
(define s-tetra (make-tetra (make-posn 1 1)
                            (list (make-block 1 0 s-tetra-color)
                                  (make-block 2 0 s-tetra-color)
                                  (make-block 0 1 s-tetra-color)
                                  (make-block 1 1 s-tetra-color))))

(define all-tetra (list o-tetra
                        i-tetra
                        l-tetra
                        j-tetra
                        t-tetra
                        z-tetra
                        s-tetra))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Definitions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering Functions

;;; Draw a block onto an image.
;;; draw-block: Block Image -> Image
(define (draw-block b img)
  (place-image (rectangle cell-size
                          cell-size
                          'solid
                          (block-color b))
               (* cell-size
                  (+ 0.5 (block-x b)))
               (* cell-size
                  (+ 0.5 (block-y b)))
               img))

(check-expect (draw-block (make-block 2 2 'green) bg)
              (place-image (rectangle cell-size
                                      cell-size
                                      'solid
                                      'green)
                           (* cell-size
                              (+ 0.5 2))
                           (* cell-size
                              (+ 0.5 2))
                           bg))

(check-expect (draw-block (make-block 4 1 'red) bg)
              (place-image (rectangle cell-size
                                      cell-size
                                      'solid
                                      'red)
                           (* cell-size
                              (+ 0.5 4))
                           (* cell-size
                              (+ 0.5 1))
                           bg))

;; Draw list of blocks onto image.
;; draw-blocks : BSet Image -> Image
(define (draw-blocks bs img)
  (foldl (λ (b current-img)
           (draw-block b current-img))
         img bs))

(check-expect (draw-blocks '() bg) bg)
(check-expect (draw-blocks (list (make-block 2 2 'green)
                                 (make-block 4 1 'red))
                           bg)
              (draw-block (make-block 2 2 'green)
                          (draw-block (make-block 4 1 'red)
                                      bg)))

;; Draws text for score.
;; Number -> Image
(define (draw-score n)
  (text (string-append "Score: " (number->string n))
                    18 'black))

(check-expect (draw-score 0) (text "Score: 0" 18 'black))
(check-expect (draw-score 12) (text "Score: 12" 18 'black))

;; Draws a world (by converting it to an image).
;; draw : World -> Image
(define (draw w)
  (place-image (draw-score (world-score w))
               45 15
               (draw-blocks (tetra-blocks (world-tetra w))
                           (draw-blocks (world-pile w) bg))))

(check-expect (draw (make-world l-tetra
                                (list (make-block 2 2 'green)
                                      (make-block 4 1 'red))
                                37))
              (place-image
               (draw-score 37)
               45 15
               (draw-blocks (tetra-blocks l-tetra)
                            (draw-blocks (list (make-block 2 2 'green)
                                              (make-block 4 1 'red))
                                        bg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rotation Functions

;; Rotate blocks 90 degrees clockwise around the posn.
;; blocks-rotate-cw : Posn BSet -> BSet
(define (blocks-rotate-cw c bs)
  (map (λ (b)
         (make-block (+ (posn-x c)
                        (- (posn-y c)
                           (block-y b)))
                     (+ (posn-y c)
                        (- (block-x b)
                           (posn-x c)))
                     (block-color b)))
       bs))

(check-expect (blocks-rotate-cw (make-posn 1 2) empty) empty)
(check-expect (blocks-rotate-cw (make-posn 1 2)
                                (list (make-block 1 2 'green)
                                      (make-block 2 3 'green)))
              (list (make-block 1 2 'green)
                    (make-block 0 3 'green)))

;; Rotate blocks 90 degrees counterclockwise around the posn.
;; blocks-rotate-ccw : Posn BSet -> BSet
(define (blocks-rotate-ccw c bs)
  (blocks-rotate-cw c (blocks-rotate-cw c (blocks-rotate-cw c bs))))

(check-expect (blocks-rotate-ccw (make-posn 1 2) empty) empty)
(check-expect (blocks-rotate-ccw (make-posn 1 2)
                                 (list (make-block 1 2 'green)
                                       (make-block 2 3 'green)))
              (list (make-block 1 2 'green)
                    (make-block 2 1 'green)))

;; Rotates tetra 90 degrees clockwise.
;; tetra-rotate-cw : Tetra -> Tetra
(define (tetra-rotate-cw t)
  (make-tetra (tetra-center t)
              (blocks-rotate-cw (tetra-center t)
                                (tetra-blocks t))))

(check-expect (tetra-rotate-cw j-tetra)
              (make-tetra (tetra-center j-tetra)
                          (blocks-rotate-cw (tetra-center j-tetra)
                                            (tetra-blocks j-tetra))))
(check-expect (tetra-rotate-cw l-tetra)
              (make-tetra (tetra-center l-tetra)
                          (blocks-rotate-cw (tetra-center l-tetra)
                                            (tetra-blocks l-tetra))))

;; Rotates tetra 90 degrees counterclockwise.
;; tetra-rotate-ccw : Tetra -> Tetra
(define (tetra-rotate-ccw t)
  (make-tetra (tetra-center t)
              (blocks-rotate-ccw (tetra-center t)
                                 (tetra-blocks t))))

(check-expect (tetra-rotate-ccw j-tetra)
              (make-tetra (tetra-center j-tetra)
                          (blocks-rotate-ccw (tetra-center j-tetra)
                                             (tetra-blocks j-tetra))))
(check-expect (tetra-rotate-ccw l-tetra)
              (make-tetra (tetra-center l-tetra)
                          (blocks-rotate-ccw (tetra-center l-tetra)
                                             (tetra-blocks l-tetra))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movement Functions

;; Move blocks by given number of cells.
;; move-blocks : BSet Number Number -> BSet
(define (move-blocks bs x y)
  (map (λ (b)
         (make-block (+ x (block-x b))
                     (+ y (block-y b))
                     (block-color b)))
       bs))

(define test-blocks (list (make-block 0 0 'green)
                          (make-block 2 3 'red)
                          (make-block -10 -20 'pink)))

(check-expect (move-blocks empty 10 20)     empty)
(check-expect (move-blocks test-blocks 0 0) test-blocks)
(check-expect (move-blocks test-blocks 2 -3)
              (list (make-block 2 -3 'green)
                    (make-block 4 0 'red)
                    (make-block -8 -23 'pink)))

;; Move tetra by given number of cells.
;; move-tetra : Tetra Number Number -> Tetra
(define (move-tetra t x y)
  (make-tetra (make-posn (+ x (posn-x (tetra-center t)))
                         (+ y (posn-y (tetra-center t))))
              (move-blocks (tetra-blocks t) x y)))

(check-expect (move-tetra s-tetra 0 0) s-tetra)
(check-expect (move-tetra s-tetra 2 -3)
              (make-tetra (make-posn (+ 2  (posn-x (tetra-center s-tetra)))
                                     (+ -3 (posn-y (tetra-center s-tetra))))
                          (move-blocks (tetra-blocks s-tetra) 2 -3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collision Functions

;; Does block intersect any of the blocks in the list?
;; block-intersects? : Block BSet -> Boolean
(define (block-intersects? b bs)
  (ormap (λ (b-curr)
           (and (= (block-x b)
                   (block-x b-curr))
                (= (block-y b)
                   (block-y b-curr))))
           bs))

(check-expect (block-intersects? (make-block 0 0 'green) empty)       #false)
(check-expect (block-intersects? (make-block 1 1 'green) test-blocks) #false)
(check-expect (block-intersects? (make-block 0 0 'green) test-blocks) #true)
(check-expect (block-intersects? (make-block 0 0 'red) test-blocks)   #true)

;; Does any block in the first list intersect any block in the second?
;; blocks-intersect? : BSet BSet -> Boolean
(define (blocks-intersect? x y)
  (ormap (λ (x-curr)
           (block-intersects? x-curr y))
         x))

(check-expect (blocks-intersect? empty empty)             #false)
(check-expect (blocks-intersect? test-blocks empty)       #false)
(check-expect (blocks-intersect? empty test-blocks)       #false)
(check-expect (blocks-intersect? test-blocks test-blocks) #true)
(check-expect (blocks-intersect? (list (make-block 0 0 'green))
                                 test-blocks) #true)

;; Does any block intersect either left or right wall?
;; blocks-intersect-wall? : BSet -> Boolean
(define (blocks-intersect-wall? bs)
  (ormap (λ (b)
           (or (< (block-x b) 0)
               (>= (block-x b) game-width)))
         bs))

(check-expect (blocks-intersect-wall? empty) #false)
(check-expect (blocks-intersect-wall?
               (list (make-block (sub1 game-width) 2 'green))) #false)
(check-expect (blocks-intersect-wall?
               (list (make-block -1 2 'green))) #true)
(check-expect (blocks-intersect-wall?
               (list (make-block game-width 2 'green))) #true)

;; Does any block intersect floor?
;; blocks-intersect-floor? : BSet -> Boolean
(define (blocks-intersect-floor? bs)
  (ormap (λ (b)
           (>= (block-y b) game-height))
         bs))

(check-expect (blocks-intersect-floor? empty) #false)
(check-expect (blocks-intersect-floor?
               (list (make-block 2 (sub1 game-height) 'green))) #false)
(check-expect (blocks-intersect-floor?
               (list (make-block 2 game-height 'green))) #true)

;; Does any block intersect ceiling?
;; blocks-intersect-ceiling? : BSet -> Boolean
(define (blocks-intersect-ceiling? bs)
  (ormap (λ(b)
           (< (block-y b) 0))
         bs))

(check-expect (blocks-intersect-ceiling? empty) #false)
(check-expect (blocks-intersect-ceiling?
               (list (make-block 2 0 'green))) #false)
(check-expect (blocks-intersect-ceiling?
               (list (make-block 2 -1 'green))) #true)

;; Handles movement to avoid collisions.
;; avoid-collision : World World -> World
(define (avoid-collision current next)
  (if (or (blocks-intersect? (tetra-blocks (world-tetra next))
                             (world-pile next))
          (blocks-intersect-wall? (tetra-blocks (world-tetra next)))
          (blocks-intersect-floor? (tetra-blocks (world-tetra next))))
      current
      next))

(define legal-world1 (make-world (move-tetra o-tetra 3 4)
                                 (list (make-block 5 10 'red))
                                 3))

(define legal-world2 (make-world (move-tetra o-tetra 4 5)
                                 (list (make-block 5 10 'red))
                                 3))

(define tetra-intersect-walll-world (make-world
                                     (move-tetra o-tetra -1 4)
                                     (list (make-block 5 10 'red))
                                     3))

(define tetra-intersect-wallr-world (make-world
                                     (move-tetra o-tetra game-width 4)
                                     (list (make-block 5 10 'red))
                                     3))

(define tetra-intersect-pile-world (make-world
                                     (move-tetra o-tetra 3 4)
                                     (list (make-block 3 4 'red))
                                     3))

(define tetra-intersect-floor-world (make-world
                                     (move-tetra o-tetra 3 game-height)
                                     (list (make-block 5 10 'red))
                                     3))

(check-expect (avoid-collision legal-world1 legal-world2) legal-world2)
(check-expect (avoid-collision legal-world2 legal-world1) legal-world1)
(check-expect (avoid-collision legal-world1 tetra-intersect-walll-world)
              legal-world1)
(check-expect (avoid-collision legal-world1 tetra-intersect-wallr-world)
              legal-world1)
(check-expect (avoid-collision legal-world1 tetra-intersect-pile-world)
              legal-world1)
(check-expect (avoid-collision legal-world1 tetra-intersect-floor-world)
              legal-world1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Full Row Detection/Removal Functions

;; Creates a list of zeros of given length.
;; zero-list : NonNegativeInteger -> LON
(define (zero-list n)
  (build-list n (λ (x) 0)))

(check-expect (zero-list 0) empty)
(check-expect (zero-list 1) (list 0))
(check-expect (zero-list 4) (list 0 0 0 0))

;; Increment array at index.
;; increment-index : LON Number -> LON
(define (increment-index ln i)
  (cond
    [(empty? ln) empty]
    [(= i 0) (cons (add1 (first ln)) (rest ln))]
    [else (cons (first ln) (increment-index (rest ln) (sub1 i)))]))

(check-expect (increment-index (list 0 0 0 0) 0) (list 1 0 0 0))
(check-expect (increment-index (list 2 3 4 5) 2) (list 2 3 5 5))
(check-expect (increment-index empty 0) empty)

;; Increment indices of list based on presence of block at y equal
;; to that index.
;; increment-block-rows : BSet LON -> LON
(define (increment-block-rows bs ln)
  (foldl (λ (b ln-curr)
           (increment-index ln-curr
                            (block-y b)))
         ln bs))

(check-expect (increment-block-rows empty empty) empty)
(check-expect (increment-block-rows test-blocks (list 0 0 0 0))
              (list 1 0 0 1))
(check-expect (increment-block-rows (list (make-block 0 0 'green)
                                          (make-block 2 0 'green))
                                    (list 0 0))
              (list 2 0))

;; Remove all blocks with given y and increase y by one of all
;; blocks with a smaller y.
;; clear-row : Number BSet -> BSet
(define (clear-row y bs)
  (local [(define removed-y
            (filter (λ (b)
                 (not (= (block-y b) y)))
               bs))]
    (map (λ (b)
           (if (< (block-y b) y)
               (make-block (block-x b)
                         (add1 (block-y b))
                         (block-color b))
               b))
         removed-y)))

(check-expect (clear-row 0 empty) empty)
(check-expect (clear-row 2 (list (make-block 0 0 'green)
                                 (make-block 1 1 'green)
                                 (make-block 2 2 'green)
                                 (make-block 3 3 'green)))
              (list (make-block 0 1 'green)
                    (make-block 1 2 'green)
                    (make-block 3 3 'green)))

;; Clears rows corresponding with indices in list with values
;; equal to game width.
;; clear-full-rows : LON BSet -> BSet
(define (clear-rows-array ln bs)
  (foldl (λ (i bs-curr)
           (if (>= (list-ref ln i) game-width)
               (clear-row i bs-curr)
               bs-curr))
         bs (range 0 (length ln) 1)))

(check-expect (clear-rows-array empty empty) empty)
(check-expect (clear-rows-array (list 0 0 game-width 0)
                                (list (make-block 0 0 'green)
                                      (make-block 1 1 'green)
                                      (make-block 2 2 'green)
                                      (make-block 3 3 'green)))
              (list (make-block 0 1 'green)
                    (make-block 1 2 'green)
                    (make-block 3 3 'green)))

;; Clear full rows from world.
;; clear-full-rows : World -> World
(define (clear-full-rows w)
  (make-world (world-tetra w)
              (clear-rows-array (increment-block-rows (world-pile w)
                                                      (zero-list game-height))
                                (world-pile w)) (world-score w)))

(define pile-with-full-row
  (append (list (make-block 0
                            (- game-height 1)
                            'green))
          (build-list game-width (λ (x)
                                   (make-block x
                                               (- game-height 2)
                                               'red)))
          (list (make-block 2
                            (- game-height 3)
                            'blue))))

(define pile-cleared
  (list (make-block 0
                    (- game-height 1)
                    'green)
        (make-block 2
                    (- game-height 2)
                    'blue)))

(check-expect (clear-full-rows (make-world l-tetra
                                           pile-with-full-row
                                           0))
              (make-world l-tetra pile-cleared 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Logic Functions

;; Picks random tetra from list and positions it at top of world.
;; new-tetra : TSet -> Tetra
(define (new-tetra ts)
  (move-tetra (list-ref all-tetra
                        (random (length all-tetra)))
              (- (/ game-width 2) 2) -2))

; Checks resulting tetra is in given list.
(check-expect (member? (move-tetra (new-tetra all-tetra)
                                   (- (- (/ game-width 2) 2))
                                   2)
                       all-tetra) #true)
              
;; Handles collision of tetra with bottom.
;; bottom-collision : World World -> World
(define (handle-bottom current next)
  (if (or (blocks-intersect? (tetra-blocks (world-tetra next))
                             (world-pile next))
          (blocks-intersect-floor? (tetra-blocks (world-tetra next))))
      (make-world (new-tetra all-tetra)
                  (append (tetra-blocks (world-tetra current))
                          (world-pile current)) (add1 (world-score current)))
      next))

(check-expect (handle-bottom legal-world1 legal-world2) legal-world2)

(define floor-collision-world (handle-bottom legal-world1 tetra-intersect-floor-world))

(check-expect (world-pile floor-collision-world)
              (append (tetra-blocks (world-tetra legal-world1))
                      (world-pile legal-world1)))

(check-expect (world-score floor-collision-world)
              (add1 (world-score legal-world1)))

(define pile-collision-world (handle-bottom legal-world1 tetra-intersect-pile-world))

(check-expect (world-pile pile-collision-world)
              (append (tetra-blocks (world-tetra legal-world1))
                      (world-pile legal-world1)))

(check-expect (world-score pile-collision-world)
              (add1 (world-score legal-world1)))
              

;; Descend tetra if it can, otherwise add it to pile and add new tetra.
;; progress-tetra : World -> World
(define (progress-tetra w)
  (handle-bottom w
                 (make-world (move-tetra (world-tetra w)
                                         0 1)
                             (world-pile w)
                             (world-score w))))

;; Creates new world if pile exceeds top of screen.
;; reset-on-loss : World -> World
(define (reset-on-loss w)
  (if (blocks-intersect-ceiling? (world-pile w))
      (make-world (new-tetra all-tetra) empty 0)
      w))

(define pile-intersect-ceiling-world (make-world
                                      (move-tetra o-tetra -1 4)
                                      (list (make-block 1 -1 'red)
                                            (make-block 2 4 'red))
                                      3))

(define reset-world (reset-on-loss pile-intersect-ceiling-world))

(check-expect (world-pile reset-world) '())
(check-expect (world-score reset-world) 0)
(check-expect (reset-on-loss legal-world1) legal-world1)

;; Ticks tetris.
;; tick : World -> World
(define (tick w)
  (reset-on-loss
   (clear-full-rows
    (progress-tetra w))))

(define tick-reset-world (tick pile-intersect-ceiling-world))

(check-expect (world-pile tick-reset-world) '())
(check-expect (world-score tick-reset-world) 0)

(check-expect (tick (make-world l-tetra pile-with-full-row 0))
              (make-world (move-tetra l-tetra 0 1) pile-cleared 0))

;;; Makes copy of world but replaces tetra.
;;; copy-world-with-tetra : World Tetra -> World
(define (copy-world-with-tetra w t)
  (make-world t (world-pile w) (world-score w)))

(check-expect (copy-world-with-tetra legal-world1 o-tetra)
              (make-world o-tetra
                          (world-pile legal-world1)
                          (world-score legal-world1)))

(check-expect (copy-world-with-tetra legal-world2 z-tetra)
              (make-world z-tetra
                          (world-pile legal-world2)
                          (world-score legal-world2)))

;; Handles key press.
;; handle-key : World Key -> World
(define (handle-key w k)
  (local [(define t (world-tetra w))
          (define t-next
            (cond [(key=? k "left")  (move-tetra t -1 0)]
                  [(key=? k "right") (move-tetra t 1 0)]
                  [(key=? k "down")  (move-tetra t 0 1)]
                  [(key=? k "s")     (tetra-rotate-cw t)]
                  [(key=? k "a")     (tetra-rotate-ccw t)]
                  [else              t]))
          (define w-next (copy-world-with-tetra w t-next))]
  (avoid-collision w w-next)))

(check-expect (handle-key legal-world1 "x") legal-world1)
(check-expect (handle-key legal-world1 "left")
              (copy-world-with-tetra legal-world1
                                     (move-tetra (world-tetra legal-world1)
                                                              -1 0)))
(check-expect (handle-key legal-world1 "right")
              (copy-world-with-tetra legal-world1
                                     (move-tetra (world-tetra legal-world1)
                                                              1 0)))
(check-expect (handle-key legal-world1 "down")
              (copy-world-with-tetra legal-world1
                                     (move-tetra (world-tetra legal-world1)
                                                              0 1)))
(check-expect (handle-key legal-world1 "s")
              (copy-world-with-tetra legal-world1
                                     (tetra-rotate-cw
                                      (world-tetra legal-world1))))
(check-expect (handle-key legal-world1 "a")
              (copy-world-with-tetra legal-world1
                                     (tetra-rotate-ccw
                                      (world-tetra legal-world1))))
(check-expect (handle-key tetra-intersect-walll-world "left")
              tetra-intersect-walll-world)
(check-expect (handle-key tetra-intersect-wallr-world "right")
              tetra-intersect-wallr-world)
(check-expect (handle-key tetra-intersect-pile-world "left")
              tetra-intersect-pile-world)

;; Starts playing a Tetris game.
;; Any -> World
(define (play _)
  (big-bang (make-world (new-tetra all-tetra) empty 0)
            [on-tick tick 0.2]
            [on-key  handle-key]
            [to-draw draw]))