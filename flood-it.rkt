#lang racket

(require
  2htdp/image
  2htdp/universe)

(define TITLE "Flood-It!")
(define SIZE 19)
(define INFO-SIZE 16)
(define NUM-COLORS 6)
(define SIDE 20)
(define INFO-COLOR "dimgray")

(define count 0)

(define (get-color c)
  (case c
    [(0) "red"]
    [(1) "green"]
    [(2) "blue"]
    [(3) "yellow"]
    [(4) "pink"]
    [(5) "cyan"]))

(define (generate-line n)
  (cond
    [(zero? n) empty]
    [else (cons (random NUM-COLORS) (generate-line (sub1 n)))]))

(define (generate-world n)
  (cond
    [(zero? n) empty]
    [else (cons (generate-line SIZE) (generate-world (sub1 n)))]))

(define *last-state* null)
(define *last-grid* null)

(define (make-tile c)
  (square SIDE "solid" (get-color c)))

(define (place-tile tile i j bg)
  (define (pos k) (* k SIDE))
  (underlay/xy bg (pos j) (pos i) tile))

(define (get/xy x y state)
  (list-ref (list-ref state x) y))

(define (change/xy x y state t)
  (for/list ([row state] [i (length state)])
    (for/list ([e row] [j (length row)])
      (if (and (= x i) (= y j)) t e))))

(define (state->image state)
  (unless (equal? state *last-state*)
    (set! *last-grid*
          (for*/fold ([bg (square (* SIZE SIDE) "solid" "white")])
                     ([i SIZE] [j SIZE])
            (place-tile (make-tile (get/xy i j state)) i j bg)))
    (set! *last-state* state))
  *last-grid*)

(define (draw-world state)
  (let* ([won? (single-color? state)]
         [board (state->image state)]
         [score-info (text (format "Total: ~a" count) INFO-SIZE INFO-COLOR)]
         [won-info (text "You Won!" INFO-SIZE INFO-COLOR)]
         [help-info (beside (text "1:" INFO-SIZE INFO-COLOR)
                            (square SIDE "solid" (get-color 0))
                            (text "2:" INFO-SIZE INFO-COLOR)
                            (square SIDE "solid" (get-color 1))
                            (text "3:" INFO-SIZE INFO-COLOR)
                            (square SIDE "solid" (get-color 2))
                            (text "4:" INFO-SIZE INFO-COLOR)
                            (square SIDE "solid" (get-color 3))
                            (text "5:" INFO-SIZE INFO-COLOR)
                            (square SIDE "solid" (get-color 4))
                            (text "6:" INFO-SIZE INFO-COLOR)
                            (square SIDE "solid" (get-color 5)))])
(above board
       (beside score-info
               (if won? won-info empty-image)
               (rectangle (- (image-width board)
                             (image-width score-info)
                             (if won? (image-width won-info) 0)
                             (image-width help-info)) 0 "solid" "white")
               help-info))))

(define (first-color state) (first (first state)))

(define (single-color? state)
  (for*/and ([i SIZE] [j SIZE])
    (= (get/xy i j state) (first-color state))))

(define (flood state t)
  (define s (first-color state))
  (cond
    [(= t s) state]
    [else
     (set! count (add1 count))
     (define (flood/xy x y)
       (when (= s (get/xy x y state))
         (set! state (change/xy x y state t))
         (unless (= SIZE (add1 x)) (flood/xy (add1 x) y))
         (unless (= SIZE (add1 y)) (flood/xy x (add1 y)))
         (unless (zero? x) (flood/xy (sub1 x) y))
         (unless (zero? y) (flood/xy x (sub1 y)))))
     (flood/xy 0 0)
     state]))

(define (change state key)
  (cond
    [(key=? key "1") (flood state 0)]
    [(key=? key "2") (flood state 1)]
    [(key=? key "3") (flood state 2)]
    [(key=? key "4") (flood state 3)]
    [(key=? key "5") (flood state 4)]
    [(key=? key "6") (flood state 5)]
    [else state]))

(define (start initial-state)
  (big-bang initial-state
    (to-draw draw-world)
    (on-key change)
    (stop-when single-color? draw-world)
    (name TITLE)))

(start (generate-world SIZE))