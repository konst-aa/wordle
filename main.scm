(import (prefix sdl2 "sdl2:")
        (prefix sdl2-ttf "ttf:")
        )

(import (chicken format)
        (chicken memory representation)
        (chicken process-context)
        (chicken random)
        (chicken time)
        (srfi 13)
        (srfi 1)
        )

(sdl2:set-main-ready!)
(sdl2:init! '(video))
(ttf:init!)

(on-exit sdl2:quit!)

(define window (sdl2:create-window! "wordle" 100 100 480 575))
(define renderer (sdl2:create-renderer! window))
(define main-event (sdl2:make-event))

;; from reference https://gitlab.com/chicken-sdl2/chicken-sdl2-examples/-/blob/master/eggsweeper/eggsweeper.scm
;; Disable various irrelevant event types, to avoid wasted time and
;; memory garbage from handling them.
(set! (sdl2:event-state 'text-editing) #f)
(set! (sdl2:event-state 'text-input) #f)
(set! (sdl2:event-state 'mouse-wheel) #f)
(set! (sdl2:event-state 'finger-down) #f)
(set! (sdl2:event-state 'finger-up) #f)
(set! (sdl2:event-state 'finger-motion) #f)
(set! (sdl2:event-state 'multi-gesture) #f)
(set! (sdl2:event-state 'mouse-button-up) #f)
(set! (sdl2:event-state 'mouse-button-down) #f)
(set! (sdl2:event-state 'mouse-motion) #f)


(define loc-prefix
  (let ((v (get-environment-variable "WORDLE_ETC")))
    (if v
      v
      ""
      )
  ))


;; dicts have symbols, not strings
(define dict-la 
  (read (open-input-file (string-append loc-prefix "dict-la.scm"))))

(define dict-ta 
  (read (open-input-file (string-append loc-prefix "dict-ta.scm"))))

(define dict-total (append dict-la dict-ta))

(define font (ttf:open-font (string-append loc-prefix "tnr.ttf") 60))

(define WHITE (sdl2:make-color 200 200 200))
(define BLACK (sdl2:make-color 0 0 0))
(define GREEN (sdl2:make-color 0 255 0))
(define GREY (sdl2:make-color 140 140 140))
(define YELLOW (sdl2:make-color 255 255 0))

(define TILE-SIZE 90)
(define TILE-DIST 5)

(define make-point cons)
(define point-x car)
(define point-y cdr)

(define make-tile cons)
(define tile-c car)
(define tile-color cdr)


(define (draw-tile! p tile)
  (let* ((s (if (equal? (tile-c tile) #\null)
              "dont-care"
              (list->string (list (tile-c tile)))))
         (text-surf (ttf:render-utf8-blended font (string-upcase s) BLACK))
         (x (point-x p))
         (y (point-y p))
         (r (sdl2:make-rect x y TILE-SIZE TILE-SIZE))
         (w (sdl2:surface-w text-surf))
         (h (sdl2:surface-h text-surf))
         )
    (sdl2:render-draw-color-set! renderer (tile-color tile))
    (sdl2:render-fill-rect! renderer r)
    (if (not (equal? (tile-c tile) #\null))
        (sdl2:render-copy! renderer
                           (sdl2:create-texture-from-surface renderer text-surf)
                           #f
                           (sdl2:make-rect (+ x (quotient (- TILE-SIZE w) 2))
                                           (+ y (quotient (- TILE-SIZE h) 2))
                                           w h))
      )
        )
  )


(define past-grid (list))

(define input-string "")

(define goal
  (symbol->string (list-ref dict-la (pseudo-random-integer (length dict-la)))))

(define (check guess goal)
  (define wrong (list))
  (define missing-goal (list))
  (define color-vec (make-vector 5))

  (do
    ((i 0 (+ i 1)))
    ((= i 5))
    (if (equal? (string-ref guess i) (string-ref goal i))
      (vector-set! color-vec i GREEN)
      (begin 
        (set! wrong (append wrong (list i)))
        (set! missing-goal (append missing-goal (list (string-ref goal i))))
        )
      )
    )
  (for-each
    (lambda (i)
      (if (member (string-ref guess i) missing-goal)
        (begin (set! missing-goal (delete (string-ref guess i) missing-goal))
               (vector-set! color-vec i YELLOW))
        (vector-set! color-vec i GREY)
        )
      )
    wrong)

  (apply vector (map make-tile (string->list guess) (vector->list color-vec)))
  )

(define (end-game! dl-ms)
  (sdl2:delay! dl-ms)
  (sdl2:quit!)
  (exit)
)

(define (input-loop! cont)
  (define ev (sdl2:poll-event! main-event))
  (if (not ev)
    (cont 0))

  (case (sdl2:event-type ev)
        ;;((window)
        ;; (sdl2:update-window-surface! window))

        ;; User requested app quit (e.g. clicked the close button).
        ((quit)
         (sdl2:quit!)
         (exit))

        ;; Keyboard key pressed
        ((key-down)
         (let ((sym (sdl2:keyboard-event-sym ev)))
           (case sym
             ((escape)
             (end-game! 0))

            ((backspace)
             (let ((l (string-length input-string)))
               (if (> l 0)
                 (set! input-string (substring input-string 0 (- l 1)))
                 )
               )
            )
            ((return)
             (if (memq (string->symbol input-string) dict-total)
               (begin
                 (set! past-grid (append past-grid (list (check input-string goal))))
                 (set! input-string "")
                 )
               )
             )
            )
           (if (and (member (symbol->string sym)
                            (map (compose list->string list)
                                 (string->list "abcdefghijklmnopqrstuvwxyz")))
                    (< (string-length input-string) 5))
             (set! input-string
               (string-append input-string (symbol->string sym)))

             )


           )
         ))

  (input-loop! cont))

(define RENDERS-PER-SECOND 30)
(define MS-PER-RENDER (round (/ 1000 RENDERS-PER-SECOND)))

(define score 0)

;; increases with difficulty
(define renders-per-tick RENDERS-PER-SECOND)
(define render-count 1)
(define prev-render-time (current-process-milliseconds))

(define (with-control proc)
  (define res (call/cc (lambda (cont) (proc cont))))
  (cond
    ((equal? res "gg") (sdl2:quit!) (exit))))

(define prev-score-quotient 0)


(define (render-grid! grid)
  (set! grid (apply vector grid))
  (do
    ((y 0 (+ y 1)))
    ((>= y 6))
    (do
      ((x 0 (+ x 1)))
      ((>= x 5))
      (let ((p (make-point (+ TILE-DIST (* (+ TILE-DIST TILE-SIZE) x))
                           (+ TILE-DIST (* (+ TILE-DIST TILE-SIZE) y)))))
        (if (and (< y (vector-length grid))
                 (< x (vector-length (vector-ref grid y)))
                 )
          (draw-tile! p (vector-ref (vector-ref grid y) x))
          (draw-tile! p (make-tile #\null WHITE))
        )
      )
      )))

(define (game-done?)
  (or (> (length past-grid) 5)
      (and (> (length past-grid) 0) 
           (equal? (list->string (map tile-c (vector->list (last past-grid)))) goal))))


(define (main)
  ; make frame length uniform
  (sdl2:delay! (- MS-PER-RENDER
                  (modulo (- (current-process-milliseconds) prev-render-time)
                          MS-PER-RENDER)))
  (set! prev-render-time (current-process-milliseconds))

  (with-control input-loop!) ;; respond to input
  (render-grid! (append past-grid (list (apply vector (map (lambda (c) (make-tile c WHITE)) (string->list input-string))))))
  (sdl2:render-present! renderer)
  (cond
    ((> (length past-grid) 5)
     (display "Damn. Word was: ")
     (display goal)
     (newline)
     (end-game! 2000)

     )
    ((and (> (length past-grid) 0)
          (equal? (list->string (map tile-c (vector->list (last past-grid)))) goal))
     (display "good job")
     (newline)
     (end-game! 2000)
     ))

  (sdl2:render-draw-color-set! renderer (sdl2:make-color 0 0 0))
  (sdl2:render-clear! renderer)

  (main))

(main)
(sdl2:quit!)
