(import (prefix sdl2 "sdl2:")
        (prefix sdl2-ttf "ttf:")
        )

(import (chicken format)
        (chicken memory representation)
        (chicken process-context)
        (chicken random)
        (chicken time)
        (chicken tcp)
        (chicken io)
        (srfi 13)
        (srfi 1)
        vector-lib
        )


(sdl2:set-main-ready!)
(sdl2:init! '(video))
(ttf:init!)

(on-exit sdl2:quit!)

(define TILE-SIZE 90)
(define TILE-DIST 5)

(define window
  (sdl2:create-window! "wordle" 100 100 (+ TILE-SIZE (* 2 480)) (+ TILE-SIZE 575)))
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

(define tile-font (ttf:open-font (string-append loc-prefix "tnr.ttf") 60))
(define text-font (ttf:open-font (string-append loc-prefix "tnr.ttf") 30))

(define WHITE (sdl2:make-color 200 200 200))
(define BLACK (sdl2:make-color 0 0 0))
(define GREEN (sdl2:make-color 0 255 0))
(define GREY (sdl2:make-color 140 140 140))
(define DARK-GREY (sdl2:make-color 100 100 100))
(define YELLOW (sdl2:make-color 255 255 0))

(define COLORS
  `((WHITE . ,WHITE)
    (BLACK . ,BLACK)
    (GREEN . ,GREEN)
    (GREY . ,GREY)
    (DARK-GREY . ,DARK-GREY)
    (YELLOW . ,YELLOW)
    )
)

(define make-point cons)
(define point-x car)
(define point-y cdr)

(define make-tile cons)
(define tile-c car)
(define tile-color cdr)


(define text-cache (list))
(define (create-text font s color)
  (let ((res (alist-ref (list font s color) text-cache equal?)))
    (if res
      res

    (begin
      (let* ((surface (ttf:render-utf8-blended font s color))
            (texture (sdl2:create-texture-from-surface renderer surface)))
        (set! text-cache (cons (cons (list font s color) (cons surface texture))
                               text-cache))
        (cons surface texture)
        )
    ))
      )

    )

(define (draw-text! p s) 
  (let* ((temp (create-text tile-font s WHITE))
         (text-surf (car temp))
         (x (point-x p))
         (y (point-y p))
         (r (sdl2:make-rect x y TILE-SIZE TILE-SIZE))
         (w (sdl2:surface-w text-surf))
         (h (sdl2:surface-h text-surf))
         )
        (sdl2:render-copy! renderer
                           (cdr temp)
                           #f
                           (sdl2:make-rect x y w h))

        )
    'noop
  )

(define (draw-tile! p tile)
  (let* ((s (if (equal? (tile-c tile) #\null)
              "dont-care"
              (list->string (list (tile-c tile)))))
         (temp (create-text tile-font (string-upcase s) BLACK))
         (text-surf (car temp))
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
                           (cdr temp)
                           #f
                           (sdl2:make-rect (+ x (quotient (- TILE-SIZE w) 2))
                                           (+ y (quotient (- TILE-SIZE h) 2))
                                           w h))
      )
        )
    'noop
  )


(define past-grid (list))

(define input-string "")

(define goal
  (symbol->string (list-ref dict-la (pseudo-random-integer (length dict-la)))))

(define (lookup-colors v)
  (vector-map (lambda (_ p)
                (cons (car p) (alist-ref (cdr p) COLORS)))
              v))

(define (check guess)
  (write/flush (list 'guess guess) out)
  (lookup-colors (second (read-newline inp)))
  )

(define quit #f)

(define (end-game! dl-ms)
  ; (sdl2:delay! dl-ms)
  (set! quit dl-ms)
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
           ; (display sym)
           ; (newline)
           (if (equal? sym 'escape)
             (begin (end-game! 0) (cont 0)))
           (if (not opp-joined)
             (input-loop! cont))
           (case sym
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
                 (set! past-grid (append past-grid (list (check input-string))))
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


(define (render-grid! px py grid null-color)
  (set! grid (apply vector grid))
  (do
    ((y 0 (+ y 1)))
    ((>= y 6))
    (do
      ((x 0 (+ x 1)))
      ((>= x 5))
      (let ((p (make-point (+ px TILE-DIST (* (+ TILE-DIST TILE-SIZE) x))
                           (+ py TILE-DIST (* (+ TILE-DIST TILE-SIZE) y)))))
        (if (and (< y (vector-length grid))
                 (< x (vector-length (vector-ref grid y)))
                 )
          (draw-tile! p (vector-ref (vector-ref grid y) x))
          (draw-tile! p (make-tile #\null null-color))
        )
      )
      )))

(define (game-done?)
  (or (> (length past-grid) 5)
      (and (> (length past-grid) 0) 
           (equal? (list->string (map tile-c (vector->list (last past-grid)))) goal))))

(define (write/flush datum port)
  (write datum port)
  (newline port)
  )



(define (read-newline port)
  ;; flushing doesn't do anything for tcp?? newline flushes tho
  ;; so need to read it off to empty port
  (let ((res (read port)))
    (read-char port)
    res
    )
  )

(define cla (drop (command-line-arguments) 2))


(define server (first cla))
(define me (second cla))
(define opp (third cla))

(display "trying to connect to: ")
(display server)
(newline)

(define-values (inp out)
  (tcp-connect server 8123))

(display "connection successful")
(newline)

(define identifier (read-newline inp))
(define opp-name "Waiting...")
(define opp-joined #f)






(write/flush (list (string->symbol me) (string->symbol opp)) out)

; (display identifier)
; (newline)
; (display (read inp))
; (newline)

(define RENDERS-PER-SECOND 30)
(define MS-PER-RENDER (round (/ 1000 RENDERS-PER-SECOND)))

;; increases with difficulty
(define renders-per-tick RENDERS-PER-SECOND)
(define render-count 1)
(define prev-render-time (current-process-milliseconds))

(define (with-control proc)
  (define res (call/cc (lambda (cont) (proc cont))))
  (cond
    ((equal? res "gg") (sdl2:quit!) (exit))))


(define opp-grid (list))

(define (respond l)
  ; (display l)
  ; (newline)
  ; (define vals (apply values l))
  (case (car l)
    ((joined)
     ; (let-values (((_ name) vals))
       (set! opp-joined #t)
       (set! opp-name (symbol->string (second l)))

       ; )
     )
    ((opp-update)
     (set! opp-grid (append opp-grid (list (lookup-colors (second l)))))
     )
    ((opp-lost)
     ; (set! opp-grid (map lookup-colors (alist-ref 'opp-board (second l))))
     'noop
     )
    ((opp-won)
     ; (set! opp-grid (map lookup-colors (alist-ref 'opp-board (second l))))
     ; (display "yours was: ")
     ; (newline)
     'noop
     )
    ((you-lost you-won)
     ; (set! opp-grid (map lookup-colors (alist-ref 'opp-board (second l))))
     (display "your word was: ")
     (display (alist-ref 'your-word (cdr l)))
     (newline)
     (display "opponents word was: ")
     (display (alist-ref 'opp-word (cdr l)))
     (newline)
     )
    ((both-done)
     (set! opp-grid (map lookup-colors (alist-ref 'opp-board (cdr l))))
     ; (end-game! 3000)


     )
    )

  )


(define (calc-score grid)
  (let* ((grid (append grid (list #())))
         (levels
          (map (lambda (row multi)
                 (* multi 
                    (count (lambda (p) (eq? (cdr p) GREEN))
                           (vector->list row))))
               grid
               (list 20 20 13 10 7 5)
               ))
         (total (foldl + 0 levels)))
    (if (every (lambda (p) (eq? (cdr p) GREEN))
               (vector->list (last grid)))
      (+ total (* 10 (last levels)))
      total)

  ))

(define (main)
  ; make frame length uniform
  (sdl2:delay! (- MS-PER-RENDER
                  (modulo (- (current-process-milliseconds) prev-render-time)
                          MS-PER-RENDER)))
  (set! prev-render-time (current-process-milliseconds))

  (draw-text! (cons 0 0) me)
  (draw-text! (cons (+ 480 TILE-SIZE) 0) opp-name)

  (draw-text! (cons (- 480 100) 0)
              (number->string (calc-score past-grid))
              )

  (draw-text! (cons (+ TILE-SIZE (* 2 480) -100) 0)
              (number->string (calc-score opp-grid))
              )

  (with-control input-loop!)

  (write/flush (list 'heartbeat) out)

  (if (char-ready? inp)
    (let ((res (read-newline inp)))
      (if (eof-object? res)
        (begin
          (display "opponent dced. win by forfeit?")
          (newline)
          (end-game! 1000)
          )
        (respond res)

        )

      )
    )

  (render-grid! 0
                TILE-SIZE 
                (append past-grid 
                        (list (apply vector (map (lambda (c) (make-tile c WHITE)) (string->list input-string)))))
                WHITE)


  (render-grid! (+ 480 TILE-SIZE) 
                TILE-SIZE
                opp-grid
                (if opp-joined WHITE DARK-GREY)
                )
  (sdl2:render-present! renderer)

  (sdl2:render-draw-color-set! renderer (sdl2:make-color 0 0 0))
  (sdl2:render-clear! renderer)

  (if (not quit)
    (main))
  )

(main)
(sdl2:delay! quit)
(sdl2:quit!)
