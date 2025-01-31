(import (chicken tcp)
        (srfi 18)
        (srfi 1)
        (srfi 13)
        vector-lib
        (chicken random)
        (chicken process-context)
        (chicken port))


; (set! tcp-read-timeout #f)

;; screw r/w, just treat all as w for now
(define players-mutex (make-mutex 'players))
(define games-mutex (make-mutex 'games))
(define awaiting-mutex (make-mutex 'awaiting-mutex))

(define-syntax crit-sec
  (syntax-rules ()
    ((_ (mutexes ...) code ...)
     (begin
       (for-each mutex-lock! (list mutexes ...))
       (let ((res (begin code ...))
           )
       (for-each mutex-unlock! (reverse (list mutexes ...)))
       res
     ))

     )))

(define players (list))
(define games (list))
(define awaiting (list))

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

(define make-tile cons)

(define (check guess goal)
  (define wrong (list))
  (define missing-goal (list))
  (define color-vec (make-vector 5))

  (do
    ((i 0 (+ i 1)))
    ((= i 5))
    (if (equal? (string-ref guess i) (string-ref goal i))
      (vector-set! color-vec i 'GREEN)
      (begin 
        (set! wrong (append wrong (list i)))
        (set! missing-goal (append missing-goal (list (string-ref goal i))))
        )
      )
    )
  (for-each
    (lambda (i)
      (if (member (string-ref guess i) missing-goal)
        (begin (delete! (string-ref guess i) missing-goal)
               (vector-set! color-vec i 'YELLOW))
        (vector-set! color-vec i 'GREY)
        )
      )
    wrong)

  (apply vector (map make-tile (string->list guess) (vector->list color-vec)))
  )

(define listener
  (tcp-listen 8123))



(define (make-player handle opponent-handle game-uid i o)
  (list handle opponent-handle game-uid i o)
  )

(define player-o last)
(define player-game-uid caddr)

(define (other-player p board)
  (car (delete p (list (car (first board)) (car (second board))))))

(define (guess game-uid p word)
  (if (memq (string->symbol word) dict-total)
    (let* ((boards (alist-ref game-uid games))
           (player (assoc p players))
           (board (alist-ref p boards))
           (goal (string-upcase (first board)))
           (check-res (check (string-upcase word) goal))
           (new-board (append (second board) (list check-res)))
           (other (other-player p boards))
           (p-done
             (or (and (> (length (second board)) 0)
                      (equal? (list->string
                                (map car (vector->list (last (second board)))))
                              (first board)))
                 (> (length (second board)) 5)))
           (other-board (alist-ref other boards))
           (other-done 
             (or (and (> (length (second other-board)) 0)
                      (equal? (list->string
                                (map car (vector->list (last (second other-board)))))
                              (first other-board)))
                 (> (length (second other-board)) 5)))
           )

      (if (not p-done)
        (begin
          (set-cdr! board (list new-board))
          (write/flush
            (list 'guess-result check-res)
            (player-o (assoc p players)))
          (write/flush
            (list 'opp-update 
                  (vector-map (lambda (_ pair)
                                (cons #\null (cdr pair))
                                )
                              check-res))

            (player-o (assoc other players)))
              )
        )

      (cond
        ((and (or p-done (equal? (string-upcase word) goal)) other-done)
         ;; game ended
          (write/flush
            (list 'both-done
                  `(your-word . ,(first (alist-ref p boards)))
                  `(opp-word . ,(first (alist-ref other boards)))
                  `(opp-board . ,(second (alist-ref other boards)))
                  )
            (player-o player)
            )

          (write/flush
            (list 'both-done
                  `(your-word . ,(first (alist-ref other boards)))
                  `(opp-word . ,(first (alist-ref p boards)))
                  `(opp-board . ,(second (alist-ref p boards)))
                  )
            (player-o (assoc other players))
            )
          ; (set! players (delete player (delete other players)))
          ; (write #!eof (player-o player))
          ; (write #!eof (player-o (assoc other players)))

         )
    ((equal? (string-upcase word) goal)
     ;; notify winner (p)
     (write/flush
       (list 'you-won
             `(your-word . ,(first (alist-ref p boards)))
             `(opp-word . ,(first (alist-ref other boards)))
             `(opp-board . ,(second (alist-ref other boards)))
             )
       (player-o player)
       )
     ;; notify opponent of players board
     (write/flush
       (list 'opp-won
             ; `(your-word . ,(first (alist-ref other boards)))
             `(opp-word . ,(first (alist-ref p boards)))
             `(opp-board . ,(second (alist-ref p boards)))
             )
       (player-o (assoc other players))
       )
     )
    ((> (length new-board) 5)
     ;; notify loser (p)
     (write/flush
       (list 'you-lost
             `(your-word . ,(first (alist-ref p boards)))
             `(opp-word . ,(first (alist-ref other boards)))
             `(opp-board . ,(second (alist-ref other boards)))
             )
       (player-o player)
       )
     ;; notify opponent of players board
     (write/flush
       (list 'opp-lost
             ; `(your-word . ,(first (alist-ref other boards)))
             `(opp-word . ,(first (alist-ref p boards)))
             `(opp-board . ,(second (alist-ref p boards)))
             )
       (player-o (assoc other players))
       )
     )
     
    )

      )
  )
  )

(define (write/flush datum port)
  (write datum port)
  (newline port)
  )


(define (gen-goal)
  (string-upcase
    (symbol->string (list-ref dict-la (pseudo-random-integer (length dict-la))))))

(define (uid-dispatch player i o res)
  (define body (cdr res))
  (case (car res)
    ((guess)
     ; (display player)
     ; (newline)
     (guess (player-game-uid player) (car player) (car body))

     )
    )
  )

(define (make-game uid p1 p2)
  ; (list uid (list p1 (gen-goal) (list)) (list p2 (gen-goal) (list))))
  (list uid (list p1 "BEERS" (list)) (list p2 "BEERS" (list))))

(define (start-game! p1 p2)
  (let ((uid (gensym)))
    (set! games (append games (list (make-game uid p1 p2))))
    (set-car! (cddr (assoc p1 players))
      uid)
    (set-car! (cddr (assoc p2 players))
      uid)

    (write/flush
      (list 'joined p2)
      (player-o (assoc p1 players)))
    (write/flush
      (list 'joined p1)
      (player-o (assoc p2 players)))

    )
  )


(define (session-thunk i o)
  (define uid (gensym))
  (write/flush uid o)
  (define (loop player)
       (let ((res (read i)))
        (if (not (eof-object? res))
          (begin
            ; (display res)
            ; (newline)
            (crit-sec (players-mutex awaiting-mutex games-mutex)
              (uid-dispatch player i o res))
            (loop player))
          (crit-sec (players-mutex awaiting-mutex games-mutex)
            (set! players (alist-delete (car player) players))
            (set! awaiting (alist-delete (car player) awaiting))
            (let ((game (alist-ref (player-game-uid player) games)))
              (if game
                (write/flush #!eof
                             (player-o (assoc (other-player (car player) game) players)))
                )

              )
            (set! games (alist-delete (player-game-uid player) games))
            (display "player dc: ")
            (display player)
         (newline)
          (display players)
          (newline)
          (display awaiting)
          (newline)
          )

            )
            )
        
        )
  (lambda ()
    (let ((info (read i))
          (player #f))
      (crit-sec (players-mutex awaiting-mutex games-mutex)
        (set! player (apply make-player (append info (list 0 i o))))
        (set! players (append players (list player)))
        (if (member (reverse info) awaiting)
          (begin (apply start-game! info)
                 (set! awaiting (alist-delete (second info) awaiting))
                 )
          (set! awaiting (append awaiting (list info)))
         )
     (display "player join: ")
     (display player)
     (newline)
      (display players)
      (newline)
      (display awaiting)
      (newline))
      (loop player)
    ))
  )

(define (main)
  (display "waiting")
  (newline)
  (let-values (((i o) (tcp-accept listener)))
    (thread-start! (session-thunk i o))
    (main)
    )
  )

(main)
