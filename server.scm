(import (chicken tcp)
        (chicken base)
        (srfi 18)
        (srfi 1)
        (srfi 13)
        (srfi 12)
        vector-lib
        (chicken random)
        (chicken process-context)
        (chicken port))


; (set! tcp-read-timeout #f)

;; screw r/w, just treat all as w for now
(define players-mutex (make-mutex 'players))
(define games-mutex (make-mutex 'games))
(define awaiting-mutex (make-mutex 'awaiting-mutex))

(define (print . args)
  (for-each display args)
  (newline)
  )

(define-syntax anif
  (syntax-rules (:=)
    ((_ (sym := bool) x ...)
     (let ((sym bool))
       (if sym x ...))
     )))

(define-syntax crit-sec
  (syntax-rules ()
    ((_ (mutexes ...) code ...)
     (let ((exn #f)
           (escape #f))
       (call/cc 
         (lambda (cont) (set! escape cont)))

       (if exn (raise exn))

       (with-exception-handler
         (lambda (e)
           (set! exn e)
           (escape e))

         (lambda ()
           (dynamic-wind
               (lambda () 
                 (for-each mutex-lock! (list mutexes ...)))
               (lambda () code ...)
               (lambda ()
                 (for-each mutex-unlock! (reverse (list mutexes ...))))))
       )
     )
    )
    )
    )


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
        (begin (set! missing-goal (delete (string-ref guess i) missing-goal))
               (vector-set! color-vec i 'YELLOW))
        (vector-set! color-vec i 'GREY)
        )
      )
    wrong)

  (apply vector (map make-tile (string->list guess) (vector->list color-vec)))
  )

(define listener
  (tcp-listen 8123))



(define (make-player handle opp-handle game-uid i o guess-t)
  (list handle opp-handle game-uid i o guess-t)
  )

(define player-opp-handle second)
(define player-o fifth)
(define player-game-uid third)
(define player-guess-t sixth)
(define (set-player-guess-t! player t)
  (set-car! (cdr (cddddr player)) t)
  )

(define (other-player player)
  (assoc (player-opp-handle player) players))

(define (guess game-uid p word)
  (if (memq (string->symbol word) dict-total)
    (let* ((boards (alist-ref game-uid games))
           (player (assoc p players))
           (board (alist-ref p boards))
           (goal (string-upcase (first board)))
           (check-res (check (string-upcase word) goal))
           (new-board (append (second board) (list check-res)))
           (other (car (other-player player)))
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
         (both-done! player)
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
     (set-player-guess-t! player (current-secs))
     (guess (player-game-uid player) (car player) (car body))

     )
    )
  )

(define (make-game uid p1 p2)
  (list uid
        (list p1 (string-upcase (gen-goal)) (list))
        (list p2 (string-upcase (gen-goal)) (list))))
  ; (list uid (list p1 "BEERS" (list)) (list p2 "BEERS" (list))))

(define (start-game! p1 p2)
  (let ((uid (gensym)))
    (set! games (append games (list (make-game uid p1 p2))))
    (set-car! (cddr (assoc p1 players))
      uid)
    (set-car! (cddr (assoc p2 players))
      uid)

    (let ((t (current-secs)))
      (set-player-guess-t! (assoc p1 players) t)
      (set-player-guess-t! (assoc p2 players) t)
      )

    (write/flush
      (list 'joined p2)
      (player-o (assoc p1 players)))
    (write/flush
      (list 'joined p1)
      (player-o (assoc p2 players)))

    )
  )


(define (remove-player! player)
  (set! players (alist-delete (car player) players))
  (set! awaiting (alist-delete (car player) awaiting))
  (set! games (alist-delete (player-game-uid player) games))
  )

(define (session-thunk i o)
  (define uid (gensym))
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
              (remove-player! player)
              (anif (opp := (other-player player))
                (terminate (list 'opp-dced 'eof) (player-o opp))

                )
            (print "player dc: " player)

              )
            )
        )
        
        )
  (lambda ()
    (let ((info (read i))
          (player #f))
      (print "attempting to join: " info)
      (crit-sec (players-mutex awaiting-mutex games-mutex)
        (if (not (assoc (first info) players))
          (begin
            (write/flush (list 'connection-successful uid) o)
            (set! player (apply make-player (append info (list 0 i o 0))))
            (set! players (append players (list player)))
            (if (member (reverse info) awaiting)
              (begin (apply start-game! info)
                     (set! awaiting (alist-delete (second info) awaiting))
                     )
              (set! awaiting (append awaiting (list info))))
         (print "player join: " player "\nplayers:\n" players "\nawaiting:\n" awaiting)
          )
        )

          )
      (if player
        (loop player)
        (crit-sec
          (players-mutex awaiting-mutex games-mutex)
          (terminate (list 'name-taken) o)))

    ))
  )

(define (current-secs)
  (inexact->exact (time->seconds (current-time)))
  )

(define (terminate msg port)
  (write/flush
    msg
    port)
  (write/flush
    #!eof
    port)

  )
(define (notify-dc player reason)
  (terminate  (list 'dc reason) (player-o player))

  (anif (other := (other-player player))
    (terminate  (list 'opp-dc reason) (player-o other))

  )
  )


(define (both-done! player)
      (let ((p (car player))
            (other (player-opp-handle player)))
        (anif (boards := (alist-ref (player-game-uid player) games))
              (begin
                (print "ending game: " games)

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
                )
              )
        (print "removing: " player)

      (remove-player! player)
      (anif (other := (assoc other players))
        (remove-player! other))
        )

  )

(define (reaper)
    (crit-sec (players-mutex awaiting-mutex games-mutex)
      (let ((t (current-secs)))
        (for-each 
          (lambda (player)
            (if (and (not (zero? (player-guess-t player)))
                     (> (- t (player-guess-t player)) 60))
              (begin
              (both-done! player)
              (notify-dc player 'timeout)

                )
              )
            )
          players
          )
        )
    )
  (thread-sleep! 0.5)
    (reaper)
)

(define (main)
  (print "server started")
  (let-values (((i o) (tcp-accept listener)))
    (thread-start! (session-thunk i o))
    (main)
    )
  )

(thread-start! reaper)
(main)
