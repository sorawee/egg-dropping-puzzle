#lang rosette

(provide play randomize-secret interactive-strategy hardcode-strategy
         get-symbolic bin-search make-solve-game do-synthesize)

;; game rules are enforced by `play`
;; NOTE: I changed the rules so that the secret floor is the LAST floor that
;; the egg dropping will NOT be broken, rather than the FIRST floor
;; that the egg dropping will be broken
(define (play #:num-eggs num-eggs
              #:num-floors num-floors
              #:gas gas
              #:secret secret
              #:strategy strategy)
  (let loop ([result #f] [num-eggs num-eggs] [gas gas])
    (define resp (strategy result))
    (cond
      [(= resp secret) 'you-won]
      [(= gas 1) (assert #f "ran out of gas")]
      [(and (> resp secret) (= num-eggs 1)) (assert #f "ran out of eggs")]
      [(> resp secret) (loop 'b (sub1 num-eggs) (sub1 gas))] ; 'b = bad, or broken
      [else (loop 'g num-eggs (sub1 gas))]))) ; 'g = good

(define (randomize-secret num-floors) (add1 (random num-floors)))

;; a strategy consumes a result from `play` which is either
;; - 'g (the previous response is good)
;; - 'b (the previous response is bad)
;; - #f (no previous response)
;; and produces the next guess

(define (interactive-strategy result)
  (printf "result: ~a\n" result)
  (printf "your guess?\n")
  (printf "> ")
  (read))

(define hardcode-strategy
  (let ([results '()])
    (λ (result)
      (cond
        [(not result) 4]
        [else (set! results (cons result results))
              (case results
                [((b)) 1] ; 2
                [((g b)) 2] ; 3
                [((g g b)) 3] ; 4

                [((g)) 7] ; 2
                [((b g)) 5] ; 3
                [((g b g)) 6] ; 4

                [((g g)) 9] ; 3
                [((b g g)) 8] ; 4

                [((g g g)) 10] ; 4
                )]))))

(define (get-symbolic)
  (define-symbolic* x integer?)
  x)

;; binary search for gas
(define (bin-search left-gas right-gas do-solve [answer #f])
  (printf "binary search with param ~a ~a\n" left-gas right-gas)
  (cond
    [(> left-gas right-gas) answer]
    [else
     (define mid-gas (quotient (+ left-gas right-gas) 2))
     (match (do-solve mid-gas)
       ;; not enough gas
       [#f (bin-search (add1 mid-gas) right-gas do-solve answer)]
       ;; potentially too much gas
       [result (bin-search left-gas (sub1 mid-gas) do-solve
                           (cons mid-gas result))])]))

;; find the least amount of required gas
(define ((make-solve-game solve-game/gas)
         #:num-eggs num-eggs
         #:num-floors num-floors
         #:upper-bound upper-bound)
  (match-define (list the-gas the-mod the-initial-resp probe-database)
    (bin-search 1 upper-bound (λ (gas) (solve-game/gas num-eggs num-floors gas))))
  (printf "required gas: ~a\n" the-gas)
  (printf "initial resp ~a\n" (pretty-format (evaluate the-initial-resp the-mod)))
  (printf "database: ~a\n" (pretty-format (evaluate (probe-database the-gas) the-mod))))

(define (do-synthesize num-eggs num-floors gas rosette-strategy database)
  (define initial-resp (get-symbolic))
  (define secret (get-symbolic))
  (assert (<= 1 secret num-floors))

  (define mod
    (synthesize #:forall secret
                #:guarantee (play #:num-eggs num-eggs
                                  #:num-floors num-floors
                                  #:gas gas
                                  #:secret secret
                                  #:strategy (rosette-strategy initial-resp))))

  (and (sat? mod) (list mod initial-resp database)))
