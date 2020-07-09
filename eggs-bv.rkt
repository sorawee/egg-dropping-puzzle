;; This version uses bitvector and uninterpreted function to represent
;; the database

#lang rosette

(require "common.rkt")

;; use this to play interactively
#;(play #:num-eggs 2
        #:num-floors 10
        #:gas 4
        #:secret (randomize-secret 10)
        #:strategy interactive-strategy)


;; an example of a valid strategy
#;(play #:num-eggs 2
        #:num-floors 10
        #:gas 4
        #:secret (randomize-secret 10)
        #:strategy hardcode-strategy)

;; given a concrete gas, solve-game/gas determines if
;; the concrete gas is enough or not
;; if it is, it also returns a model showing how to solve
;; the game with the given gas
(define (solve-game/gas num-eggs num-floors gas)
  (define BW (add1 gas))
  ;; a map from a list of results ('g or 'b) to an integer
  ;; note that the list is reversed due to how `cons` cons to the front 
  (define-symbolic* database (~> (bitvector BW) integer?))

  ;; a strategy to be synthesized
  (define (strategy initial-resp)
    (define results (bv 1 BW))
    (λ (result)
      (cond
        [(not result) initial-resp]
        [else (set! results (bvor (bvshl results (bv 1 BW))
                                  (cond
                                    [(equal? result 'g) (bv 1 BW)]
                                    [else (bv 0 BW)])))
              ;; lookup from database
              (database results)])))

  (define (integer->g/b n)
    (let loop ([n n] [acc '()])
      (cond
        [(<= n 1) acc]
        [else (loop (quotient n 2)
                    (cons (match (remainder n 2)
                            [0 'b]
                            [1 'g])
                          acc))])))

  (define (probe-database the-gas)
    (for/list ([i (in-range 1 (expt 2 the-gas))])
      (list (integer->g/b i) (database (bv i BW)))))

  (do-synthesize num-eggs num-floors gas strategy probe-database))

(define solve-game (make-solve-game solve-game/gas))

;; use this to solve the games
#;(solve-game #:num-eggs 2
              #:num-floors 10
              #:upper-bound 10)

(solve-game #:num-eggs 2
            #:num-floors 10
            #:upper-bound 10)
