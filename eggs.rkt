;; This version uses an explicit association list to represent the database

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
  ;; a map from a list of results ('g or 'b) to an integer
  ;; note that the list is reversed due to how `cons` cons to the front 
  (define database
    (let loop ([n (sub1 gas)] [current '()] [num-bads 0])
      (cond
        [(= num-bads num-eggs) '()]
        [(= n 0) (list (list current (get-symbolic)))]
        [else (append (list (list current (get-symbolic)))
                      (loop (sub1 n) (cons 'b current) (add1 num-bads))
                      (loop (sub1 n) (cons 'g current) num-bads))])))

  ;; a strategy to be synthesized
  (define (strategy initial-resp)
    (define results '())
    (λ (result)
      (cond
        [(not result) initial-resp]
        [else (set! results (cons result results))
              ;; lookup from database
              (second (assoc results database))])))

  (define (probe-database the-gas)
    (for/list ([entry (in-list database)])
      (match-define (list key val) entry)
      (list (reverse key) val)))

  (do-synthesize num-eggs num-floors gas strategy probe-database))

(define solve-game (make-solve-game solve-game/gas))

;; use this to solve the games
#;(solve-game #:num-eggs 2
              #:num-floors 10
              #:upper-bound 10)

(solve-game #:num-eggs 2
            #:num-floors 10
            #:upper-bound 10)
