#lang racket

;; We hope this average function
#;(define (average list)
    (/ (sum list) (length list)))

;; will ???? into something like: 
(define (average list)
  (define (avg-helper sa la list)
    (if (empty? list)
        (/ sa la)
        (avg-helper (+ (first list) sa)
                    (add1 la)
                    (rest list))))
  (avg-helper 0 0 list))

(define sum-proc (λ (val partial-sum) (+ val partial-sum)))
(define len-proc (λ (val len) (add1 len)))

; ????ing average
; -------------------------------------------------------------------------------------

; pasting definitions of sum and list in place
(define (avg0 list)
  (/ (foldr sum-proc 0 list)
     (foldr len-proc 0 list)))

(define (to-parallel-proc proc1 proc2)
  (λ (val pair) (cons (proc1 val (first pair))
                  (cons (proc2 val (second pair)) empty))))

; 
(define (avg1 list)
  (let ([pair (foldr (to-parallel-proc sum-proc len-proc) '(0 0) list)])
    (/ (first pair) (second pair))))

; TODO use vector?
; TODO turn into macro
; TODO search planet for examples