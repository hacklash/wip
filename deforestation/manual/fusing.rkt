#lang racket

; IDEA - for/* as a fold?

; function name aliases
(define sum-proc (λ (val partial-sum) (+ val partial-sum)))
(define len-proc (λ (val len) (add1 len)))
(define map-square-proc (λ (val accum) (cons (square val) accum)))
(define sum-sqs-to-next (λ (seed) (values seed (+ 1 seed))))

(define square (λ (x) (expt x 2)))

(define (fibs-to n)
  (unfold (λ (seed) (values (first seed)
                            (list (second seed)
                                  (+ (first seed) (second seed))
                                  (add1 (third seed)))))
          (λ (seed) (> (third seed) n))
          '(0 1 1)))

(require "folds.rkt")

; Deforesting the sum of squares up to n
; -------------------------------------------------------------------------------------
(define (sum-of-squares-to n)
    (sum (map square (range 0 n))))

; pasting the bodies of the definitions from folds.rkt
(define (sum-of-squares-to0 n)
  (foldr sum-proc
         0
         (foldr map-square-proc
                empty
                (unfold sum-sqs-to-next
                        (λ (seed) (> seed  n))
                        0))))

; ai == (listof vo) => compose-fold-procs (vo ao -> ao) (vi ai -> ai) ai -> (vi ao -> ao)
(define (compose-fold-procs outer-proc inner-proc inner-init) 
  (λ (inner-val outer-accum)
    (outer-proc (first (inner-proc inner-val inner-init))
                outer-accum)))

; merging the folds
(define (sum-of-squares-to1 n)
  (foldr (compose-fold-procs sum-proc map-square-proc empty)
         0
         (unfold sum-sqs-to-next
                 (λ (seed) (> seed  n))
                 0)))

(define (deforest-to-next to-next fold-proc)
  (λ (seed accum)
    (begin
      (define-values (next-val next-seed) (to-next seed))
      (values (fold-proc next-val accum) next-seed))))

(define (unfold-deforested to-next stop? seed accum)  
  (define-values (next-val next-seed) (to-next seed accum))
  (if (stop? next-seed)
      next-val
      (unfold-deforested to-next stop? next-seed next-val)))

; deforesting the unfold
(define (sum-of-squares-to2 n)
  (unfold-deforested (deforest-to-next sum-sqs-to-next
                       (compose-fold-procs sum-proc map-square-proc empty))
                     (λ (seed) (> seed  n))
                     0  ; unfold seed
                     0)); sum-fold initial accumulator

; Deforesting the sum of squares up to n
; -------------------------------------------------------------------------------------
(define (sum-first-squared-odd-fibs n)
  (sum (map square (filter odd? (fibs-to n)))))
