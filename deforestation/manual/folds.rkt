#lang racket

(define fold foldr)

(define (filter keep? list)
  (fold (λ (list-item filtered-list) 
          (if (keep? list-item) 
              (cons list-item filtered-list) 
              filtered-list))
        empty
        list))

(define (map func list)
  (fold (λ (val accum) (cons (func val) accum))
        empty
        list))

(define (sum list)
  (fold (λ (val partial-sum) (+ val partial-sum))
        0
        list))

(define (length list)
  (fold (λ (val len) (add1 len))
        0
        list))

(define (unfold to-next stop? seed)
  (define-values (next-val next-seed) (to-next seed))
  (cons next-val (if (stop? next-seed)
                     empty
                     (unfold to-next stop? next-seed))))

(define (range start stop (step 1))
  (unfold (λ (seed) (values seed (+ step seed)))
          (λ (seed) (> seed  stop))
          start))

(define (build-list n func)
  (map func (range 0 n)))

(provide (all-defined-out))