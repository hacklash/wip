#lang racket

(define (filter keep? list)
  (cond
    [(empty? list) empty]
    [(keep? (first list))
     (cons (first list)
           (filter keep? (rest list)))]
    [else (filter keep? (rest list))]))

(define (map func list)
  (if (empty? list)
      empty
      (cons (func (first list))
            (map func (rest list)))))

(define (sum list)
  (if (empty? list)
      0
      (+ (first list)
         (sum (rest list)))))

(define (length list)
  (if (empty? list)
      0
      (add1 (length (rest list)))))

(define (range start stop (step 1))
  (if (> start stop)
      empty
      (cons start (range (+ start step) stop step))))

(define (build-list n func)
  (map func (range 0 n 1)))

