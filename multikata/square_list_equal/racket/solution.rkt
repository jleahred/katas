#lang racket/base

(define (comp a b)
  (let ([sqr (lambda (x) (* x x))])
    (equal? (sort (map sqr a) <) (sort b <))))

(module+ test
  (require rackunit)
  (test-case
      "Compare tests"
    (let ([a '(1 2 3 4)]
          [b '(1 4 9 16)])
      (check-true (comp a b)))
    (let ([a '(2 3 1 4)]
          [b '(16 4 9 1)])
      (check-true (comp a b)))
    (let ([a '(1 2 3 4)]
          [b '(1 4 9)])
      (check-false (comp a b)))
    (let ([a '(1 2 3 4 4)]
          [b '(1 4 9 16 16)])
      (check-true (comp a b)))
    (let ([a '(1 2 3 4 4)]
          [b '(1 4 9 16)])
      (check-false (comp a b)))
    ;; introduced to fully define kata
    (let ([a '(1 2 3)]
          [b '(1 4 25)])
      (check-false (comp a b)))))
