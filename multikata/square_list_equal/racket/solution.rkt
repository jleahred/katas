#lang racket/base

(module supersecure typed/racket/base
  (provide comp)

  (: comp (-> (Listof Integer) (Listof Integer) Boolean))
  (define (comp a b)
    (: sqr (-> Integer Integer))
    (define (sqr x) (* x x))
    (equal? (sort (map sqr a) <) (sort b <))))

(module test racket/base
  (require rackunit)
  (require (submod ".." supersecure))
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
      (check-false (comp a b))))
  (test-case
      "Typing tests"
    (check-exn exn:fail:contract? (lambda () (comp '(a b c) '(1 2 3))))
    (check-exn exn:fail:contract? (lambda () (comp '(a b c) '(a b c))))
    (check-exn exn:fail:contract? (lambda () (comp '(1 2 3) '(a b c))))
    (check-exn exn:fail:contract? (lambda () (comp '(1.0 2.0) '(1.0 4.0))))
    (check-exn exn:fail:contract? (lambda () (comp 2 4)))
    (check-exn exn:fail:contract? (lambda () (comp (vector 1 2 3) (vector 1 4 9))))))
