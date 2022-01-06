#lang racket
(require rackunit)
(require rackunit/text-ui)
(require rackunit/gui)
(require "scheme-examples.rkt")

(define lecture-tests
  (test-suite
   "Scheme Tests"
   list-tests
   hof-tests))

(define list-tests
  (test-suite
   "List Tests"
   
   (test-suite
    "length"
    
    (test-case
     "(length '())"
     (check-equal? (length '()) 0))
    
    (test-case
     "(length '(1 2))"
     (check-equal? (length '(1 2))
                   2))
    
    (test-case
     "(length '(1 (2 3)))"
     (check-equal? (length '(1 (2 3)))
                   2)))

   (test-suite
    "sum"
    
    (test-case
     "(sum '(1 2 3 4))"
     (check-equal? (sum '(1 2 3 4))
                   10)))

   (test-suite
    "nth"
    
    (test-case
     "(nth 0 '(1 2 3))"
     (check-equal? (nth 0 '(1 2 3))
                   1))
    
    (test-case
     "(nth 2 '(1 2 3))"
     (check-equal? (nth 2 '(1 2 3))
                   3)))

   (test-suite
    "concat"
    
    (test-case
     "(concat '(1 2 3) '(4 5 6))"
     (check-equal? (concat '(1 2 3) '(4 5 6))
                   '(1 2 3 4 5 6))))

   (test-suite
    "numints"
    
    (test-case
     "(numints 5)"
     (check-equal? (numints 5)
                   1))
    
    (test-case
     "(numints '(1 2 (3 4) 5))"
     (check-equal? (numints '(1 2 (3 4) 5))
                  5)))
   
   (test-suite
    "apply-all"

    (test-case
     "(apply-all (lambda (x) (+ x 1)) '(1 2 3))"
     (check-equal? (apply-all (lambda (x) (+ x 1)) '(1 2 3))
                  '(2 3 4))))

   (test-suite
    "squares"

    (test-case
     "(squares '(1 2 3 4 5))"
     (check-equal? (squares '(1 2 3 4 5))
                   '(1 4 9 16 25))))))

(define hof-tests
  (test-suite
   "Higher-order Function Tests"
   (test-case
    "inc-all"
    (check-equal? (inc-all '(1 2 3 4 5))
                  '(2 3 4 5 6)))
   
   (test-case
    "add-n-all"
    (check-equal? (add-n-all 1 '(1 2 3 4 5))
                  '(2 3 4 5 6))
    (check-equal? (add-n-all 5 '(1 2 3 4 5))
                  '(6 7 8 9 10)))
   
   (test-case
    "apply-all"
    (check-equal? (apply-all (lambda (x) (+ 1 x)) '(1 2 3 4 5))
                  '(2 3 4 5 6))
    (check-equal? (apply-all (lambda (x) (+ 5 x)) '(1 2 3 4 5))
                  '(6 7 8 9 10)))
   
   (test-case
    "filter"
    (check-equal? (filter (lambda (x) (> x 0)) '(-1 -4 5 6 0))
                  '(5 6)))
   
   (test-case
    "squares"
    (check-equal? (squares '(1 2 3 4 5))
                  '(1 4 9 16 25)))
   
   (test-case
    "only-even"
    (check-equal? (only-even '(1 2 3 4 5))
                  '(2 4)))))

(test/gui lecture-tests)