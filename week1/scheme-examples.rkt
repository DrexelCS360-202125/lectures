#lang racket
(require "tree.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Compute the length of the list l
;   The length of the empty list is 0.
;   The length of a non-empty list is 1 + the length of the cdr of the list.
;
; Examples:
;   (length '()) => 0
;   (length '(1 2)) => 2
;   (length '(1 (2 3))) => 2
;

(define (length xs)
  (if (null? xs)
      0
      (+ 1 (length (rest xs)))))

;
; Compute the sum of a list of integers
;
; Example:
;   (sum '(1 2 3 4)) => 10

(define (sum xs)
  (if (null? xs)
      0
      (+ (first xs) (sum (rest xs)))))

; Return the nth element of a list, counting from 0.
;
; Examples:
;  (nth 0 '(1 2 3)) => 1
;  (nth 2 '(1 2 3)) => 3

(define (nth n xs)
  (if (= n 0)
      (first xs)
      (nth (- n 1) (rest xs))))

; Concatenate the lists l1 and l2 (append l2 to l1)
;   The concatention of l1 and l2 is equal to l2 if l1 is null.
;   Otherwise it is the list whose first element (car) is the first
;   element of l1 and whose tail (cdr) is equal to the concatention
;   of the tail of l1 and l2.
;
; Example:
;   (concat '(1 2 3) '(4 5 6)) => (1 2 3 4 5 6)
;

(define (concat xs ys)
  (if (null? xs)
      ys
      (cons (first xs) (concat (rest xs) ys))))

; Count the number of integers in a list
;  The number of integers in an object that is an integer is 1.
;  The number of integers in a null list is zero.
;  Otherwise the number of integers in a list l is the number of integers
;    in the car of l plus the number of integers in the cdr of l.
;
; Example:
;   (numints 5) => 1
;   (numints '(1 2 (3 4) 5)) => 5
;
; How can we handle nested lists that contain non-integers?

(define (numints xs)
  (cond [(integer? xs) 1]
        [(null? xs)    0]
        [else          (+ (numints (first xs)) (numints (rest xs)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Higher Order Function Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Return a list containing all the elements of xs incremented by 1
;
; Examples:
;  (inc-all '(1 2 3 4 5)) => '(2 3 4 5 6)

(define (inc-all xs)
 (if (null? xs)
     null
     (cons (+ 1 (first xs)) (inc-all (rest xs)))))
  
; Return a list containing all the elements of xs incremented by n
;
; That is, abstract over the value 1
;
; Examples:
;   (add-n-all 1 '(1 2 3 4 5)) => '(2 3 4 5 6)

(define (add-n-all n xs)
 (if (null? xs)
     null
     (cons (+ n (first xs)) (add-n-all n (rest xs)))))

; Return a list containing all the elements of xs when passed as arguments to a
; function.
;
; That is, abstract over the operation + n
;
; Examples:
;   (apply-all (lambda (x) (+ 1 x)) '(1 2 3 4 5)) => '(2 3 4 5 6))
;

(define (apply-all f xs)
 (if (null? xs)
     null
     (cons (f (first xs)) (apply-all f (rest xs)))))

;
; Calculate the squares of a list of integers. Make the function non-recursive.
;
; Example:
;   (squares '(1 2 3 4 5)) => '(1 4 9 16 25)
;

(define (squares xs)
  (apply-all (lambda (x) (* x x)) xs))

;
; Return a list containing only the elements of xs for which the predicate f
; returns true.
;
; Examples:
;   (filter (lambda (x) (> x 0)) '(-1 -4 5 6 0)) => '(5 6)
;

(define (filter f xs)
  (cond [(null? xs)     null]
        [(f (first xs)) (cons (first xs) (filter f (rest xs)))]
        [else           (filter f (rest xs))]))

;
; Write a non-recursive function that takes a list and returns all the even
; integers in the list
;
; Examples:
;   (only-even '(1 2 3 4 5)) => '(2 4)

(define (only-even xs)
  (filter even? xs))

; Write a function that composes two functions.
; That is, ((compose f g) x) should be the same as (f (g x))

(define (compose f g)
  (lambda (x) (f (g x))))

; Write a function that partially applies a function to a single argument.
; That is, ((papply f x) y) should be the same as (f x y)

(define (papply f x)
  (lambda (y) (f x y)))

; Add one to a number
; Define using papply

(define inc
  (papply + 1))

;;
;; Folds
;;

; Right fold

(define (foldr f z xs)
  (if (null? xs)
      z
      (f (first xs) (foldr f z (rest xs)))))

;; (show-tree (foldr (show-function 'cons) 'null '(1 2 3 4)))

; Left fold

(define (foldl f z xs)
  (if (null? xs)
      z
      (foldl f (f z (first xs)) (rest xs))))

;; (show-tree (foldl (show-function 'cons) 'null '(1 2 3 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stream Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Return the nth element of a stream, counting from 0.
;
; Examples:
;  (stream-nth 0 (stream 1 2 3)) => 1
;  (stream-nth 2 (stream 1 2 3)) => 3

(define (stream-nth n s)
  'not-implemented)

; Create a new stream that is the result of applying the function f to the
; stream s.
;
; Examples:
;   (stream->list (stream-map (lambda (x) (+ x 1)) (stream 0 1 2 3 4 5))) => (1 2 3 4 5 6)

(define (stream-map f s)
  'not-implemented)

; Create a new stream containing only the element of s satisfying the predicate
; pred.
;
; Examples:
;   (stream->list (stream-filter (lambda (x) (odd? x)) (stream 0 1 2 3 4 5))) => (1 3 5)

(define (stream-filter pred s)
  'not-implemented)

; Create a stream containing all numbers lo, lo+1, lo+2, ..., hi
(define (stream-enumerate lo hi)
  'not-implemented)

; Create a stream containing all numbers n, n+1, n+2, ...
; Note: this is an infinite stream
;
; Examples:
;  (stream-nth 100 (stream-enumerate-from 100)) => 200

(define (stream-enumerate-from n)
  'not-implemented)
