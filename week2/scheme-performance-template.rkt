#lang racket

(define-syntax time-expression
  (syntax-rules ()
    ((_ e) (begin (collect-garbage)
                  (time e)
                  #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List membership
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Return #t if x is a member of the list ys, #f otherwise. Equality is
;; determined using equal?
;;

(define (member? x ys)
  'not-implemented)

;; What if we swap the first two conditions?
;; What if we use length instead of null?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reverse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; Write a function that reverses a list
;

(define (reverse xs)
  'not-implemented)

(define (reverse2 xs)
  'not-implemented)

;
; What about performance?
;
(define (repeat x n)
  'not-implemented)

(define long-list (repeat 1 10000))

;; Try: (time-expression (reverse long-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exponent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; Define a function that computes the integer exponent of a number
;

(define (expt b n)
  'not-implemented)

(define (expt2 b n)
  'not-implemented)

(define (fast-expt b n)
  'not-implemented)

;
; What about performance?
;

;; Try: (time-expression (expt 2 100000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let Over Lambda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lambda-computer)
  (let ((y 1))
    (lambda (x)
      (+ y x))))

(define (procedure-computer y)
  (define (inc x)
    (+ y x))
  inc)