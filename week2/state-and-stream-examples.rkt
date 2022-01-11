#lang racket

(provide (all-defined-out))

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
