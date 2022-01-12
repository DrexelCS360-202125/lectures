#lang racket

(define (memo-proc proc)
  (let ((already-run? false)
        (result null))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))
