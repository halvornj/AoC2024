#lang racket
(require srfi/42) ; Racket


(define (file->lines filename)
  (call-with-input-file filename
    (lambda (p)
      (list-ec (:port line p read-line) line))))