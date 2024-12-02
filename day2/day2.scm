#lang racket
(require srfi/42) ; Racket

(define (file->lines filename)
  (call-with-input-file filename
    (lambda (p)
      (list-ec (:port line p read-line) line))))


(define (string->report string)
  (map (lambda (x) (string->number x)) (string-split string)))

(define (is-ascending? report)
  (if (null? (cdr report))
      #t
      (and (< (car report) (cadr report)) (is-ascending? (cdr report)))))

(define (is-descending? report)
    (if (null? (cdr report))
      #t
      (and (> (car report) (cadr report)) (is-descending? (cdr report)))))


(define (is-within-range report)
  (define (differences l)
    (if (null? (cdr l))
        '()
        (cons (abs (- (cadr l) (car l))) (differences (cdr l)))))
  
  (define (check-levels l)
    (if (null? l) #t
     (and (and (> 4 (car l)) (< 0 (car l))) (check-levels (cdr l)))))

  (check-levels (differences report)))

(define (check-report report)
  (and (or (is-ascending? report) (is-descending? report)) (is-within-range report)))

(define (check-all reports acc)
  (cond
    ((null? reports) acc)
    ((check-report (car reports)) (check-all (cdr reports) (+ 1 acc)))
    (else (check-all (cdr reports) acc))))

