#lang racket
;works in Chicken, Racket, SISC
;Read a file to a list of chars
(require srfi/42) ; Racket

(define (file->lines filename)
  (call-with-input-file filename
    (lambda (p)
      (list-ec (:port line p read-line) line))))


(define (string-to-num-cells string)
  (let ((list (string-split string)))
    (cons (string->number (car list)) (string->number (cadr list)))))

(define (num-pairs)
  (define (iter l)
    (if(null? l) '()
       (cons (string-to-num-cells (car l)) (iter (cdr l)))))
  (iter (file->lines "input")))

(define (listA full)
  (if (null? full) '()
      (cons (caar full) (listA (cdr full)))))

(define (listB full)
  (if (null? full) '()
      (cons (cdar full) (listB (cdr full)))))

(define (qsort e)
  (if (or (null? e) (<= (length e) 1)) e
      (let loop ((left null) (right null)
                             (pivot (car e)) (rest (cdr e)))
        (if (null? rest)
            (append (append (qsort left) (list pivot)) (qsort right))
            (if (<= (car rest) pivot)
                (loop (append left (list (car rest))) right pivot (cdr rest))
                (loop left (append right (list (car rest))) pivot (cdr rest)))))))

(define list-a (qsort (listA (num-pairs))))
(define list-b (qsort (listB (num-pairs))))


;; part 1
(define (get-sum)
    (define (sum-iter la lb acc)
      (if (null? la) acc
          (sum-iter (cdr la) (cdr lb) (+ acc (abs (- (car la) (car lb)))))))
    (sum-iter list-a list-b 0))


;; helper functions, exist in standard library
(define (occurences symbol list)
  (define (occurences-inner l acc)
    (cond
      ((null? l) acc)
      ((eq? (car l) symbol) (occurences-inner (cdr l) (+ acc 1)))
      (else (occurences-inner (cdr l) acc))))
  (occurences-inner list 0))

(define (reduce fn list init)
  (if (null? list) init
      (fn (car list)
          (reduce fn (cdr list) init))))

;; part 2
(define (get-similarity)
    (reduce (lambda (x s) (+ s (* x (occurences x list-b)))) list-a 0))
        




         



