#lang racket
(require srfi/42) ; Racket


(define (file->lines filename)
  (call-with-input-file filename
    (lambda (p)
      (list-ec (:port line p read-line) line))))

(define input (file->lines "input"))

;; todo string->number
(define (get-rules file)
  (define r (pregexp "\\|"))
  (map (lambda (z) (regexp-split #px"\\|" z)) (filter (lambda (x) (regexp-match r x)) file)))

(define rules (get-rules input))
(define starters (map (lambda (x) (car x)) rules))

(define (get-end x)
  (map (lambda (z) (cadr z)) (filter (lambda (y) (equal? (car y) x)) rules)))

(define (get-updates file)
  (define r (pregexp ","))
  (map (lambda (z) (regexp-split #px"," z)) (filter (lambda (x) (regexp-match r x)) file)))

(define updates (get-updates input))

(define (update-is-valid? update)
  (let ((seeking (list)))
    (define (inner reversed)
      
      (cond
        ((null? reversed) #t)
        ((member (car reversed) seeking)   #f)
        ((member (car reversed) starters) (begin
                                            (set! seeking (append (get-end (car reversed)) seeking))
                                            (inner (cdr reversed))))
        (else (inner (cdr reversed)))))
    (inner (reverse update))))

(define (get-middle update)
  (define target (/ (- (length update) 1) 2))
  (define (middle-iter lst acc)
    (if (eq? acc target)
        (car lst)
        (middle-iter (cdr lst) (+ 1 acc))))
  (middle-iter update 0))
  

(define (part1 updates acc)
  (cond
    ((null? updates) acc)
    ((update-is-valid? (car updates)) (part1 (cdr updates) (+ acc (string->number (get-middle (car updates))))))
    (else (part1 (cdr updates) acc))))


(display "PART1: \n")
(part1 updates 0)