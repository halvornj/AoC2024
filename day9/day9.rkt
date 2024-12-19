#lang racket
(require srfi/42) ; Racket


(define (eller lst)
  (cond
    ((null? lst) #f)
    ((car lst) #t)
    (else (eller (cdr lst)))))

(define (og lst)
  (cond
    ((null? lst) #t)
    ((not (car lst)) #f)
    (else (og (cdr lst)))))


(define (file->lines filename)
  (call-with-input-file filename
    (lambda (p)
      (list-ec (:port line p read-line) line))))

(define input (cdr (reverse (cdr (reverse (map string->number (regexp-split #px"" (car (file->lines "input")))))))))


(define (expand-symbol length symbol)
  (if (eq? 0 length)
      '()
      (cons symbol (expand-symbol (- length 1) symbol))))

(define (expanded-diskmap inp)

  (define (diskmap-inner initial expanded i)
    (cond
      ((null? initial) expanded)
      ((even? i) (diskmap-inner (cdr initial) (append expanded (expand-symbol (car initial) (/ i 2))) (+ 1 i)))
      (else (diskmap-inner (cdr initial) (append expanded (expand-symbol (car initial) #f)) (+ i 1)))))
  (diskmap-inner inp '() 0))

(define expanded (expanded-diskmap input))




(define (move map)
  (define (rest lst)
    (if (not (car lst)) '() (cons (car lst) (rest (cdr lst)))))

  (define (iter primary reverse)
    (cond
      ((not (car reverse)) (iter primary (cdr reverse)))
      ((eq? (car primary) (car reverse)) (rest primary))
      ((car primary) (cons (car primary) (iter (cdr primary) reverse)))
      (else (cons (car reverse) (iter (cdr primary) (cdr reverse))))))
  

 (iter map (reverse map)))

(define moved (move expanded))


  (define (get-id-mul-index lst acc index)
    (if (null? lst)
        acc
        (get-id-mul-index (cdr lst) (+ acc (* (car lst) index)) (+ index 1))))


(display "start-time: ")
(display (current-seconds))
(display "\npart1: \n")
(get-id-mul-index moved 0 0)
(display "\n end-time: ")
(display (current-seconds))