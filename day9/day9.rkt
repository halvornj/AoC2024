#lang racket
(require srfi/42) ; Racket


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

(define (rearrange-map expanded-map)

  (define (move-block map block)
    (cond
      ((null? map) '())
      ((not (car map)) (cons block (cdr map)))
      (else (cons (car map) (move-block (cdr map) block)))))

  
  (define(move-last map)
    (define (iter m prev)
      (cond
        ((og m) (append prev #f)) 
        ((car m) (move-block (reverse m) (car m)))
        (else (iter (cdr m) m))))
    (define rmap (reverse map))
    (define i (iter rmap rmap))
    (reverse(cdr (reverse i))))


  
  (define (move-all map)
    (if (og map) map (move-all (move-last map))))

  (move-all expanded-map)
  )

(define (part1)
  (define (get-id-mul-index lst acc index)
    (if (null? lst)
        acc
        (get-id-mul-index (cdr lst) (+ acc (* (car lst) index)) (+ index 1))))
  (get-id-mul-index (rearrange-map (expanded-diskmap input)) 0 0))

(display "part 1: ")
(part1)
