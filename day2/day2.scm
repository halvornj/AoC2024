#lang racket
(require srfi/42) ; Racket


(define (file->lines filename)
  (call-with-input-file filename
    (lambda (p)
      (list-ec (:port line p read-line) line))))


(define (string->report string)
  (map (lambda (x) (string->number x)) (string-split string)))



(define (succesive-pred pred report)
  (if (null? (cdr report))
      #t
      (and (pred (car report) (cadr report)) (succesive-pred pred (cdr report)))))


(define (check-report report)
  (and (or (succesive-pred < report) (succesive-pred > report)) (succesive-pred (lambda (x y) (and (< (abs (- y x)) 4) (> (abs (- y x)) 0))) report)))

(define (check-all reports acc)
  (cond
    ((null? reports) acc)
    ((check-report (car reports)) (check-all (cdr reports) (+ 1 acc)))
    (else (check-all (cdr reports) acc))))


(check-all (map string->report (file->lines "input")) 0)


;;;;;;;;;;;;; part2

(define (succesive-pred-p2 pred report strike)
  (cond
    ((null? (cdr report)) #t)
    ((pred (car report) (cadr report)) (succesive-pred-p2 pred (cdr report) strike))
    ((not (eq? strike #f)) strike) ;; we have a strike, and failed again
    (else (succesive-pred-p2 pred (cdr report) (car report)))))


(define (delete-1st x lst)
  (cond ((null? lst) '())
        ((equal? (car lst) x) (cdr lst))
        (else (cons (car lst)
                    (delete-1st x (cdr lst))))))

(define (check-report-p2 report)
  (let ((incs  (succesive-pred-p2 (lambda (x y) (and (< (abs (- y x)) 4) (> (abs (- y x)) 0))) report #f)))
    (display "incs: ")
    (display incs)
    (display "\n")
    (cond
      ((eq? incs #f) #f)
      ((eq? incs #t) (or (succesive-pred-p2 < report #f) (succesive-pred-p2 > report #f)))
      (else (or (succesive-pred < (delete-1st incs report)) (succesive-pred > (delete-1st incs report)))))))
    
  ;;(and (or (succesive-pred-p2 < report #f) (succesive-pred-p2 > report #f)) (succesive-pred-p2 (lambda (x y) (and (< (abs (- y x)) 4) (> (abs (- y x)) 0))) report #f)))
(define (check-all-p2 reports acc)
  (cond
    ((null? reports) acc)
    ((check-report-p2 (car reports)) (check-all-p2 (cdr reports) (+ 1 acc)))
    (else (check-all-p2 (cdr reports) acc))))


(check-all-p2 (map string->report (file->lines "test")) 0)
