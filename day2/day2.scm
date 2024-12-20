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


(define acceptable-level-difference?
  (lambda (x y) (and (< (abs (- y x)) 4) (> (abs (- y x)) 0))))


(define (check-report report)
  (and (or (succesive-pred < report) (succesive-pred > report)) (succesive-pred acceptable-level-difference? report)))

(define (check-all reports acc)
  (cond
    ((null? reports) acc)
    ((check-report (car reports)) (check-all (cdr reports) (+ 1 acc)))
    (else (check-all (cdr reports) acc))))


(check-all (map string->report (file->lines "input")) 0)


;;;;;;;;;;;;; part2
(display "PART 2\n")

;;deletes the first occurence of x from lst.
(define (delete-1st x lst)
  (cond ((null? lst) '())
        ((equal? (car lst) x) (cdr lst))
        (else (cons (car lst)
                    (delete-1st x (cdr lst))))))


(define (delete-index i lst)
  (define (santas-little-helper l acc)
    (cond
      ((null? l) '())
      ((eq? acc i) (cdr l))
      (else (cons (car l) (santas-little-helper (cdr l) (+ 1 acc))))))
  (santas-little-helper lst 0))


;; returns #t if we got it, 'el' if el is the failing element
(define (succesive-pred-p2 pred report)
  (define (is-lowkey-cap i report)
     (cond
       ((null? (cdr report)) #t)
       ((pred (car report) (cadr report)) (is-lowkey-cap (+ i 1) (cdr report)))
       (else i)))
  (is-lowkey-cap 0 report))


  (define (check-report-p2 report)    

    (define (check-failed-indeces indeces report)
      (cond
        ((null? indeces) #f)
        ((check-report (delete-index (car indeces) report)) #t)
        (else (check-failed-indeces (cdr indeces) report))))
      

    (let ((failed-indeces (list)))
    (if
      ;; this should flag first regardless of ascend or descend.
      (not (eq? #t (succesive-pred-p2 acceptable-level-difference? report))) (set! failed-indeces (cons (succesive-pred-p2 acceptable-level-difference? report) failed-indeces)) '()) ;; check the levels normally, with bad el removed.
      ;; if not ascending or descending, try both with fail el removed - 1 will fail, but other could be true.
      (if
      (not (eq? #t (succesive-pred-p2 > report))) (set! failed-indeces (cons (succesive-pred-p2 > report) failed-indeces)) '())
      (if
      (not (eq? #t (succesive-pred-p2 < report))) (set! failed-indeces (cons (succesive-pred-p2 < report) failed-indeces)) '())

      (display "  failed indeces: ")
      (display failed-indeces)
      
      (cond
        ((and (eq? #t (succesive-pred-p2 acceptable-level-difference? report)) (or (eq? #t (succesive-pred-p2 < report)) (eq? #t (succesive-pred-p2 > report)))) #t)
        ;; hacky check for last element
        ((check-report (reverse (cdr (reverse report)))) #t)
        (else (check-failed-indeces failed-indeces report)))))





      
  (define (check-all-p2 reports acc)
    
;;;;;;;DEBUG
    
    (if (not (null? reports))
        (begin
          (display "\n")
          (display (car reports))
          (display (check-report-p2 (car reports))))
        '())
;;;;;;;;;;;;;;;;;;;
    
    (cond
      ((null? reports) acc)
      ((check-report-p2 (car reports)) (check-all-p2 (cdr reports) (+ 1 acc)))
      (else (check-all-p2 (cdr reports) acc))))


  (display "TEST: \n")
  (check-all-p2 (map string->report (file->lines "test")) 0)


  (display "FINAL FROM INPUT: \n")
  (check-all-p2 (map string->report (file->lines "input")) 0)

