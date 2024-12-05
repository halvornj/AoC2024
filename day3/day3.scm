#lang racket
(require srfi/42) ; Racket


(define (file->lines filename)
  (call-with-input-file filename
    (lambda (p)
      (list-ec (:port line p read-line) line))))



(define teststring "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
(define input (file->lines "input"))

(define (find-matches-in-string string)
  (define r (pregexp "mul\\([0-9]+,[0-9]+\\)"))
  (regexp-match* r string))

(define (reduce fn list init)
  (if (null? list) init
      (fn (car list)
          (reduce fn (cdr list) init))))


(define (get-nums-from-string s)
  (map (lambda (x) (map string->number (string-split (string-trim x "mul(") #rx"[,\\)]"))) s))





(define (get-result-from-string s)
  (reduce (lambda (x y) (+ (* (car x) (cadr x)) y)) (get-nums-from-string (find-matches-in-string s)) 0))  

  
(define (string-iter strings acc)
  (if (null? strings)
      acc
      (string-iter (cdr strings) (+ acc (get-result-from-string (car strings))))))

(string-iter input 0)

 
 ;; PART 2


(define (find-matches-p2 string)
  (define mul (pregexp "mul\\([0-9]+,[0-9]+\\)"))
  (define do (pregexp "do"))
  (define dont (pregexp "don't"))
  (regexp-match* #rx"mul\\([0-9]+,[0-9]+\\)|don't|do" string))

(define input2 (string-append* (car input) (cdr input)))

(define (part2)
  (let ((do #t)
        (total 0)
        (strings (find-matches-p2 input2)))

    (define (mulstr->number str)
      
      (display "\n mulstr->number called with ")
      (display str)
      (display "\n")
      
      (let ((nums (map string->number (string-split (string-trim str "mul(") #rx"[,\\)]"))))
        (* (car nums)(cadr nums))))
      ;;(map string->number (string-split (string-trim str "mul(") #rx"[,\\)]")))

    
    (define (check-entry item)
      (display item)
      (cond
        ((equal? item "do") (begin (set! do #t) 0))
        ((equal? item "don't") (begin (display "\nsetting do #f\n") (set! do #f) 0))
        (else (mulstr->number item))))

    (define (count-total list)
      (cond
        ((null? list) #t)
        ((eq? 0 (check-entry (car list))) (count-total (cdr list)))
        (else (begin (if do (set! total (+ total (check-entry (car list)))) (display "\nin a don't-mul\n")) (count-total (cdr list))))))

   (count-total strings)
    total))


(part2)
          




  