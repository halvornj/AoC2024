#lang racket
(require srfi/42) ; Racket
(require memo)


(define (file->lines filename)
  (call-with-input-file filename
    (lambda (p)
      (list-ec (:port line p read-line) line))))


(define input
  (map (lambda (x)(map string->number  (string-split (regexp-replace #rx":" x "")))) (file->lines "input")))

(define (mul x y)
  (display "\n mul x y: ")
  (display x)
  (display " ")
  (display y)
  (* x y))

(define (plus x y)
  (display "\n plus x y: ")
  (display x)
  (display " ")
  (display y)
  (+ x y))





(define/memoize (uint->bit-string uint length)
  (cond
    ((eq? 0 length) '())
    ((>= uint (expt 2 (- length 1))) (cons #t (uint->bit-string (- uint (expt 2 (- length 1))) (- length 1))))
    (else (cons #f (uint->bit-string uint (- length 1))))))



(define (is-valid-equation? equation)

  (define (uint->procs proc1 proc2 uint)
    (define bitstring (uint->bit-string  uint (- (length equation) 2))) ;; -2 because -1 for target of equation, and we need 1 less proc than parameters ( 1 foo 2 bar 3)

    (define (inner-bitstring bitstr)
      (cond
        ((null? bitstr) '())
        ((car bitstr) (cons proc2 (inner-bitstring (cdr bitstr))))
        (else (cons proc1 (inner-bitstring (cdr bitstr))))))
    (inner-bitstring bitstring))

  ;;should only be called with procs as 2-parameter procedures (mul, add, sub, concat ...)
  (define (apply-procs procs args)
    (if (null? (cdr procs))
        ((car procs) (car args) (cadr args))
        ((car procs) (car args) (apply-procs (cdr procs) (cdr args)))))


  (define (try-combination uint)
    (cond
      ((eq? uint (- (expt 2 (- (length equation) 2)) 1)) ;; if we've reached the final permutation - 
       (eq? (apply-procs (uint->procs + * uint) (cdr equation)) (car equation))) ;; check the answer, and return the match
      ((eq? (apply-procs (uint->procs + * uint) (cdr equation)) (car equation)) #t) ;;otherwise, if the answer matches, return true
      (else (try-combination (+ uint 1)))))

  (try-combination 0))
     
      

;;;;;;;;;;;;;;;; part1

;;(define (part1)
;;  (define (p1-iter equations acc)
;;    (cond
;;      ((null? equations) acc)
;;      ((is-valid-equation? (cons (car (car equations)) (reverse (cdr (car equations))))) (p1-iter (cdr equations) (+ acc (car (car equations))))) ;; car-car is first el in equation, target
;;      (else (p1-iter (cdr equations) acc))))
;;  (p1-iter input 0))

;;(display "part1: ")
;;(part1)

;;;;;;;;;;;;;;;;part2


(define/memoize (digits n)
  (inexact->exact (+ 1 (floor (/ (log n) (log 10))))))

(define (|| x y)
  (+ (* y (expt 10 (digits x))) x))


;;moved these outside to memoize globally

(define/memoize (uint->procs proc1 proc2 uint bitstring-length)
  (define bitstring (uint->bit-string  uint bitstring-length)) ;; -2 because -1 for target of equation, and we need 1 less proc than parameters ( 1 foo 2 bar 3)
    
  (define (inner-bitstring bitstr)
    (cond
      ((null? bitstr) '())
      ((car bitstr) (cons proc2 (inner-bitstring (cdr bitstr))))
      (else (cons proc1 (inner-bitstring (cdr bitstr))))))
  (inner-bitstring bitstring))



(define/memoize (uint-and-input->procs procs proc2 uint bitstring-length)
  (define bitstring (uint->bit-string  uint bitstring-length)) ;; -2 because -1 for target of equation, and we need 1 less proc than parameters ( 1 foo 2 bar 3)
  (define (iter procs bitstr)
    (cond
      ((null? bitstr) '())
      ((car bitstr) (cons proc2 (iter (cdr procs) (cdr bitstr))))
      (else (cons (car procs) (iter (cdr procs) (cdr bitstr))))))
  (iter procs bitstring))



(define (is-valid-equation-p2? equation)
  
  ;;should only be called with procs as 2-parameter procedures (mul, add, sub, concat ...)
  (define (apply-procs procs args)
    (if (null? (cdr procs))
        ((car procs) (car args) (cadr args))
        ((car procs) (car args) (apply-procs (cdr procs) (cdr args)))))


  (define (try-combinations uint)
    (define procs (uint->procs + * uint (- (length equation) 2)))
    
    (define (try-inner inner-uint)
      (cond
        ((eq? inner-uint (- (expt 2 (- (length equation) 2)) 1)) ;; if we've reached the final permutation - 
         (eq? (apply-procs (uint-and-input->procs procs || inner-uint (- (length equation) 2)) (cdr equation)) (car equation))) ;; check the answer, and return the match
        ((eq? (apply-procs (uint-and-input->procs procs || inner-uint (- (length equation) 2)) (cdr equation)) (car equation)) #t) ;;otherwise, if the answer matches, return true
        (else (try-inner (+ inner-uint 1)))))
      
    (cond
      ((eq? uint (- (expt 2 (- (length equation) 2)) 1)) ;; if we've reached the final permutation - 
       (try-inner 0)) ;; check the answer, and return the match
      ((try-inner 0) #t) ;;otherwise, if the answer matches, return true
      (else (try-combinations (+ uint 1)))))

  (try-combinations 0))



(define (part2)
  (define (p2-iter equations acc)
    (cond
      ((null? equations) acc)
      ((is-valid-equation-p2? (cons (car (car equations)) (reverse (cdr (car equations))))) (p2-iter (cdr equations) (+ acc (car (car equations))))) ;; car-car is first el in equation, target
      (else (p2-iter (cdr equations) acc))))
  (p2-iter input 0))

;;;;
(display "part2: \n")
(display "start: ")
(display (current-seconds))
(display "\n")

(part2)

(display "\nend: ")
(display (current-seconds))