(define-module naoyat.math
  (use srfi-1)

  (export !=
		  
		  square
		  factorial

		  divisors
		  divisible?

		  log2 log10 log16
		  C Σ Π

		  interpolate
		  numbers-between
		  accumulate-numbers

		  inf-filter

		  smallest largest

		  logistic-sigmoid σ
		  logit

		  expt* ;; 0^0=0を保証
		  ))
(select-module naoyat.math)

;;(define *epsilon* 1e-12)

(define (!= x y) (not (= x y)))

;;
;; x^2
;;
(define (square x) (* x x))

;;
;; n!
;;
(define (factorial n)
  (let loop ((n n) (prod 1))
    (if (<= n 1) prod
		(loop (- n 1) (* prod n)))))

;;
;; 約数
;;
(define (divisors n)
  (let1 sqrt-n (floor->exact (sqrt n))
	(let loop ((divisor sqrt-n) (result (list 1 n)))
	  (if (= divisor 1) (uniq (sort result))
		  (if (= 0 (remainder n divisor))
			  (loop (- divisor 1) (cons (/ n divisor) (cons divisor result)))
			  (loop (- divisor 1) result))
		  ))))

(define (divisible? n d) (= 0 (remainder n d)))

;;
;; log_2, log_10, log_16
;;
(define (logb base)
  (let1 log_b (log base)
	(lambda (x) (/ (log x) log_b))))

(define log2 (logb 2))
(define log10 (logb 10))
(define log16 (logb 16))

;;
;; nCk
;;
(define (C n k)
  (let loop ((x 1) (n n) (k k))
	(if (= k 0) x
		(loop (/ (* x n) k)
			  (- n 1)
			  (- k 1)))))

;;
;; Σ, Π
;; 
(define (Σ lis) (apply + lis))
(define (Π lis) (apply * lis))

;;
;; x0*(1-t) + x1*t, ie. x=x0 when t=0, x1 when t=1
;;
(define (interpolate x0 x1 t) (+ (* x0 (- 1 t)) (* x1 t)))

;;
;; [x..y] をリストで返す
;;
(define (numbers-between x y)
  (if (<= x y)
	  (iota (+ 1 (- y x)) x)
	  '()))

;;
;;
;;
(define (accumulate-numbers base nums)
  (reverse! (fold (lambda (x y) (cons (+ x (car y)) y))
				  (list base) nums)))

;; グラフ化する際に±∞,±NaN をカットするための関数
;(define (inf-filter x) (clamp x -99999 99999))
(define (inf-filter x . args)
  (let* ([+limit (if (null? args) 999999 (car args))]
		 [-limit (- +limit)])
	(cond [(not (real? x)) #f]
		  [(= x +inf.0) +limit]
		  [(= x -inf.0) -limit]
										;		[(= x +nan.0) #f]
;		[(= x -nan.0) #f]
		  [else x])))

;;
(define (smallest lis)
  (if (null? lis)
	  (if #f #f)
	  (car (sort lis <))))

(define (largest lis)
  (if (null? lis)
	  (if #f #f)
	  (car (sort lis >))))


;;
;; logistic sigmoid function
;;
(define (logistic-sigmoid a) (/ 1 (+ 1 (exp (- a)))))
(define σ logistic-sigmoid)

(define (logit p) (log (/ p (- 1 p))))


;;
;;
(define (expt* x y) (if (= x 0) 0 (expt x y)))

;;
;;
(provide "naoyat/math")
