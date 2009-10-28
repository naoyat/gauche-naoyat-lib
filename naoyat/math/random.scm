;;
;; normal (Gaussian) distribution
;;
(define-module naoyat.math.random
  (use math.const) ;; pi
  (use math.mt-random) ;; Mersenne Twiste
  
  (use gauche.array)

  (export random
		  random-integer
		  
		  make-uniform-rand
		  box-muller-rand

		  make-nd-rand
		  make-nd2-rand
		  ))
(select-module naoyat.math.random)

(define *mt* (make <mersenne-twister> :seed (sys-time)))

(define (random n) ;; for Q3.05 <monte-carlo>
  (if (integer? n)
      (mt-random-integer *mt* n)
      (* (mt-random-real *mt*) n)))

(define (random-integer min max)
  (+ min (mt-random-integer *mt* (+ (- max min) 1))))

;;
;; 一様分布
(define (make-uniform-rand min max)
;; (interpolate min max (mt-random-real *mt*)))
  (lambda () (+ min (* (- max min) (mt-random-real *mt*)))))

;;
;; Box-Muller法
;;
(define (box-muller)
  (let ([r1 (mt-random-real *mt*)]
		[r2 (mt-random-real *mt*)])
	(let ([a (sqrt (* -2 (log r1)))]
		  [b (* 2 pi r2)])
	  (values (* a (cos b)) (* a (sin b)))
	  )))

(define *box-muller-stream* '())

(define (box-muller-rand)
  (if (null? *box-muller-stream*)
	  (receive (z1 z2) (box-muller)
		(set! *box-muller-stream* (list z2))
		z1)
	  (pop! *box-muller-stream*)
	  ))

(define (make-nd-rand mu sigma)
  (lambda () (+ mu (* sigma (box-muller-rand)))))

;;
;; 2次元
;;
(define (make-nd2-rand Mu Sigma)
  (let ([mu_x (vector-ref Mu 0)]
		[mu_y (vector-ref Mu 1)]
		[var_x (array-ref Sigma 0 0)]
		[var_y (array-ref Sigma 1 1)]
		[cov_xy (array-ref Sigma 0 1)]
		[nd (make-nd-rand 0 1)])
	(lambda ()
	  (let ([e1 (nd)] [e2 (nd)])
		(let1 x (+ mu_x (* (sqrt var_x) e1))
		  (values x
				  (+ mu_y (* (/ cov_xy var_x) (- x mu_x))
					 (* (sqrt (- var_y (/ (* cov_xy cov_xy) var_x))) e2)))))
	  )))

(provide "naoyat/math/random")
;;EOF