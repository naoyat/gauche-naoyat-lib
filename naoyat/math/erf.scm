(define-module naoyat.math.erf
  (use math.const) ;; pi

  (export erf erfc
		  complex-erf
		  inverse-erf

		  normal-cumulative-distribution Φ
		  probit
		  ))
(select-module naoyat.math.erf)

(define *epsilon* 1e-12)

;;
;; (Gauss) erf (error function)
;;
(define (erf z)
  (let1 r (* -1 z z)
    (let loop ((n 0) (pmz 1) (n! 1) (sum 0))
      (let1 delta (/ pmz n! (+ n n 1))
        (if (< (abs delta) *epsilon*)
            (* (/ (* 2 z) (sqrt pi)) sum)
            (loop (+ n 1)
                  (* pmz r)
                  (* (+ n 1) n!)
                  (+ sum delta) ))))))

;;
;; complementary error function
;;
(define (erfc x) (- 1 (erf x)))

;;
;; complex error function
;;
(define (complex-erf x) (* (exp (* -1 x x)) (erfc (* -i x))))
(define w complex-erf)

;;
;; inverse error function (erf-1)
;;
(define (inverse-erf z)
  (define (calc-next-ck k c)
    (let loop ((m 0) (sum 0) (ca c) (cz (reverse c)))
      (if (= m k) sum
          (loop (+ m 1)
                (+ sum (/. (* (car ca) (car cz)) (+ m 1) (+ m m 1)))
                (cdr ca) (cdr cz)))))
  (define (calc-cks k)
    (let loop ((i 0) (cks '(1)))
      (if (= i k) cks
          (loop (+ i 1) (cons (calc-next-ck (+ i 1) cks) cks)))))
  (define (calc-ck k) (car (calc-cks k)))

  (define (inverse-erf>0 z)
    (let1 r (* pi z z 1/4) ; (πz^2)/4
      (let loop ((k 0) (cks '(1)) (sum 0) (a 1))
        (let1 delta (* a (/ (car cks) (+ k k 1)))
          (if (< delta (* sum *epsilon*))
              (* 1/2 z (sqrt pi) sum)
              (loop (+ k 1)
                    (cons (calc-next-ck (+ k 1) cks) cks)
                    (+ sum delta)
                    (* a r)))))))

  (cond [(< z 0) (- (inverse-erf>0 (- z)))]
        [(= z 0) 0]
        [else (inverse-erf>0 z)]) )

;;
;; normal cumulative distribution function
;;
;;  -> probit function の逆関数
;;
(define (normal-cumulative-distribution x) (* 1/2 (+ 1 (erf (/ x (sqrt 2))))))
(define Φ normal-cumulative-distribution)

;;
;; normal quantile function (probit function)
;;
(define (probit p)
  (define (probit>0 p) (* (inverse-erf (- (* p 2) 1)) (sqrt 2)))
  (if (< p 0) 
      (- 1 (probit>0 (- p)))
      (probit>0 p)))

(provide "naoyat/math/erf")
