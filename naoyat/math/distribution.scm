(define-module naoyat.math.distribution
  (use math.const) ;; pi
  (use math.mt-random) ;; Mersenne Twister

  (use naoyat.math)
  (use naoyat.math.h)

  (export make-Bernoulli-distribution
		  make-binomial-distribution
		  make-beta-distribution
		  make-multinomial-distribution
		  make-Dirichlet-distribution
		  ;; gauss1 gauss2
		  ))
(select-module naoyat.math.distribution)

;;;;;;;;;
;;
;; ベルヌーイ分布 Bernoulli distribution
;;
(define (make-Bernoulli-distribution μ)
  (let ([E_const μ]
        [var_const (* μ (- 1 μ))])

    (define (p x) ; p(x|μ) ... (2.1)
      (cond [(= x 1) μ]
            [(= x 0) (- 1 μ)]
            ;[else #<undefined>]
            ))
    (define (Bern x) ; Bern(x|μ) ... (2.2)
      (* (expt μ x) (expt (- 1 μ) (- 1 x))))

    (define (E x) E_const) ; ... (2.3)
    (define (var x) var_const) ; ... (2.4)

    (define (likelihood D) ; p(D|μ) ... (2.5)
      (Π (map Bern D)))
    (define (ln-likelihood D) ; ln p(D|μ) ... (2.6)
      (Σ (map (lambda (x_n) (+ (* x_n (log μ))
                                (* (- 1 x_n) (log (- 1 μ))))))))
    (define (μML D) ; ... (2.7)
      (/ (Σ D) (length D)))
    
    (lambda (m)
      (case m
        [(p Bern) p];Bern]
        [(E) E]
        [(var) var]
        [(likelihood) likelihood]
        [(μML) μML]
        ))))

;;
;; 二項分布 binomial distribution
;;
(define (make-binomial-distribution N μ)
  (let ([E_const (* N μ)]
        [var_const (* N μ (- 1 μ))])
    (define (Bin m) (* (C N m) (expt μ m) (expt (- 1 μ) (- N m))))
    (define (E m) E_const)
    (define (var m) var_const)

    (lambda (m)
      (case m
        [(p Bin) Bin]
        [(E) E]
        [(var) var]
        ))))

;;
;; ベータ分布 beta distribution
;;
(define (make-beta-distribution a b)
  (let ([g (/ (Γ (+ a b)) (Γ a) (Γ b))]
        [E_const (/ a (+ a b))]
        [var_const (/ (* a b) (+ a b) (+ a b) (+ a b 1))])
    (define (Beta μ) (* g
                         (or (inf-filter (expt μ (- a 1))) 1)
                         (or (inf-filter (expt (- 1 μ) (- b 1))) 1) ))
    (define (E μ) E_const)
    (define (var μ) var_const)
    (lambda (m)
      (case m
        [(p Beta) Beta]
        [(E) E]
        [(var) var]
        ))))

;;
;; 多項分布 multinomial distribution
;;
(define (make-multinomial-distribution μs N) ;; μs = (μ1 ... μk)T
  (define (Mult ms) ;; ... (2.34)
    (* (/ (factorial N) (Π (map factorial ms))) ;; 2.35
       (Π (map expt μs ms))))
  (lambda (m)
    (case m
      [(p Mult) Mult]
;     [(E) E]
;     [(var) var]
      )))

;;
;; ディリクレ分布 Dirichlet distribution
;;
(define (make-Dirichlet-distribution αs) ;; αs = (α1 .... αk)T
  (let1 g (/ (Γ (Σ αs)) (Π (map Γ αs)))
    (define (Dir μs) ;; μs = (μ1 ... μk)
      (* g (Π (map (lambda (μ α) (expt μ (- α 1))) μs αs))))
    (lambda (m)
      (case m
        [(p Dir) Dir]
;     [(E) E]
;     [(var) var]
        ))))


;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

#|
;;
;; Gaussian distributions (deprecated)
;;
(define (gauss1 x mu sigma)
  (let1 sigma^2 (square sigma)
    (/ (exp (* -1 (/ (square (- x mu)) (* 2 sigma^2))))
       (/ 1 (sqrt (* 2 pi sigma^2))) )))

(define (gauss2 X Mu Sigma) ;; それぞれ vector
  (let* ([lmd (- (* (vector-ref Sigma 0) (vector-ref Sigma 3)) ;; 0.936
                 (* (vector-ref Sigma 1) (vector-ref Sigma 2)))]
         [xm0 (- (vector-ref X 0) (vector-ref Mu 0))]
         [xm1 (- (vector-ref X 1) (vector-ref Mu 1))]
         [mahalanobis^2 (/ (+ (* xm0 xm0 (vector-ref Sigma 3))
                              (* -1 xm0 xm1 (+ (vector-ref Sigma 1)
                                               (vector-ref Sigma 2)))
                              (* xm1 xm1 (vector-ref Sigma 0)) )
                           lmd)])
    (/ (exp (* -1/2 mahalanobis^2))
       (* pi 2 (sqrt lmd)) )))
|#

(provide "naoyat/math/distribution")
;;EOF