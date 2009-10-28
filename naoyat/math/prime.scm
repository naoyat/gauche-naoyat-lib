;;
;; 素数関連
;;
(define-module naoyat.math.prime
  (use srfi-1)
  (use gauche.uvector)

  (use naoyat.math)
  (use naoyat.math.random)

  (export primes-until
		  prepare-primes-until

		  prime-factors
		  prime-factors*

		  largest-prime-factor
		  nth-prime
		  the-sum-of-the-primes-below-n
		  longest-sum-of-consecutive-primes
		  n-digit-primes

		  prime-factor-table

		  prime?
		  fast-prime?
		  ))
(select-module naoyat.math.prime)

(define *primes-prepared-until* 0)
(define *primes-prepared* '())

(define (prime? n) (and (memq n *primes-prepared*) #t))

(define (primes-until n-max)
  (if (<= n-max *primes-prepared-until*)
	  (let loop ((rest *primes-prepared*) (pp '()))
		(if (null? rest) (reverse! pp)
			(let1 prime (car rest)
			  (if (< n-max prime) (reverse! pp)
				  (loop (cdr rest) (cons prime pp))))))
	  (let1 sieve (make-vector (+ n-max 1))
		(dotimes (i (+ n-max 1))
		  (vector-set! sieve i i))
		(vector-set! sieve 0 #f)
		(vector-set! sieve 1 #f)
		(let loop ((i 2))
;		  (for-each (cut vector-set! sieve <> #f)
;					(map (cut * <> i) (numbers-between 2 (quotient n-max i))))
		  (let jloop ((j (quotient n-max i)))
			(if (= j 1) 'done
				(begin (vector-set! sieve (* i j) #f)
					   (jloop (- j 1)) )))
		  (let search ((j (+ i 1)))
			(when (< j n-max)
			  (if (vector-ref sieve j) (loop j)
				  (search (+ j 1))
				  ))))
		(values (filter identity (vector->list sieve))
				sieve)
		)))

;;
;; till までの素数を用意しておく
;;
(define (prepare-primes-until till)
  (set! *primes-prepared* (primes-until till))
  (set! *primes-prepared-until* till))

;; nを素因数分解
(define (prime-factors n)
  (if (= n 1) '();<= n 3) (list (list n 1))
	  (let1 sqrt-n (floor->exact (sqrt n))
		(let next ((rest n)
				   (factors '())
				   (prime-candidates
					(primes-until sqrt-n)))
		  (if (null? prime-candidates)
			  (reverse! (if (= 1 rest) factors (cons (list rest 1) factors)))
			  (let1 prime (car prime-candidates)
				(let loop ((rest rest) (count 0))
				  (if (= 1 rest)
					  (reverse! (if (= 0 count) factors
									(cons (list prime count) factors)))
					  (if (= 0 (remainder rest prime))
						  (loop (/ rest prime) (+ count 1))
						  (next rest (if (= 0 count) factors 
										 (cons (list prime count) factors))
								(cdr prime-candidates))
						  ) ))))))))

;; nを素因数分解
(define (prime-factors* n) ;個数不問の場合
  (if (= n 1) '()
	  (let1 sqrt-n (floor->exact (sqrt n))
		(let next ((rest n)
				   (factors '())
				   (prime-candidates (primes-until sqrt-n)))
		  (if (null? prime-candidates)
			  (reverse! (if (= 1 rest) factors (cons rest factors)))
			  (let1 prime (car prime-candidates)
				(let loop ((rest rest) (count 0))
				  (if (= 1 rest)
					  (reverse! (if (= 0 count) factors
									(cons prime factors)))
					  (if (= 0 (remainder rest prime))
						  (loop (/ rest prime) (+ count 1))
						  (next rest (if (= 0 count) factors 
										 (cons prime factors))
								(cdr prime-candidates))
						  ) ))))))))


(define (largest-prime-factor n)
  (largest (map car (prime-factors n))))


(define (nth-prime n)
  (define prime-until-120000 (primes-until 120000))
  (list-ref prime-until-120000 (- n 1)))


(define (the-sum-of-the-primes-below-n n)
  (fold + 0 (primes-until (- n 1))))

(define (longest-sum-of-consecutive-primes until)
  (let ((lenmax 0)
		(at 0)
		(the-sum 0))
	(let loop ((primes (cdr (primes-until until)))
			   (sum 2)
			   (since 2)
			   (len 1))
	  (define (prime? n) (memq n primes))
	  (if (null? primes)
		  (list lenmax at the-sum)
		  (let* ((prime (car primes))
				 (newsum (+ sum prime)))
			(format #t "(loop ... ~d ~d ~d; ~d ~d)\n" sum since len prime newsum)
			(if (prime? newsum)
				(begin
				  (when (= len lenmax)
					(set! lenmax (+ len 1))
					(set! at since)
					(set! the-sum newsum))
				  (loop (cdr primes) newsum since (+ len 1))
				  )
				(loop (cdr primes) prime prime 1))
			)))))

(define (longest-sum-of-consecutive-primes until)
  (let* ((primes (primes-until until))
;;		 (accum (accumulate-numbers 0 primes))
		 (accum (filter (cut <= <> until) (accumulate-numbers 0 primes)))
		 (accum-max (car (last-pair accum)))
		 (prime* (primes-until (+ 1 accum-max)))
		 (the-length 0)
		 (the-sublist '())
		 (the-sum 0)
		 )
	(define (prime? n) (memq n prime*))
	(let loop ((i 0) (rest accum));(filter (cut <= <> until) accum)))
	  (if (null? rest) 'done
		  (let* ((ac0 (car rest))
				 (candidates (reverse! (map (cut - <> ac0) rest))))
			(let iloop ((cs candidates))
			  (if (null? cs) #f
				  (let1 c (car cs)
					(when (prime? c)
					  (let1 len (- (length cs) 1)
						(when (< the-length len)
						  (set! the-length len)
						  ;(set! since  (list-ref primes ac0))
						  (set! the-sublist (subseq primes ac0 (+ ac0 len)))
						  (set! the-sum c)
						  (format #t "found sum=~d ~a length=~d\n" c cs (- (length cs) 1))
						  )))
					(iloop (cdr cs)) ))); iloop
			(loop (+ i 1) (cdr rest)) ))); loop
	(list the-sum the-sublist the-length)
	))

(define (n-digit-primes n)
  (let ([lower (expt 10 (- n 1))]
		[upper (- (expt 10 n) 1)])
	(filter (cut <= lower <>) (primes-until upper))))

;;;
;;;
(define (prime-factor-table num . args)
  (let* ([primes (primes-until num)]
		 [factors (prime-factors num)]
		 [primes-ht (make-hash-table)]
		 [truncatable? (if (null? args) #t #f)]
		 [size (if truncatable? (length primes) (car args))]
		 [vec (make-u32vector size 0)]
		 )

	(define (nth-prime prime) (hash-table-get primes-ht prime))

	(let loop ((ps primes) (i 0))
	  (unless (null? ps)
		(hash-table-put! primes-ht (car ps) i)
		(loop (cdr ps) (+ i 1))))

	(dolist (factor factors)
	  (let ([prime (car factor)]
			[cnt (cadr factor)])
		(when (<= 2 prime)
		  (u32vector-set! vec (nth-prime prime) cnt)
		  )))

	(if truncatable?
		(let loop ((i (- size 1)))
		  (cond [(< i 0) #u32()]
				[(= 0 (u32vector-ref vec i)) (loop (- i 1))]
				[else (u32vector-copy vec 0 (+ i 1))] ))
		vec)))

;;
;; Miller-Rabinテスト (SICP 1.28)
;;
#|
(define (miller-rabin-test* times until)
  (let1 primes (primes-until until)
	(let loop ((i 3) (miss 0))
	  (if (< until i)
		  miss
		  (let ((pm (if (memq i primes) #t #f))
				(mr (fast-prime? i times)))
			(loop (+ i 2)
				  (if (eq? pm mr) miss (+ miss 1))))))))
|#
(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod* a (- n 1) n) 1)) ;; expmod だけを変更

  (try-it (+ 1 (random (- n 1)))))

(define (expmod* base exp m)
  (define (check-nontrivial-sqrt-1 a)
    (define (check-if-1 r)
      (if (and (< 1 a)
               (< a (- m 1))
               (= r 1))
          0
          r))
    (check-if-1 (remainder (square a) m)))
  (cond ((= exp 0) 1)
        ((even? exp)
         (check-nontrivial-sqrt-1 (expmod* base (/ exp 2) m)))
        (else
         (remainder (* base (expmod* base (- exp 1) m))
                    m))))

(define (fast-prime? n . args)
  (let1 times (if (null? args) 3 (car args))
	(cond [(< n 2) #f]
		  [(= n 2) #t]
		  [(zero? (remainder n 2)) #f]
		  [(= times 0) #t]
		  [(miller-rabin-test n) (fast-prime? n (- times 1))]
		  [else #f])))

(provide "naoyat/math/prime")
;;EOF