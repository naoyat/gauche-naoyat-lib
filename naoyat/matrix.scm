(define-module naoyat.matrix
  (use srfi-1)
  ;;(use srfi-25) ; shape
  (use gauche.array)
  (use math.mt-random)

  (use naoyat.list) ; make-list-with-generator, make-random-int-list

  (export %rep %: %c %vec %t ->array
		  %solve %-1 %det
		  %^ %+ %+! %/ %/! %- %* %*%
		  %matrix %array

		  %RxC
		  %1x1 %1x2 %1x3 %1x4
		  %2x1 %2x2 %2x3 %2x4
		  %3x1 %3x2 %3x3 %3x4
		  %4x1 %4x2 %4x3 %4x4
		  %0 %1

		  O1x1 O1x2 O1x3 O1x4
		  O2x1 O2x2 O2x3 O2x4
		  O3x1 O3x2 O3x3 O3x4
		  O4x1 O4x2 O4x3 O4x4
		  I1 I2 I3 I4

		  ->list
		  %rbind %cbind
		  %dim %nrow %ncol
		  %row %col %elem %find
		  %row1 %col1 %elem1 %find1

		  %diag %set-diag!
		  %Tr

		  ;; %upper-tri %lower-tri
		  ;; %set!

		  %moore-penrose %†

		  scalar? matrix?
		  make-random-matrix
		  matrix-print
		  ))
(select-module naoyat.matrix)

(define *undef* (if #f #f))

(define (%rep elem len) (make-list len elem))
(define (%: from to) (iota (+ (- to from) 1) from))

(define (make-random-matrix nrow ncol min max)
  ;;(define (rnd) (+ min (mt-random-integer *mt* (+ (- max min) 1))))
  (%matrix (make-random-int-list (* nrow ncol) min max) :ncol ncol :nrow nrow))

;;;
;;; gauche.array
;;;
;(define %t array-transpose)
(define %c vector) ; 横ベクトル (1xN行列)
(define (%vec . elems) ; 縦ベクトル (Nx1行列)
  (apply array (shape 0 (length elems) 0 1) elems))

(define (%t o)
  (cond [(array? o) (array-transpose o)]
		[(vector? o) ;;(vector->array o)] ; 縦
		 (apply array (shape 0 (vector-length o) 0 1) (vector->list o))]
		[(list? o) (apply array (shape 0 (length o) 0 1) o)]
		[else #f]))

;(define (vector->array vec) ;; 基本的に縦
;  (apply array (shape 0 (vector-length vec) 0 1) (vector->list vec)))

(define (->array o)
  (cond [(array? o) o]
		[(vector? o) ;(%t (vector->array o))] ; 横
		 (apply array (shape 0 1 0 (vector-length o)) (vector->list o))]
		[(list? o) (apply array (shape 0 1 0 (length o)) o)]
		[else #f]))

(define %solve array-inverse) ; 逆行列
(define %-1 array-inverse)
(define %det determinant) ; 行列式 |A|

;; array-rotate-90
;; array-flip
;; identity-array

;;
(define scalar? number?)

(define %^ array-expt) ; (%^ array pow)

(define %+ array-add-elements)
(define %+! array-add-elements!)
;(define %- array-sub-elements)
;(define %-! array-sub-elements!)
;(define %* array-mul-elements)
;(define %*! array-mul-elements!)
(define %/ array-div-elements)
(define %/! array-div-elements!)

(define (%- . ms)
  (if (= 1 (length ms))
	  (array-mul-elements (car ms) -1)
	  (apply array-sub-elements ms)))

(define (%*% . ms) ;; array-mul を３つ以上の行列に拡張
  (let loop ((rest (cdr ms)) (prod (->array (car ms))))
	(if (null? rest)
		prod
		(let1 leftmost (car rest)
		  (loop (cdr rest)
				(if (scalar? leftmost)
					(array-mul-elements prod leftmost)
					(array-mul prod (->array leftmost))))))))
(define (%* . ms)
  (let1 prod (apply %*% ms)
	(if (= 1 (array-size prod))
		(array-ref prod 0 0)
		prod))) ; 結果が1x1行列の場合、スカラー値を返す　

;;
;; let-keyword の restvar で得られる残り引数が元の順番と異なる（２個ずつのペアで逆順）ので
;; キーワード部以前の引数を元の順番で得たいというニーズに応える関数を作った
;;
(define (omit-keywords options)
  (let loop ((orig options) (dest '()))
	(if (null? orig)
		(values (reverse! dest) '())
		(if (keyword? (car orig))
			(values (reverse! dest) orig)
			(loop (cdr orig) (cons (car orig) dest))))))

(define (matrix-print mx)
  (let ([start0 (array-start mx 0)] [end0 (array-end mx 0)] [length0 (array-length mx 0)]
		[start1 (array-start mx 1)] [end1 (array-end mx 1)] [length1 (array-length mx 1)])
	(do ((y start0 (+ y 1)))
		((= y end0) *undef*)
	  (do ((x start1 (+ x 1)))
		  ((= x end1) (newline))
		(format #t " ~d" (array-ref mx y x)) ))))

;;(define pp matrix-print)

(define (%matrix x . options)
  (unless (pair? x) (set! x (list x))) ; xがatomの場合リスト化
  (let1 itemcnt (length x)
	(receive (args keywords) (omit-keywords options)
	  (let-keywords keywords ((nrow #f) (nr #f)
							  (ncol #f) (nc #f)
							  (byrow #f) (b *undef*)) ; デフォルトで行主導。arrayでは列主導
		(when nr (set! nrow nr))
		(when nc (set! ncol nc))
		(unless (undefined? b) (set! byrow b))
		(let-optionals* args ((nrow_ #f)
							  (ncol_ #f)
							  (byrow_ *undef*))
		  (when nrow_ (set! nrow nrow_))
		  (when ncol_ (set! ncol ncol_))
		  (unless (undefined? byrow_) (set! byrow byrow_)))
		(cond [(and nrow ncol) #t]
			  [(and nrow (not ncol))
			   (set! ncol (/ itemcnt nrow))]
			  [(and ncol (not nrow))
			   (set! nrow (/ itemcnt ncol))]
			  [else (set! nrow 0) (set! ncol 0)])
		(unless (= itemcnt (* nrow ncol))
		  (set! x (make-list* (* nrow ncol) x)))
		(if byrow
			(apply array (shape 0 nrow 0 ncol) x)
			(array-transpose
			 (apply array (shape 0 ncol 0 nrow) x)) )
		))))

(define-macro (%RxC nrow ncol . elems) `(array (shape 0 ,nrow 0 ,ncol) ,@elems))
;(define (%RxC nrow ncol . elems) (apply array (shape 0 nrow 0 ncol) elems))
;(define (%1x1 . elems) (apply array (shape 0 1 0 1) elems))
(define-macro (%1x1 . elems) `(%RxC 1 1 ,@elems)) (define-macro (%1x2 . elems) `(%RxC 1 2 ,@elems))
(define-macro (%1x3 . elems) `(%RxC 1 3 ,@elems)) (define-macro (%1x4 . elems) `(%RxC 1 4 ,@elems))
(define-macro (%2x1 . elems) `(%RxC 2 1 ,@elems)) (define-macro (%2x2 . elems) `(%RxC 2 2 ,@elems))
(define-macro (%2x3 . elems) `(%RxC 2 3 ,@elems)) (define-macro (%2x4 . elems) `(%RxC 2 4 ,@elems))
(define-macro (%3x1 . elems) `(%RxC 3 1 ,@elems)) (define-macro (%3x2 . elems) `(%RxC 3 2 ,@elems))
(define-macro (%3x3 . elems) `(%RxC 3 3 ,@elems)) (define-macro (%3x4 . elems) `(%RxC 3 4 ,@elems))
(define-macro (%4x1 . elems) `(%RxC 4 1 ,@elems)) (define-macro (%4x2 . elems) `(%RxC 4 2 ,@elems))
(define-macro (%4x3 . elems) `(%RxC 4 3 ,@elems)) (define-macro (%4x4 . elems) `(%RxC 4 4 ,@elems))

(define-macro (%0 nrow ncol) `(make-array (shape 0 ,nrow 0 ,ncol) 0))
(define O1x1 (%0 1 1)) (define O1x2 (%0 1 2)) (define O1x3 (%0 1 3)) (define O1x4 (%0 1 4))
(define O2x1 (%0 2 1)) (define O2x2 (%0 2 2)) (define O2x3 (%0 2 3)) (define O2x4 (%0 2 4))
(define O3x1 (%0 3 1)) (define O3x2 (%0 3 2)) (define O3x3 (%0 3 3)) (define O3x4 (%0 3 4))
(define O4x1 (%0 4 1)) (define O4x2 (%0 4 2)) (define O4x3 (%0 4 3)) (define O4x4 (%0 4 4))

(define %1 identity-array)
(define I1 (%1 1)) (define I2 (%1 2)) (define I3 (%1 3)) (define I4 (%1 4))
;(define E1 (%1 1)) (define E2 (%1 2)) (define E3 (%1 3)) (define E4 (%1 4))

(define (%array x s . options)
  ;;array は必ず列主導(byrow=#f)で要素が並べられる
  (let ((nrow (first s))
		(ncol (second s)))
	;;	  (format #t "(matrix ~a nrow:~d ncol:~d byrow:F base:~d)\n" x nrow ncol base)
	(array-transpose
	 (apply array (shape 0 ncol 0 nrow) x)) ))

(define (->list list-or-vector)
  (if (vector? list-or-vector) (vector->list list-or-vector) list-or-vector))

(define (%rbind . rows)
  (let* ((rows* (map ->list rows))
		 (nrow (length rows*))
		 (ncol (length (car rows*))))
;	(format #t ">> rbind(nrow:~d, ncol:~d)\n" nrow ncol)
	(apply array (shape 0 nrow 0 ncol)
		   (apply append (map ->list rows*)))))

(define (%cbind . cols)
  (let* ((cols* (map ->list cols))
		 (nrow (length cols*))
		 (ncol (length (car cols*))))
	(format #t ">> rbind(nrow:~d, ncol:~d)\n" nrow ncol)
	(array-transpose (apply array (shape 0 ncol 0 nrow)
							(apply append cols*)))))

(define (%dim mx) (list (array-length mx 0) (array-length mx 1)))
(define (%nrow mx) (array-length mx 0))
(define (%ncol mx) (array-length mx 1))

(define (%row mx row)
  (list->vector
   (map (cute array-ref mx row <>)
		(iota (%ncol mx)) )))
(define (%col mx col)
  (%t (list->vector
	   (map (cute array-ref mx <> col)
			(iota (%nrow mx)) ))))
(define %elem array-ref)
(define (%find mx elem)
  (call/cc (lambda (ret)
			 (array-for-each-index mx
								   (lambda ix (when (= elem (apply array-ref mx ix)) (ret ix)))
								   )
			 (ret #f))))

(define (%row1 mx row1) (%row mx (- row1 1)))
(define (%col1 mx col1) (%col mx (- col1 1)))
(define (%elem1 mx row1 col1) (%elem mx (- row1 1) (- col1 1)))
(define (%find1 mx elem)
  (let1 found (%find mx elem)
	(if found (map (cut + 1 <>) found) #f)))

(define (matrix? obj) (and (array? obj) (= 2 (array-rank obj))))

(define (%set-diag! mx elems)
  (unless (pair? elems) (set! elems (list elems)))
  (let ([start 0]
		[end (min (array-length mx 0) (array-length mx 1))])
	(let loop ((i start) (el elems))
	  (if (= i end) mx
		  (begin (array-set! mx i i (car el))
				 (loop (+ i 1) (if (null? (cdr el)) elems (cdr el))) )))))

(define (%diag . options)
;  (when (odd? (length options))
;	(set! options (append options (list :dummy))))
  (define (make-diag nrow ncol elems)
;	(format #t "(make-diag nrow:~d ncol:~d elems:~a)\n" nrow ncol elems)
	(let ([start 0]
		  [end (min nrow ncol)])
	  (let1 mx (make-array (shape 0 nrow 0 ncol) 0)
		(%set-diag! mx elems))))
  (define (diag-elements mx)
	(let ([start (min (array-start mx 0) (array-start mx 1))]
		  [end (min (array-end mx 0) (array-end mx 1))])
	  (let loop ((i start) (elems '()))
		(if (= end i) (reverse! elems)
			(loop (+ i 1) (cons (array-ref mx i i) elems))))))

  (if (matrix? (car options))
	  (diag-elements (car options))
	  (receive (args keywords) (omit-keywords options)
		(let-keywords keywords ((nrow #f) (nr #f)
								(ncol #f) (nc #f))
		  ;;(format #t "(diag nrow:~d ncol:~d rest:~a)\n" nrow ncol args))
		  (case (length args)
			[(0) '?]
			[(1) ; (dim) / (elem)
			 (if nrow
				 ;; (elem)
				 (let1 elems (car args)
				   (make-diag nrow (or ncol nrow) elems))
				 ;; (dim)
				 (cond [(number? (car args)) ;; N次元の単位行列
						(let1 dim (car args)
						  ;(identity-array dim)
						  (make-diag (or nrow dim) (or ncol dim) '(1))
						  )]
					   [(pair? (car args)) ;;(length (car args))次元の対角行列
						(let1 dim (length (car args))
						  (make-diag (or nrow dim) (or ncol dim) (car args))
						  )]
					   [else #f]) )]
			[(2 3) ; (elem dim)
			 (cond [(number? (first args)) ;; dim次元の単位行列xelem
					(let ([nrow (second args)]
						  [ncol (last args)])
					  (let1 elems (make-list (min nrow ncol) (first args))
						(make-diag (or nrow dim) (or ncol dim) elems)))]
				   [(pair? (first args)) ;;dim次元の対角行列
					(let ([elems (first args)]
						  [nrow (second args)]
						  [ncol (last args)])
					  (make-diag (or nrow dim) (or ncol dim) elems) )]
				   [else #f]) ]
			[else #\?])
		  ))))

;; トレース
(define (%Tr mx) (apply + (%diag mx)))

;; 三角行列
#;(define (%upper-tri mx . options)
  (let-keyword options ((diag #f))
			   '()))
			   
#;(define (%lower-tri mx . options)
  (let-keyword options ((diag #f))
			   '()))

(define (%set! mx my) ; mx <- my
  )

;;; ムーア-ペンローズの疑似逆行列
;;; (Moore-Penrose pseudo-inverse matrix)
(define (%moore-penrose phi) ;; (3.17)Φ†
  (let1 phiT (%t phi)
	(%*% (%-1 (%*% phiT phi) phiT))))

(define %† %moore-penrose)


(provide "naoyat/matrix")
;;EOF