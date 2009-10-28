;;
;; neural network (by matrix)
;;
(define-module naoyat.misc.neural-net-matrix
  (use gauche.array)
  (use srfi-1) ; iota
  (use math.const)

  ;(use naoyat.util)
  (use naoyat.matrix)
  (use naoyat.math)
  (use naoyat.math.random)
  
  (export make-nn-simple
   ))
(select-module naoyat.misc.neural-net-matrix)

;(define h tanh)
;(define (h/ x) (- 1 (square (h x))))
(define (1-z^2 z) (- 1 (* z z)))
(define (one anything) 1)

(define *debug* #t)
(define (debug fmt . args)
  (when *debug* (apply format *debug* fmt args)))

(define (pp mx)
  (when *debug*
	(with-output-to-port (current-output-port)
	  (lambda () (matrix-print mx)))))

(define (make-nn-simple eta default-weight-setter . nodecnt-list)
  (debug "(make-nn-simple eta=~d nodecnt=~a)\n" eta nodecnt-list)

  (let* ([nodecnts (list->vector nodecnt-list)]
		 [layercnt (- (vector-length nodecnts) 1)] ; PRMLの言い方
		 [z (make-vector (+ layercnt 1))]
		 [w (make-vector (+ layercnt 1))]
		 [d (make-vector (+ layercnt 1))]
		 [s-hidden tanh]
		 [s-output identity];logistic-sigmoid]
		 )

	(define (nodecnt layer) (vector-ref nodecnts layer))

	(define (description)
	  (debug "(description)\n")
	  (debug "~d input --> ~d hidden layer[s] ~a --> ~d output\n"
			 (car nodecnt-list)
			 (- layercnt 1) (drop-right (cdr nodecnt-list) 1)
			 (last nodecnt-list) )
	  (print "z: "z);(dump-z)
	  (print "w: "w);(dump-w)
	  (print "d: "d);(dump-d)
	  )

	(define (foreprop input)
	  (debug "(foreprop ~a)\n" input)
	  (set-input! input)
	  ;(debug "  0) ") (dump-z)
	  (for l 1 layercnt 1
		   (debug "w~d:\n" l)
		   (pp (vector-ref w l))
		   (debug " x z~d:~a" (- l 1) (vector-ref z (- l 1)))
		   ;(pp (vector-ref z (- l 1)))
		   (let1 a (array->list (%*% (vector-ref w l) (%t (vector-ref z (- l 1)))))
			 (vector-set! z l (cons 1 (map (if (= l layercnt) s-output s-hidden) a)))
			 (debug " => ~a\n" a)
			 (debug "  ;; z~d:~a\n" l (vector-ref z l))
			 )))

	(define (vector**= vec ix op . args)
	  (vector-set! vec ix (apply op (vector-ref vec ix) args)))

	(define (backprop target-output)
	  (debug "(backprop t:~a)\n" target-output)
	  ;; d/k = y/ - t/
	  (vector-set! d layercnt
				   (map - (cdr (vector-ref z layercnt)) target-output))
	  (debug "d(~d)=~a\n" layercnt (vector-ref d layercnt))
	  (for l layercnt 1 -1
		   (debug "LAYER ~d -> ~d\n" l (- l 1))
		   (let* ([dk (vector-ref d l)]
				  [dw (%*% (%t dk) (apply %c (vector-ref z (- l 1))) eta )])
			 ;; w/j -= eta.d/k.z/j
			 (vector**= w l %- dw)
			 (debug "w[~d] -=\n" l) (pp dw)
			 (debug "     =>\n") (pp (vector-ref w l))
			 ;; d/j = (1-z^2).w/.d/k
			 (when (< 1 l)
			   (let1 dj (cdr (map *
								  (map 1-z^2 (vector-ref z (- l 1)))
								  (array->list (%*% (%t (vector-ref w l)) (%t dk)))))
				 (debug "d(~d)=~a\n" (- l 1) dj)
				 (vector-set! d (- l 1) dj) )))))
	  
	;; 入力値のセット／出力値の取り出し
	(define (set-input! input)
	  (debug "(set-input! ~a)\n" input)
	  (vector-set! z 0 (cons 1 input)))

	(define (get-output)
	  (debug "(get-output)") (flush)
	  (let1 output (cdr (vector-ref z layercnt))
		(debug " => ~a\n" output)
		output))

    (define (test x/)
	  (foreprop x/)
	  (get-output))
    (define (learn x/ t/)
	  (debug "\n")
	  (foreprop x/)
	  (backprop t/))

	;; initialize
	(define (init)
	  (for l 1 layercnt 1
		   (vector-set! z l (make-list (+ (nodecnt l) 1) 1))
		   (vector-set! w l (%matrix (make-list-with-generator
									  (* (nodecnt l) (+ (nodecnt (- l 1)) 1))
									  default-weight-setter)
									 :nrow (nodecnt l) :ncol (+ (nodecnt (- l 1)) 1)))
		   (vector-set! d l (make-vector (nodecnt l) 0))
		   )
	  ;(array-for-each-index w (lambda (l i j) (array-set! w l i j (default-weight-setter))))
	  (description)
	  )

	(init)

	(lambda (m)
	  (case m
		((learn) learn)
		((test) test)
		((desc description) description)
		))))

(provide "naoyat/misc/neural-net-matrix")
;;EOF