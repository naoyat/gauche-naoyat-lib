;;
;; normal (Gaussian) distribution
;;
(use naoyat.math.random)
(use naoyat.vector) ; vector-inc! vector-dec!

(use gauche.test)
(test-start "random")


;;
;; Box-Muller法
;;
(test-section "Box-Muller")

(define nd-rand (make-nd-rand 50 10))
(define cnt (make-vector 101 0))

(dotimes (i 1000000)
  (let1 x (round->exact (nd-rand))
	(when (<= 0 x 100)
	  (vector-inc! cnt x))
		))

(dotimes (i 101)
  (format #t "~3d: ~a\n" i (make-string (quotient (vector-ref cnt i) 500) #\*)))


(test-end)

