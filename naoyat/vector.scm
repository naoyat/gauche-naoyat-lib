(define-module naoyat.vector
  (export vector-inc!
		  vector-dec!
		  ))
(select-module naoyat.vector)

(define (vector-inc! vec i . args)
  (let-optionals* args ((delta 1))
	(vector-set! vec i (+ (vector-ref vec i) delta))))

(define (vector-dec! vec i . args)
  (let-optionals* args ((delta 1))
	(vector-set! vec i (- (vector-ref vec i) delta))))

(provide "naoyat/vector")
;;EOF