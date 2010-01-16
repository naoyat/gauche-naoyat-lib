(define-module naoyat.list
  (use naoyat.math.random)

  (export fringe
		  assoc-value

		  make-list-with-generator
		  make-random-int-list
		  make-list*
		  ))
(select-module naoyat.list)

(define (fringe tree)
  (let loop ((rest tree) (result '()))
    (if (null? rest)
        result
        (loop (cdr rest)
              (append result
					  ((if (pair? (car rest)) fringe list)
					   (car rest)))))))


(define (assq-value key assoc-list)
  (cdr (or (assq key assoc-list) '(#f . #f))))
(define (assv-value key assoc-list) 
  (cdr (or (assv key assoc-list) '(#f . #f))))
(define (assoc-value key assoc-list) 
  (cdr (or (assoc key assoc-list) '(#f . #f))))


(define (make-list-with-generator len gen-proc)
  (let loop ([i (- len 1)] [res '()])
    (if (< i 0) (reverse! res)
        (loop (- i 1) (cons (gen-proc i) res)))))

(define (make-random-int-list len min max)
  (define (rnd x) (random-integer min max))
  (make-list-with-generator len rnd))


;; fillがアトムの場合、SRFI-1の(make-list)と同じ振舞い
;; fillがリストの場合、fillのパターンを繰り返して埋めた長さlenのリストを生成
(define (make-list* len fill)
  (if (pair? fill)
	  (let loop ((i len) (l '()) (items fill))
		(if (zero? i) (reverse! l)
			(loop (- i 1)
				  (cons (car items) l)
				  (if (null? (cdr items)) fill (cdr items)))))
	  (make-list len fill)))

;;
;; lis[k] = val
;;
(define (list-set! lis k val)
  (set-car! (list-tail lis k) val))

(provide "naoyat/list")
;;EOF