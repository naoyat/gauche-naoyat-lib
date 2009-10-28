(define-module naoyat.misc
  (export memoize
		  for
		  ))
(select-module naoyat.misc)

;;
;; メモ化
;;
(define (memoize proc)
  (let1 fh (make-hash-table 'equal?)
	(lambda args
	  (or (hash-table-get fh args #f)
		  (let1 val (apply proc args)
			(hash-table-put! fh args val)
			val)))
	))

;;
;; forマクロ
;;
(define-macro (for var from to step . body)
  ;; for (var=from; var<=to; var+=step) { body ...; }
  (let ((val (gensym))
		(op (gensym)))
	`(do ((,val (if #f #f))
		  (,var ,from (+ ,var ,step))
		  (,op (if (< 0 ,step) > <)))
		 ((,op ,var ,to) ,val)
	   (set! ,val (begin ,@body)))))

(provide "naoyat/misc")
