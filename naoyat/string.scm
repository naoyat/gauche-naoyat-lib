(define-module naoyat.string
  (use srfi-13); String

  (export blank?
		  join
		  memqs
		  ))
(select-module naoyat.string)

(define (blank? s)
  (cond ((eq? s #f) #t)
		((null? s) #t)
;		((string? s) (string=? "" s))
		((string? s) (string-null? s))
		(else #f))) ; undef

(define (join string-list)
;  (print "JOIN>" string-list)
  (string-join (remove (lambda (s) (not (string? s))) string-list) " "))

(define (memqs str x)
  (cond [(null? x) #f]
		[(string=? str (car x)) x]
		[else (memqs str (cdr x))]))

(provide "naoyat/string")
;;EOF
