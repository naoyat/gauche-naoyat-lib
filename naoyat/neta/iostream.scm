(define-module naoyat.neta.iostream
  (export cout
		  ))
(select-module naoyat.neta.iostream)

(define-syntax cout
  (syntax-rules (<< endl)
	((_ << endl) (newline))
	((_ << expr) (display expr))
	((_ << expr << endl) (print expr))
	((_ << expr1 << expr2) (begin (display expr1) (display expr2)))
	((_ << expr1 << expr2 << endl) (print expr1 expr2))
	))

(provide "naoyat/neta/iostream")
;;EOF
