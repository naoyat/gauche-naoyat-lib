(define-module naoyat.test
  (use gauche.test)
  
  (export ~=
		  every~=
		  ~7= ~8=

		  ask-human
		  have-human-see
		  manual-test*
		  ))
(select-module naoyat.test)

(define *undef* (if #f #f))

(define (~= epsilon x y) (< (abs (- x y)) epsilon))
(define (every~= epsilon xs ys)
;  (every identity (map (cut ~= epsilon <> <>) xs ys)))
  (let loop ((xs xs) (ys ys))
	(if (null? xs) (null? ys)
		(and (~= epsilon (car xs) (car ys)) (loop (cdr xs) (cdr ys))))))

(define ~7= (cut ~= 1e-7 <> <>))
(define ~8= (cut ~= 1e-8 <> <>))

(define (ask-human)
  (display "[ OK(Y) / NG(n) / AGAIN(a) ]? ") (flush)
  (let1 line (read-line)
	(if (zero? (string-length line))
		#t
		(case (string-ref line 0)
		  [(#\Y #\y #\O #\o) #t]
		  [(#\N #\n) #f]
		  [(#\A #\a) *undef*]
		  [else #f]))))

(define (have-human-see proc)
  (let loop ()
	(proc)
	(let1 human-answer (ask-human)
	  (if (undefined? human-answer)
		  (loop)
		  human-answer))))

(define-macro (manual-test* msg proc)
  `(test* ,msg #t (have-human-see ,proc)))

(provide "naoyat/test")
;;EOF