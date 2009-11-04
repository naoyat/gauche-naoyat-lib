;;;
;;;   naoyat.printf - naoya_t's printf/sprintf, supposed to be used mainly for debug
;;;                   (you might want to use slib/printf.scm instead...)
;;;
;;; -------------------------------------------------------------------------------------
;;;   Example:
;;;
;;;   (printf "%03d" 7) ==> "007"
;;;   (printf "%x" 15) ==> "f"
;;;   (printf "|%5s,%-5s|" "abc" "def") ==> "|  abc,def  |"
;;;
;;;   Please see 'test/printf.scm' for detail.
;;; -------------------------------------------------------------------------------------
(define-module naoyat.printf
  (export printf
		  sprintf
		  ))
(select-module naoyat.printf)

(define (log10 x) (/ (log x) (log 10.0)))

(define (sprintf fmt . args)
  (let1 out (open-output-string)
	(let loop ((fmt fmt) (args args))
	  (define (ret) (display fmt out) (get-output-string out))
	  (let1 match (#/%(?<flag>[-+# 0]*)(?<num>[1-9][0-9]*|\*)?(\.(?<below>[0-9]+|\*))?(?<type>[%diouxXfeEgGbcs])/ fmt)
	    (if match
			(let* ([flags (if (match 'flag) (string->list (match 'flag)) '())]
				   [num (match 'num)] ;str/指定がなければ#f. ※0.8.11までは#fではなく""が返る
				   [below (match 'below)] ;str/指定がなければ#f. ※0.8.11までは#fではなく""が返る
				   [type (string->symbol (match 'type))]
				   [consumes-an-arg? (not (eq? '% type))]
				   [flush-left? (memq #\- flags)]
				   [signed? (memq #\+ flags)]
				   [zero-pad? (memq #\0 flags)])

			  (when (and num (string=? num "*"))
				(set! num (x->string (car args)))
				(set! args (cdr args)))
			  (when (and below (string=? below "*"))
				(set! below (x->string (car args)))
				(set! args (cdr args)))

			  (if (and consumes-an-arg? (null? args))
				  (ret) ;;; arguments-exhausted
				  (let1 arg (if consumes-an-arg? (car args) 'not-eaten)
					(define (format-int-value type)
					  (let1 %value (if (integer? arg) arg (x->integer (truncate (x->number arg))))
						(if flush-left?
							(let1 %fmt (string-append "~" type)
							  (format (string-append "~" (or num "") "a") (format %fmt %value)))
							(let1 %fmt (string-append "~" (or num "") "," (if zero-pad? "'0" "")
													  (if signed? "@" "") type)
							  (format %fmt %value)))))

					(define (format-float-value); type)
					  (let* ([%value (if (real? arg) arg (x->number arg))]
							 [%e-offset (x->integer (floor (log10 (abs %value))))]
							 [%precision (case type
										   [(f)
											;;(if (string=? "" below) 6 (x->integer below))) ; < 0.8.12
											(if below (x->integer below) 6)]
										   [(e E)
											(cond ((> %e-offset 0) ;;; sorry i'm using (set!)
												   (set! %value (/ %value (expt 10 %e-offset))))
												  ((< %e-offset 0)
												   (set! %value (* %value (expt 10 (abs %e-offset)))))
												  (else #t))
											;;(if (string=? "" below) 6 (x->integer below))) ;< 0.8.12
											(if below (x->integer below) 6)]
										   ;;(else (if (string=? "" below) 12 (x->integer below))))) ;< 0.8.12
										   (else (if below (x->integer below) 12)))])
						(let* ([%int (x->integer (if (= 0 %precision)
													 (round %value)
													 (truncate %value)))]
							   [%fract (abs (- %value %int))]
							   [%rounded (x->integer (round (+ (expt 0.1 (+ %precision 1))
															   (* (expt 10 %precision) %fract))))]
							   [%str (if (= 0 %precision)
										 (format "~d" %int)
										 (format (string-append "~d.~" (format "~d" %precision) ",'0d")
												 %int %rounded))]
							   [%-str (if (and (= 0 %int) (< %value 0))
										  (string-append "-" %str)
										  %str)]
							   [%f-fmt (string-append "~" (or num "") ;"," (if zero-pad? "'0" "")
													  (if flush-left? "" "@") "a")]
							   [%e-fmt (string-append %f-fmt
													  "~a" ; [eE]
													  (if (< %e-offset 0) "-" "+")
													  "~2,'0d")])
						  (case type
							[(f)
							 (format %f-fmt %-str)]
							[(g G)
							 (format %f-fmt (regexp-replace #/0+$/ %-str ""))]
							[(e E)
							 (format %e-fmt %-str type (abs %e-offset))] ))))
					
					(display (match 'before) out)
					;; warnings
;					(case type
;					  ((d i)
;					   (when (not (integer? arg))
;							 (print "warning: %~a requires <integer>" type) ))
;					  ((b o u X x c)
;					   (when (not (and (integer? arg) (< 0 arg)))
;							 (print "warning: %~a requires <unsigned integer>" type) ))
;					  ((f e E g G)
;					   (when (not (real? arg))
;							 (print "warning: %~a requires <real>" type)))
;;					  ((b)) ; we use %b for unsigned binary
;					  ((s)
;					   (when (not (string? arg))
;							 (format "warning: %s requires <string>")))
;					  )
					(display (case type
							   [(d b o x X) ;;; remove 'b' if you don't want to support BINARY
								(format-int-value (match 'type))]
							   [(i u) ; signed/unsigned decimal
								(format-int-value "d")]
							   [(f) ;float ; [-]ddd.ddd
								(format-float-value)]
							   [(e E) ;'not-supported) ; [-]d.ddde+-dd
								(format-float-value)]
							   [(g G) ; 'not-supported) ;
								(format-float-value)]
							   ;;[(b) 'not-supported] ; backslash-escape seq
							   [(c) ; first-char
								(cond ((integer? arg)
									   (string (integer->char arg)))
									  ((string? arg)
									   (string-ref arg 0))
									  (else 
									   (string (integer->char (truncate (x->number arg))))) )]
							   [(s) ; 'str
								(let1 %s-fmt (string-append "~"
															(format "~a~a"
																	(or num "")
																	(if flush-left? "" "@")
																	)
															"a")
									  (format %s-fmt arg))]
							   [(%) "%"]) out)
					(loop (match 'after)
						  (if consumes-an-arg? (cdr args) args))
					)))
			(ret))))))

(define (printf fmt . args)
  (let1 s (apply sprintf fmt args)
	(display s)
	(string-length s)))

(provide "naoyat/printf")
;;EOF
