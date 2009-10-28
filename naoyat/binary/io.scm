(define-module naoyat.binary.io
  (use gauche.uvector)
  (use binary.io) ;default-endianの設定とか

  (export read-f80
		  write-f80
		  ))
(select-module naoyat.binary.io)

; 80 bit IEEE Standard 754 floating point number
(define (read-f80)
  (let1 vec (make-u8vector 10 0)
	(if (eof-object? (read-block! vec)) 0
		(let* ([buf0 (u8vector-ref vec 0)]
			   [sign (if (logbit? 7 buf0) -1 1)]
			   [exp (- (logior (ash (logand buf0 #x7f) 8) (u8vector-ref vec 1)) 16383)])
		  (let loop ((ix 2) (value 0) (ofs 7))
			(if (= ix 10) (* sign value)
				(loop (+ ix 1)
					  (+ value (ash (u8vector-ref vec ix) (- exp ofs)))
					  (+ ofs 8))))))))

(define (write-f80 value)
  (let ([vec (make-u8vector 10 0)] [sign-mask 0] [exp 16383])
	(when (< value 0)
	  (set! sign-mask 0x8000)
	  (set! value (- value)))
	(let* ([log (/ (log value) (log 2))]
		   [exp (floor->exact log)]
		   [body (/ value (expt 2 exp))]
		   [first16 (logior sign-mask (+ 16383 exp))])
	  (u8vector-set! vec 0 (ash first16 -8))
	  (u8vector-set! vec 1 (logand first16 255))
	  (let loop ([body (* body 128)] [i 2])
		(when (< i 10)
		  (let1 b (floor body)
			(u8vector-set! vec i b)
			(loop (* (- body b) 256) (+ i 1))) ))
	  (write-block vec)))) ; u8vector

(provide "naoyat/binary/io")
