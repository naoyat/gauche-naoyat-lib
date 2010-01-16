;;
;; GD
;;
(define-module naoyat.graph.gd
  (use srfi-1)
  (use graphics.gd)

  (use naoyat.misc) ;; for

  (export make-histogram-png
		  make-graph-png
		  make-graph-image

		  gd-image-string+
		  gd-image-string+i
		  
		  *graph-bar-mode*
		  *graph-default-ink*
		  ))
(select-module naoyat.graph.gd)

(define *graph-bar-mode* #f)
(define *graph-default-ink* '(255 0 0))

(define *regular-font* "/Library/Fonts/Arial.ttf")
(define *italic-font* "/Library/Fonts/Arial Italic.ttf")

(define (make-histogram-png path x-range y-range p proc)
  (let ([x-from (first x-range)] [x-to (second x-range)] [x-step (third x-range)]
		[y-from (first y-range)] [y-to (second y-range)] [y-step (third y-range)])
	(let* ([xcnt (+ 1 (ceiling->exact (/ (- x-to x-from) x-step)))]
		   [ycnt (ceiling->exact (/ (- y-to y-from) y-step))]
		   [x0 22] [xF (+ x0 (* 25 xcnt))]
		   [xmag (/ 25 x-step)] ;positive
		   [yF 6] [y0 (+ yF (* 50 ycnt))]
		   [ymag (/ -50 y-step)] ;negative
		   [w/2 10]
		   [width (+ xF 5)] [height (+ y0 22)])
	  (let* ([im (gd-image-create width height)]
			 [white (gd-image-color-allocate im 255 255 255)]
			 [black (gd-image-color-allocate im 0 0 0)]
			 [gray (gd-image-color-allocate im 128 128 128)]
			 [f (gd-font-get-small)])
		(define (x_tr x) (round->exact (+ x0 13 (* xmag (- x x-from)))))
		(define (y_tr y) (round->exact (+ y0 (* ymag (- y y-from)))))
		
		(for x x-from x-to x-step
			 (let ([x_ (x_tr x)]
				   [y_ (round->exact (y_tr (p x)))])
			   (when (< y_ y0)
				 (gd-image-filled-rectangle im (- x_ w/2) (- y0 1) (+ x_ w/2) y_ gray))
			   (gd-image-line im x_ yF x_ (+ yF 2) black)
			   (gd-image-line im x_ (- y0 2) x_ y0 black)
			   (gd-image-string im f (- x_ 2) (+ y0 2) (format #f "~a" x) black) ))

		(for y y-from y-to y-step
			 (let1 y_ (y_tr y)
			   (gd-image-string im f (- x0 20) (- y_ 7) (format #f "~a" (*. y)) black)
			   (gd-image-line im x0 y_ (+ x0 2) y_ black)
			   (gd-image-line im xF y_ (- xF 2) y_ black) ))

		(gd-image-rectangle im x0 yF xF y0 black)

		(when proc (proc im f x0 xF y0 yF black))

		(save-as im path 'png)
		(gd-image-destroy im)
		))))

(define-macro (make-graph-png path x-range y-range p proc)
  `(make-graph-image ,path ,x-range ,y-range ,p ,proc 'png))

(define (make-graph-image path x-range y-range p proc fmt)
  (define (inf-nan-filter x)
	(cond [(eq? x +nan.0) #f]
		  [(= x +inf.0) 99999]
		  [(= x -inf.0) -99999]
		  [else x]))
  (let ([x-from (first x-range)] [x-to (second x-range)] [x-step (third x-range)]
		[y-from (first y-range)] [y-to (second y-range)] [y-step (third y-range)]
		[p* (compose inf-nan-filter p)])
	(let* ([xcnt (ceiling->exact (/ (- x-to x-from) x-step))]
		   [ycnt (ceiling->exact (/ (- y-to y-from) y-step))]
		   [x0 22] [xF (+ x0 200)]
		   [xmag (/ 200 (- x-to x-from))] ;positive
		   [yF 6] [y0 (+ yF (* 50 ycnt))]
		   [ymag (/ -50 y-step)] ;negative
		   [width (+ xF 5)] [height (+ y0 22)])
	  (let* ([im (gd-image-create width height)]
			 [white (gd-image-color-allocate im 255 255 255)]
			 [black (gd-image-color-allocate im 0 0 0)]
			 [ink (apply gd-image-color-allocate im *graph-default-ink*)]
			 [f (gd-font-get-small)])
		(define (x_tr x) (round->exact (+ x0 (* xmag (- x x-from)))))
		(define (y_tr y) (round->exact (+ y0 (* ymag (- y y-from)))))

		(define (plot-curve fn color)
		  (let ([step (/ (- x-to x-from) 1000)])
			(let loop ((x1 x-from) (y1 (fn x-from)))
			  (let* ([x2 (+ x1 step)] [y2 (fn x2)])
				(when (<= x2 x-to)
				  (when (and y1 y2 (> (* y1 y2) -10))
					(gd-image-line im (x_tr x1) (y_tr y1) (x_tr x2) (y_tr y2) color)
					)
				  (loop x2 y2))))))

		(define (plot-bars fn color)
		  (let ([step (/ (- x-to x-from) 1000)])
			(let loop ((x x-from))
			  (when (<= x x-to)
				(gd-image-line im (x_tr x) y0 (x_tr x) (y_tr (fn x)) color)
				(loop (+ x step))))))

		(define (dispatcher msg)
		  (case msg
			[(plot-curve) (cut plot-curve <> <>)]
			[(color) (cut gd-image-color-allocate im <> <> <>)]
			[(black) black]
			[(ink) ink]
			[else #f]))

		(if *graph-bar-mode*
;;			(plot-curve p* ink))
			(plot-bars p* ink)
			(plot-curve p* ink))

		(for y y-from y-to y-step
			 (let ([y_ (y_tr y)]
				   [y-str (x->string y)])
			   ;;(gd-image-string im f (- x0 20) (- y_ 7) (format #f "~a" (*. y)) black)
			   (gd-image-string im f (- x0 3 (* 5 (string-length y-str))) (- y_ 7) y-str black)
			   (gd-image-line im x0 y_ (+ x0 2) y_ black)
			   (gd-image-line im xF y_ (- xF 2) y_ black) ))

		(for x x-from x-to x-step
			 (let1 x_ (x_tr x)
			   (gd-image-line im x_ yF x_ (+ yF 2) black)
			   (gd-image-line im x_ (- y0 2) x_ y0 black)
			   (gd-image-string im f (- x_ 2) (+ y0 2) (format #f "~a" x) black) ))

		(gd-image-rectangle im x0 yF xF y0 black)

		(when proc (proc im f x0 xF y0 yF dispatcher))

		(save-as im path fmt)
		(gd-image-destroy im)
		))))

(define (gd-image-string+ im f x y s color)
  (gd-image-string-ft im color *regular-font* 10.0 0 x (+ y 10) s))
(define (gd-image-string+i im f x y s color)
  (gd-image-string-ft im color *italic-font* 10.0 0 x (+ y 10) s))

(provide "naoyat/graph/gd")
;;