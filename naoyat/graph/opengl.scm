;;
;; OpenGL
;;
(define-module naoyat.graph.opengl
  (use gl)
  (use gl.glut)

  (export draw-char
		  ))
(select-module naoyat.graph.opengl)

;; body

(define *color-scale* 1.414213562)
(define *draw-mode* GL_LINE_LOOP)

(define (draw-char x y z ch)
  (gl-raster-pos (- (* xmag x) 0.02)
                 (- (* ymag y) 0.02)
				 (- (* zmag z) 0.02) )
  (glut-bitmap-character GLUT_BITMAP_8_BY_13 (char->integer ch)))

(define (draw-string x y z str)
  (gl-raster-pos (- (* xmag x) 0.02)
                 (- (* ymag y) 0.02)
                 (- (* zmag z) 0.02))
  (for-each (cut glut-bitmap-character GLUT_BITMAP_8_BY_13 <>)
            (map char->integer (string->list str))))

(define (draw-line x1 y1 z1 x2 y2 z2)
  (gl-begin* GL_LINES
             (gl-vertex (* xmag x1) (* ymag y1) (* zmag z1))
             (gl-vertex (* xmag x2) (* ymag y2) (* zmag z2)) ))

(define (draw-circle cx cy cz e0x e0y e0z e1x e1y e1z)
  (gl-begin* GL_LINE_LOOP
			 (dotimes (i 100)
			   (let* ([theta (* 2 pi (/ i 100))]
					  [cos_ (cos theta)]
					  [sin_ (sin theta)])
				 ;; c + e0.cos(theta) + e1.sin(theta)
				 (gl-vertex (* xmag (+ cx (* e0x cos_) (* e1x sin_)))
							(* ymag (+ cy (* e0y cos_) (* e1y sin_)))
							(* zmag (+ cz (* e0z cos_) (* e1z sin_))) )))))

(define (color z)
  (let1 th *color-scale*
    (cond [(< z 0)
           (list 0 0 1)] ;; blue
          [(< z (/ th 2)) ;; blue -> 
           (let1 t (/ z (/ th 2))
             (list (interpolate 0 1 t)
                   (interpolate 0 1 t)
                   (interpolate 1 0 t)
                   ))]
          [(< z th)
           (let1 t (/ (- z (/ th 2)) (/ th 2))
             (list (interpolate 1 1 t)
                   (interpolate 1 0 t)
                   (interpolate 0 0 t)
                   ))]
          [else (list 1 0 0)] )))

(define (draw-quads-f f x0 y0 x1 y1 x2 y2 x3 y3)
  (let ([z0 (f x0 y0)]
        [z1 (f x1 y1)]
        [z2 (f x2 y2)]
        [z3 (f x3 y3)])
    (let1 zm (/ (+ z0 z1 z2 z3) 4)
      (apply gl-color (color zm))
      (gl-begin* *draw-mode*
				 (gl-vertex (* xmag x0) (* ymag y0) (* zmag z0))
				 (gl-vertex (* xmag x1) (* ymag y1) (* zmag z1))
				 (gl-vertex (* xmag x2) (* ymag y2) (* zmag z2))
				 (gl-vertex (* xmag x3) (* ymag y3) (* zmag z3)) ))))

(define (draw-quads x0 y0 z0 x1 y1 z1 x2 y2 z2 x3 y3 z3)
  (let1 zm (/ (+ z0 z1 z2 z3) 4)
	(apply gl-color (color zm))
	(gl-begin* *draw-mode*
			   (gl-vertex x0 y0 z0)
			   (gl-vertex x1 y1 z1)
			   (gl-vertex x2 y2 z2)
			   (gl-vertex x3 y3 z3) )))

(define (draw-triangles-f f x0 y0 x1 y1 x2 y2)
  (let ([z0 (f x0 y0)]
        [z1 (f x1 y1)]
        [z2 (f x2 y2)])
    (let1 zm (/ (+ z0 z1 z2) 4)
      (apply gl-color (color zm))
      (gl-begin* *draw-mode*
				 (gl-vertex (* xmag x0) (* ymag y0) (* zmag z0))
				 (gl-vertex (* xmag x1) (* ymag y1) (* zmag z1))
				 (gl-vertex (* xmag x2) (* ymag y2) (* zmag z2)) ))))

(define (draw-triangles x0 y0 z0 x1 y1 z1 x2 y2 z2)
  (let1 zm (/ (+ z0 z1 z2) 4)
	(apply gl-color (color zm))
	(gl-begin* *draw-mode*
			   (gl-vertex (* xmag x0) (* ymag y0) (* zmag z0))
			   (gl-vertex (* xmag x1) (* ymag y1) (* zmag z1))
			   (gl-vertex (* xmag x2) (* ymag y2) (* zmag z2)) )))

(define (on-display) ;; please override this
  (gl-clear GL_COLOR_BUFFER_BIT)
  ;; do nothing
  (gl-flush))

(define (on-keydown key x y)
  (when (= key 27) (exit 0))
;  (let1 ch (integer->char key)
;  (glut-post-redisplay)
  )

(define (on-reshape w h)
  )

(define (on-init window-title)
  (glut-init-display-mode GLUT_RGBA)
  (glut-init-window-size 400 400)
  (glut-init-window-position 10 10)
  (glut-create-window window-title)

  (gl-clear-color 0.0 0.0 0.0 0.0) ;;1.0 1.0 1.0 1.0))
  (gl-matrix-mode GL_PROJECTION)
  (gl-enable-client-state GL_VERTEX_ARRAY)
  (glu-perspective 60 1 0.1 20)
  )

(define (gl-main args)
  (glut-init args)

  (on-init (car args))

  (glut-keyboard-func on-keydown)
  (glut-reshape-func on-reshape)
  (glut-display-func on-display)
  
  (glut-main-loop)
  0)

(provide "naoyat/graph/opengl")
;;EOF
