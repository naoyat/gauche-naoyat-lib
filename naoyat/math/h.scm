;;
;; libm の関数はこちら
;;
(define-module naoyat.math.h
  (use c-wrapper)

;  (c-load "math.h" :import 'erf); :import 'erfc)
;  (c-load "math.h" :import 'erfc)
;  (c-load "math.h" :import 'tgamma)
  (c-load "math.h")

  ;(export-all)
  (export erf
		  erfc
		  tgamma Γ
		  ))
(select-module naoyat.math.h)

(define Γ tgamma)

(provide "naoyat/math/h")
;;EOF