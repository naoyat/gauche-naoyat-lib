(use gauche.test)
(test-start "cl : Common Lisp-like utility functions")

(use srfi-1) ; for macros

(use naoyat.cl)
(test-module 'naoyat.cl)

(test-section "last")
(test* "(last '(1 2 3 4))" '(4) (cl:last '(1 2 3 4)))
(test* "(last '(1 2 3 4) 1)" '(4) (cl:last '(1 2 3 4) 1))
(test* "(last '(1 2 3 4) 2)" '(3 4) (cl:last '(1 2 3 4) 2))
(test* "(last '(1 2 3 4) 3)" '(2 3 4) (cl:last '(1 2 3 4) 3))

(test-section "cl:atom")
(test* "(atom 1)" #t (cl:atom 1))
(test* "(atom (a))" #f (cl:atom '(a)))
(test* "(atom ())" #t (cl:atom cl:nil)) ;; because nil

(test-section "atom?")
(test* "(atom? 1)" #t (atom? 1))
(test* "(atom? (a))" #f (atom? '(a)))
(test* "(atom? ())" #f (atom? cl:nil))

(test-section "random")
(let1 outside 0
  (dotimes (i 10000)
	(unless (<= 0 (cl:random 5) 4) (inc! outside)))
  (test* "0 <= (random 5) <= 4" 0 outside))


(test-section "merge")
(test* "(1 3 4 6 7), (2 5 8), #'<" '(1 2 3 4 5 6 7 8) (cl:merge 'list '(1 3 4 6 7) '(2 5 8) <))
(test* "\"BOY\", \"nosy\", #'char-lessp" "BnOosYy" (cl:merge 'string "BOY" "nosy" cl:char-lessp))

(test-section "cl:subst")
(test* "(= (+ (* 3 x) y) 12), x->2" '(= (+ (* 3 2) y) 12) (cl:subst 2 'x '(= (+ (* 3 x) y) 12)))
(test* "(= (+ (* 3 x) y) 12), y->6" '(= (+ (* 3 x) 6) 12) (cl:subst 6 'y '(= (+ (* 3 x) y) 12)))

(test-section "cl:sublis")
(test* "((?y . 40) (?x . people)), (= ?x ?y))"
	   '(= people 40)
	   (cl:sublis '((?y . 40) (?x . people)) '(= ?x ?y)))
(test* "((?y . advertisements) (?x . 20)), (* (/ ?x 100) ?y))"
	   '(* (/ 20 100) advertisements)
	   (cl:sublis '((?y . advertisements) (?x . 20)) '(* (/ ?x 100) ?y)))

(test-section "cl:car")
(test* "(a)" 'a (cl:car '(a)))
(test* "(a b)" 'a (cl:car '(a b)))
(test* "(a . b)" 'a (cl:car '(a . b)))
(test* "()" cl:nil (cl:car '()))

(test-section "cl:cdr")
(test* "(a)" '() (cl:cdr '(a)))
(test* "(a b)" '(b) (cl:cdr '(a b)))
(test* "(a . b)" 'b (cl:cdr '(a . b)))
(test* "()" cl:nil (cl:cdr '()))

(test-section "cl:fourth")
(test* "(1 2 3 4 5)" 4 (cl:fourth '(1 2 3 4 5)))
(test* "(1 2 3 4)" 4 (cl:fourth '(1 2 3 4)))
(test* "(1 2 3)" cl:nil (cl:fourth '(1 2 3)))
(test* "(1 2)" cl:nil (cl:fourth '(1 2)))
(test* "(1)" cl:nil (cl:fourth '(1)))
(test* "()" cl:nil (cl:fourth '()))

(test-section "cl:fifth")
(test* "(1 2 3 4 5)" 5 (cl:fifth '(1 2 3 4 5)))
(test* "(1 2 3 4)" cl:nil (cl:fifth '(1 2 3 4)))
(test* "(1 2 3)" cl:nil (cl:fifth '(1 2 3)))
(test* "(1 2)" cl:nil (cl:fifth '(1 2)))
(test* "(1)" cl:nil (cl:fifth '(1)))
(test* "()" cl:nil (cl:fifth '()))

(test-section "cl:remove")
(test* "" '(1 2 1 3 5) (cl:remove 4 '(1 2 4 1 3 4 5)))
(test* "" '(1 2 1 3 4 5) (cl:remove 4 '(1 2 4 1 3 4 5) :count 1))
;(test* "" '(1 2 4 1 3 5) (cl:remove 4 '(1 2 4 1 3 4 5) :count 1 :from-end #t))

(test* "" '(4 3 4 5) (cl:remove 3 '(1 2 4 1 3 4 5) :test >))
;(test* "" '(2 4 4) (cl:remove-if odd? '(1 2 4 1 3 4 5)))
;(test* "" '(1 2 4 1 3 5) (cl:remove-if even? '(1 2 4 1 3 4 5) :count 1 :from-end t))

(test-section "cl:nconc")
(define x '(a b c))
(define y '(d e f))
(test* "(nconc x:(a b c) y:(d e f))" '(a b c d e f) (cl:nconc x y))
(test* ";; x:(a b c) => x:(a b c d e f)" '(a b c d e f) x)
;(print x) (print y)


(test-section "cl:mapcar")
(test* "(mapcar #'abs '(3 -4 2 -5 -6))" '(3 4 2 5 6)
	   (cl:mapcar abs '(3 -4 2 -5 -6)))
(test* "(mapcar #'cons '(a b c) '(1 2 3))" '((a . 1) (b . 2) (c . 3))
	   (cl:mapcar cons '(a b c) '(1 2 3)))

(test-section "cl:maplist")
(test* "(maplist #'(lambda (x) (cons 'foo x)) '(a b c d))"
	   '((foo a b c d) (foo b c d) (foo c d) (foo d))
	   (cl:maplist (lambda (x) (cons 'foo x))
				   '(a b c d)))
(test* "(maplist #'(lambda (x) (if (member (car x) (cdr x)) 0 1))) '(a b a c d b c)"
	   '(0 0 1 0 1 1 1) 
	   (cl:maplist (lambda (x) (if (member (car x) (cdr x)) 0 1))
				   '(a b a c d b c)))

(test-section "cl:mapcan")
(test* "" '(1 3 4 5) (cl:mapcan (lambda (x) (if (number? x) (list x) '())) '(a 1 b c 3 4 d 5)))

(test-section "cl:push")
(define p0 '())
(test* "" '() p0)
(test* "push 1 p0" '(1) (cl:push 1 p0))
(test* "" '(1) p0)
(test* "push 2 p0" '(2 1) (cl:push 2 p0))
(test* "" '(2 1) p0)
(test* "push 3 p0" '(3 2 1) (cl:push 3 p0))
(test* "" '(3 2 1) p0)
(test* "push 3 p0" '(3 3 2 1) (cl:push 3 p0))
(test* "" '(3 3 2 1) p0)
(test* "push 2 p0" '(2 3 3 2 1) (cl:push 2 p0))
(test* "" '(2 3 3 2 1) p0)
(test* "push 1 p0" '(1 2 3 3 2 1) (cl:push 1 p0))
(test* "" '(1 2 3 3 2 1) p0)

(test-section "cl:pushnew")
(set! p0 '())
(test* "" '() p0)
(test* "pushnew 1 p0" '(1) (cl:pushnew 1 p0))
(test* "" '(1) p0)
(test* "pushnew 2 p0" '(2 1) (cl:pushnew 2 p0))
(test* "" '(2 1) p0)
(test* "pushnew 3 p0" '(3 2 1) (cl:pushnew 3 p0))
(test* "" '(3 2 1) p0)
(test* "pushnew 3 p0" '(3 2 1) (cl:pushnew 3 p0))
(test* "" '(3 2 1) p0)
(test* "pushnew 2 p0" '(3 2 1) (cl:pushnew 2 p0))
(test* "" '(3 2 1) p0)
(test* "pushnew 1 p0" '(3 2 1) (cl:pushnew 1 p0))
(test* "" '(3 2 1) p0)


(test-section "cl:some")
(test* "" '() (cl:some identity '(() ())))
(test* "" 'a (cl:some identity '(() a () b)))


(test-end)
