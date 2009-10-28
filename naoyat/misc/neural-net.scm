;;
;; neural network (dag), ver.1
;;
(define-module naoyat.misc.neural-net
  (use srfi-1)

  (use naoyat.list) ; list-set!
  (use naoyat.math) ; logistic-sigmoid
  (use naoyat.math.random)

  (export make-weight
		  weight-value
		  weight-value-set!
		  weight-from-node
		  weight-from-node-set!
		  weight-to-node
		  weight-to-node-set!
		  weight-label
		  weight-desc
		  weight-fore-value

		  make-node
		  node-value
		  node-value-set!
		  node-delta
		  node-delta-set!
		  node-delta-set-with-target!
		  node-from-list
		  node-from-nodes
		  node-func
		  node-deriv-func
		  node-label
		  node-desc
		  node-from-list-append!
		  node-to-list-append!
		  node-foreprop
		  node-backprop

		  connect-nodes
		  get-connection
		  
		  make-neural-network
		  ))
(select-module naoyat.misc.neural-net)

(define (tanh-deriv z) (- 1 (* z z)))
(define (logistic-sigmoid-deriv z) (* z (- 1 z)))
(define (identity-deriv z) 1)

(define (deriv fn)
  (cond
   [(eq? fn tanh) tanh-deriv]
   [(eq? fn logistic-sigmoid) logistic-sigmoid-deriv]
   [(eq? fn identity) identity-deriv]
   [else #f]))

;;
;; weight
;;
(define (make-weight label from-node to-node value)
  (list value from-node to-node label))
(define (weight-value weight) (first weight))
(define (weight-from-node weight) (second weight))
(define (weight-from-node-set! weight node) (set! (second weight) node))
(define (weight-to-node weight) (third weight))
(define (weight-to-node-set! weight node) (set! (third weight) node))
(define (weight-value-sub! weight delta) (set! (first weight) (- (first weight) delta)))
(define (weight-label weight) (fourth weight))
(define (weight-desc weight)
  (format #f "weight ~a from ~a to ~a; value = ~a"
		  (weight-label weight)
		  (node-label (weight-from-node weight))
		  (node-label (weight-to-node weight))
		  (weight-value weight)
		  ))
(define (weight-fore-value weight)
  (* (node-value (weight-from-node weight)) (weight-value weight)))

;;
;; node
;;
(define (make-node label value func); deriv-func)
  (list value 0 '() '() func (deriv func) label))
(define (node-value node) (first node))
(define (node-value-set! node value) (set! (first node) value))
(define (node-delta node) (second node))
(define (node-delta-set! node value) (set! (second node) value))
(define (node-delta-set-with-target! node target) (set! (second node) (- (node-value node) target)))
(define (node-from-list node) (third node))
(define (node-from-nodes node) (map weight-from-node (node-from-list node)))
(define (node-to-list node) (fourth node))
(define (node-to-nodes node) (map weight-to-node (node-to-list node)))
(define (node-func node) (fifth node))
(define (node-deriv-func node) (sixth node))
(define (node-label node)
  (if node (seventh node) "(undefined)"))
(define (node-desc node)
  (define (weight-desc* dir weight)
	(string-append "  " dir " " (weight-desc weight) "\n"))
  (string-append (format #f "node ~s, value = ~a, delta = ~a\n"
						 (node-label node)
						 (node-value node)
						 (node-delta node))
				 (apply string-append (map (cut weight-desc* "<<" <>) (node-from-list node)) )
				 (apply string-append (map (cut weight-desc* ">>" <>) (node-to-list node)) )
				 ))
(define (node-from-list-append! node weight)
  (push! (third node) weight)) ;; third = node-from-list
(define (node-to-list-append! node weight)
  (push! (fourth node) weight)) ;; fourth = node-to-list

(define (node-foreprop node) ; decides output
  (let1 from-list (node-from-list node)
	(if (null? from-list) #f
		(let1 new-value ((node-func node) (apply + (map weight-fore-value from-list)))
		  (node-value-set! node new-value)
		  new-value))))

(define (node-backprop eta node)
  ;;(format #t "(node-backprop ~a)\n" (node-label node))
  (let1 to-list (node-to-list node)
	(if (null? to-list) #f
		(let1 my-value (node-value node)
		  ;; modify weight
		  (for-each (lambda (weight)
					  ;;(print (weight-desc weight) " : ")
					  (let1 diff (* eta my-value (node-delta (weight-to-node weight)))
						;;(format #t " [ ~a -= ~a ]\n" (weight-label weight) diff)
						(weight-value-sub! weight diff)))
					to-list)
		  ;; setting my delta
		  (unless (null? (node-from-list node))
			(node-delta-set! node
							 (* ((node-deriv-func node) (node-value node))
								(apply + (map (lambda (weight)
												(* (weight-value weight)
												   (node-delta (weight-to-node weight))))
											  (node-to-list node))))) ))
		)))

(define (connect-nodes from-node to-node initial-weight)
  (let* ([wlabel (format "w.~a.~a" (node-label to-node) (node-label from-node))]
		 [w (make-weight wlabel from-node to-node initial-weight)])
	(node-to-list-append! from-node w)
	(node-from-list-append! to-node w)
	))
(define (get-connection from-node to-node)
  (find (lambda (weight) (eq? (weight-to-node weight) to-node))
		(node-to-list from-node)))

;;
;; network
;;
;;(use util.toposort)
(define (make-neural-network eta hidden-func output-func . nodecnt-list)
  (let* ([nodecnts (list->vector nodecnt-list)]
		 [layercnt (- (vector-length nodecnts) 1)] ; PRMLの言い方
		 [weight-setter (make-nd-rand 0 1)]
		 [node-list-list (make-list (+ layercnt 1) '())]
		 [input-node-list '()]
		 [output-node-list '()]
		 [nodes '()])

	  (define (nodecnt layer) (vector-ref nodecnts layer))


	  (define (add-node node)
		;;(format #t "add node ~a to ~a\n" (node-label node) (map node-label nodes))
		(push! nodes node))
#;	  (define (topo-sort)
		;; sort topologically
		(set! nodes (topological-sort
					 (map (lambda (node) (cons node (node-to-nodes node))) nodes)
					 eq?))
		(for-each (lambda (node)
					(format #t "T/ ~a >> ~a >> ~a\n"
							(map node-label (node-from-nodes node))
							(node-label node)
							(map node-label (node-to-nodes node))
							))
				  nodes))
	  
	  (define (foreprop input-data)
		(for-each (cut node-value-set! <> <>) input-node-list input-data)
		(for-each (cut node-foreprop <>) nodes)
		)
	  (define (backprop target-data)
		(for-each (cut node-delta-set-with-target! <> <>) output-node-list target-data)
		(for-each (cut node-backprop eta <>) (reverse nodes))
		)
	  
	  (define (learn input target)
		;;(format #t "(learn ~a ~a)\n" input target)
		(foreprop input)
		(backprop target))
	  (define (test input)
		(foreprop input)
		(map node-value output-node-list))
		
	  (define (desc)
		(dotimes (l (+ layercnt 1))
		  (format #t "Layer #~d:\n" l)
		  (for-each (lambda (node) (format #t "  ~a\n" (node-desc node)))
					(list-ref node-list-list l))))

	  (define (init)
		;; input
		(let1 input-node-list* (append (map (lambda (i) (make-node (format "input#~d" i) 0 identity))
											(iota (nodecnt 0)))
									   (list (make-node "bias<0>" 1 #f)))
		  (for-each add-node input-node-list*)
		  (list-set! node-list-list 0 input-node-list*)
		  (set! input-node-list (reverse (cdr (reverse input-node-list*)))))

		;; hidden
		(dotimes (l-1 (- layercnt 1))
		  (let1 l (+ l-1 1)
			(let1 hidden-node-list (append (map (lambda (i) (make-node (format "hidden<~d>#~d" l i) 0 hidden-func))
												(iota (nodecnt l)))
										   (list (make-node (format "bias<~d>" l) 1 #f)))
			  (for-each add-node hidden-node-list)
			  (list-set! node-list-list l hidden-node-list))))

		;; output
		(let1 output-node-list* (map (lambda (i) (make-node (format "output#~d" i) 0 output-func))
									 (iota (nodecnt layercnt)))
		  (for-each add-node output-node-list*)
		  (list-set! node-list-list layercnt output-node-list*)
		  (set! output-node-list output-node-list*))

		(set! nodes (reverse! nodes))
		
		(dotimes (l layercnt)
		  (let ([from (nodecnt l)]
				[to (nodecnt (+ l 1))])
			;(format #t "INIT ~d - ~d\n" from to)
			(dotimes (f (+ (nodecnt l) 1))
			  (dotimes (t (nodecnt (+ l 1)))
				(connect-nodes (list-ref (list-ref node-list-list l) f)
							   (list-ref (list-ref node-list-list (+ l 1)) t)
							   (weight-setter))))
			)))

	  (init)

	  (lambda (m)
		(case m
;		  ((add-nodes) add-nodes)
		  ((desc) desc)
		  ((learn) learn)
		  ((test) test)
		  
		  ;; for testing
;		  ((foreprop) foreprop)
;		  ((backprop) backprop)
		  ))
	  ))

(provide "naoyat/misc/neural-net")
;;EOF