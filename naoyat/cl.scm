;;
;; cl-emu
;;
(define-module naoyat.cl
  (use srfi-1)
  (use srfi-27)
  (use gauche.sequence)

  (export cl:last
		  cl:elt
		  cl:random
		  cl:consp
		  cl:listp
		  cl:nil
		  cl:atom
		  atom?
		  cl:rest
		  cl:assoc
		  cl:princ
		  cl:copy-tree
		  cl:subst
		  cl:sublis
		  cl:subsetp
		  cl:some
		  cl:every
		  cl:push ;macro
		  cl:pushnew ;macro

		  consp
		  listp
		  rest
		  princ
		  copy-tree
		  subst
		  sublis
		  some
		  push
		  subsetp

		  cl:and
		  nthcdr
		  cl:nth
		  cl:position
		  
		  funcall
		  cl:apply

		  progv ;macro
		  cl:merge
		  cl:char-lessp

		  cl:thru
		  scheme:thru

		  cl:find
		  cl:setf ;macro
		  cl:remove-if
		  cl:remove-if-not
		  cl:find-all-if
		  multiple-value-bind
		  cl:copy-list

		  cl:delete
		  cl:remove

		  cl:if ;macro
		  cl:when ;macro
		  cl:unless ;macro
		  cl:car
		  cl:cdr
		  cl:remove-duplicates
		  cl:delete-duplicates

		  cl:union
		  cl:nunion
		  cl:set-difference
		  cl:nset-difference
		  cl:sort
		  cl:stable-sort

		  cl:fourth
		  cl:fifth
		  
		  cl:nconc ;macro
		  cl:adjoin

		  cl:mapcar
		  mapcar
		  cl:maplist
		  cl:mapc
		  mapc
		  cl:mapl
		  cl:mapcan ;macro
		  mapcan
		  cl:mapcon ;macro
		  mapcon
		  cl:count-if ;macro
		  
		  ))
(select-module naoyat.cl)

(define %ht (make-hash-table 'equal?))
(define (%get sym key)
  (hash-table-get %ht (cons sym key) #f));;(if #f #f)))
(define (%set! sym key val)
  (hash-table-put! %ht (cons sym key) val)
  val)

(define (cl:last lis . a)
  (if (null? a)
      (last-pair lis)
      (take-right lis (car a)) ))

(define cl:elt list-ref)

(random-source-randomize! default-random-source)
(define (cl:random number) ;; [0..num)
  (random-integer number))

(define cl:consp pair?)
(define (cl:listp x) (or (pair? x) (null? x)))
(define cl:nil '())
(define (cl:atom exp) (not (pair? exp)))
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define cl:rest cdr)

(define (cl:assoc k l)
  (or (assoc k l) '()) )

(define cl:princ display);;とりあえず

;;; tree
(define (cl:copy-tree tree)
  (cond [(null? tree) '()]
        [(pair? tree) (cons (car tree)
                            (copy-tree (cdr tree)))]
        [else tree]
        ))

#;(define (cl:subst new old tree)
  (define (replace elem)
    (if (eq? elem old) new elem))
  (define (sub tree)
    (cond [(null? tree) '()]
          [(pair? tree) (cons (replace (car tree))
                              (sub (cdr tree)))]
          [else (lookup tree)]))
  (sub tree))
(define (cl:subst new old tree)
  (define (subst exp)
	(cond [(null? exp) '()]
		  [(pair? exp) (cons (subst (car exp))
							 (subst (cdr exp)))]
		  ;;[(atom? exp) (if (eq? old exp) new exp)]
		  [else (if (eq? old exp) new exp)] ))
  (subst tree))

(define (cl:sublis subst-list tree)
  (define (lookup elem)
    (let1 subst (assoc elem subst-list)
      (if subst (cdr subst) elem)))
  (define (sub exp)
    (cond [(null? exp) '()]
		  ;[(atom? exp) (lookup exp)]
          [(pair? exp) (cons (sub (car exp))
							 (sub (cdr exp)))]
		  [else (lookup exp)]))
  ;(format #t "(cl:sublis subst-list:~a tree:~a)\n" subst-list tree)
  (sub tree))

(define (cl:subsetp list1 list2 . args)
  (let-keywords* args ((test eq?)
					   (test-not #f)
					   (key #f))
	#;(format #t "subsetp ~a\n   ~a ...\n" list1 list2)
	(if key
		(lset<= (if test-not (lambda (x y) (not (test-not x y))) test)
				(map key list1) (map key list2))
		(lset<= (if test-not (lambda (x y) (not (test-not x y))) test)
				list1 list2)
		)))

(define (cl:some proc lis)
  (scheme:thru (any (compose cl:thru proc) lis)))
(define (cl:every proc lis)
  (scheme:thru (every (compose cl:thru proc) lis)))

(define-macro (cl:push item place)
  `(begin (push! ,place ,item)
          ,place))

(define-macro (cl:pushnew item place . args)
  (let-keywords* args ((test eqv?)
					   (test-not #f)
					   (key #f))
	(when test-not (set! test (complement test-not)))
	`(begin
	   (unless (member ,item ,place ,test)
		 (push! ,place ,item))
	   ,place)))
		 
;;(define (cl:member item list 
;;srfi-1でmemberは=関数をオプションで取れるo

;; 名前がかぶらなさそうなやつね
(define consp cl:consp)
(define listp cl:listp)
;(define atom cl:atom)
;(define cl:atom atom?)
(define rest cl:rest)
(define princ cl:princ)

(define copy-tree cl:copy-tree)
(define subst cl:subst)
(define sublis cl:sublis)

(define some cl:some)
(define push cl:push)

(define subsetp cl:subsetp)


;;;;
(define (cl:and . exps)
  (every identity (map (lambda (exp) (if (null? exp) #f exp)) exps)))
#|    #;(format #t " { ~d => ~d }\n" exps es)
    (if (null? es) #t
        (let1 top (car es)
          (and es (loop (cdr es)))))))
|#
(define (nthcdr n lis)
  (list-tail lis n #f))

(define (cl:nth n lis)
  (list-ref lis n #f))

(define (cl:position item sequence . args)
  (let-keywords args ((from-end #f)
                      (test eq?)
                      (test-not #f)
                      (start 0)
                      (end #f)
                      (key #f))
                (let1 ix (find-index (cut test item <>) (nthcdr start sequence))
                  (if ix (+ start ix) #f)
                  )))

(define (funcall fn . args)
  (let1 proc (cond [(procedure? fn) fn]
                   [(symbol? fn)
                    (global-variable-ref (current-module) fn)]
                   [else #f])
    (when fn (apply proc args))))

(define (cl:apply fn args)
  (let1 proc (cond [(procedure? fn) fn]
                   [(symbol? fn)
                    (global-variable-ref (current-module) fn)]
                   [else #f])
    (when fn (apply proc args))))

;;;
(define-macro (progv symbols values . body)
  `(let* ([module (current-module)]
          [env (interaction-environment)])
     (let loop ((vars ,symbols) (vals ,values))
       (if (null? vars)
           (begin
             ,@body);;(eval ,body env)
           (begin
             (eval (list 'define (car vars) (car vals)) env)
             (loop (cdr vars) (cdr vals))) ))))

(define (cl:merge result-type sequence1 sequence2 predicate . args)
  (let-keywords* args ((key identity))
      ;;とりあえず実装
      (case result-type
        [(list)
         (let loop ((result '()) (seq1 sequence1) (seq2 sequence2))
           ;(format #t "(loop ~a ~a ~a)...\n" result seq1 seq2)
           (cond [(null? seq1) (append! (reverse! result) seq2)]
                 [(null? seq2) (append! (reverse! result) seq1)]
                 [(predicate (key (car seq1)) (key (car seq2))) ; seq1 < seq2
                  (loop (cons (car seq1) result) (cdr seq1) seq2)]
                 [(predicate (key (car seq2)) (key (car seq1))) ; seq1 > seq2
                  (loop (cons (car seq2) result) seq1 (cdr seq2))]
                 [else ; seq1 = seq2
                  (loop (cons (car seq1) result) (cdr seq1) seq2)]))]
        [(string)
         (list->string (cl:merge 'list (string->list sequence1) (string->list sequence2) predicate :key key))]
        [else '()])))

(define cl:char-lessp char-ci<?)

;;; nil を #f に読み替える
(define (cl:thru exp) (if (or (null? exp) (undefined? exp)) #f exp))
;;; #f を nil に読み替える
(define (scheme:thru exp) (or exp '()))

;;
(define (cl:find item sequence . args)
  (let-keywords* args ((from-end #f)
					   (test eqv?)
					   (test-not #f)
					   (start 0)
					   (end #f)
					   (key identity))
	(scheme:thru ((if from-end find-tail find)
				  (if test-not
					  (lambda (x) (not (test-not (key x) item)))
					  (lambda (x) (test (key x) item)) )
				  (cond [end (subseq sequence start end)]
						[(> start 0) (subseq sequence start)]
						[else sequence])))))

(define-macro (cl:setf x y) `(begin (set! ,x ,y) ,x))

(define cl:remove-if remove)
(define cl:remove-if-not filter)
(define cl:find-all-if cl:remove-if-not)

#;(define (cl:remove item sequence . args)
  (let-keywords* args ((from-end #f)
					   (test #f)
					   (test-not #f)
					   (start #f)
					   (end #f)
					   (count #f)
					   (key #f))
	(cond [test (delete item sequence test)]
			[test-not (delete item sequence (lambda (y) (not (test x y))))]
			[else (delete item sequence)])))

(define multiple-value-bind receive)

(define cl:copy-list list-copy) ; srfi-1

(define (cl:delete/remove remove-fn item sequence args)
  (let-keywords* args ((test eq?) (test-not #f)
					   (start #f) (end #f) ; start/end not supported yet
					   (count #f) (from-end #f) ; from-end not supported yet
					   (key identity))
	(let1 test-fn (if test-not
					  (lambda (x) (not (test-not item (key x))))
					  (lambda (x) (test item (key x))))
	  (if count
		  (let1 test-fn* (lambda (x) (if (zero? count) #f
										 (let1 r (test-fn x)
										   (if r (begin (dec! count) r) #f))))
			(remove-fn test-fn* sequence))
		  (remove-fn test-fn sequence)
		  ))))

(define (cl:delete item sequence . args) (cl:delete/remove remove! item sequence args))
(define (cl:remove item sequence . args) (cl:delete/remove remove item sequence args))
#|
(define (cl:delete item sequence . args)
  ;;実装適当... cl:delete = cl:remove!
  (let-keywords* args ((test eq?) (test-not #f)
					   (start #f) (end #f)
					   (count #f) (from-end #f)
					   (key identity))
	;(let1 seq (if key (map key sequence) sequence)
	(let1 test-fn (if test-not 
					  (lambda (x) (not (test-not (key x) item)))
					  (lambda (x) (test (key x) item)))
	  (remove! test-fn sequence))))

(define (cl:remove item sequence . args)
  ;;実装適当... (delete x list [elt=]) = (remove (lambda (y) (elt= x y)) list)@srfi-1
  (let-keywords* args ((test eq?) (test-not #f)
					   (start #f) (end #f)
					   (count #f) (from-end #f)
					   (key identity))
	;(let1 seq (if key (map key sequence) sequence)
	(let1 test-fn (if test-not 
					  (lambda (x) (not (test-not (key x) item)))
					  (lambda (x) (test (key x) item)))
	  (remove test-fn sequence))))
|#

(define-macro (cl:if cond then . args)
  (if (pair? args)
	  `(if (cl:thru ,cond) ,then ,(car args))
	  `(if (cl:thru ,cond) ,then cl:nil) ))

(define-macro (cl:when cond . then)
  `(if ,cond (begin ,@then) cl:nil))
(define-macro (cl:unless cond . then)
  `(if ,cond cl:nil (begin ,@then)))

(define (cl:car lis)
  (if (pair? lis) (car lis) cl:nil))
(define (cl:cdr lis)
  (if (pair? lis) (cdr lis) cl:nil))

(define (cl:remove-duplicates sequence . args)
  (let-keywords* args ((from-end #f)
					   (test equal?)
					   (test-not #f)
					   (start #f)
					   (end #f)
					   (key #f))
	(delete-duplicates sequence test))) ;; srfi-1

(define (cl:delete-duplicates sequence . args)
  (let-keywords* args ((from-end #f)
					   (test equal?)
					   (test-not #f)
					   (start #f)
					   (end #f)
					   (key #f))
	(delete-duplicates! sequence test))) ;; srfi-1

;;;;;
(define (cl:union list1 list2 . args)
  (let-keywords* args ((test equal?) (test-not #f) (key #f))
	(lset-union test list1 list2))) ;; srfi-1
(define (cl:nunion list1 list2 . args)
  (let-keywords* args ((test equal?) (test-not #f) (key #f))
	(lset-union! test list1 list2))) ;; srfi-1
(define (cl:set-difference list1 list2 . args)
  (let-keywords* args ((test equal?) (test-not #f) (key #f))
	(lset-difference test list1 list2))) ;; srfi-1
(define (cl:nset-difference list1 list2 . args)
  (let-keywords* args ((test equal?) (test-not #f) (key #f))
	(lset-difference! test list1 list2))) ;; srfi-1

(define (cl:sort sequence predicate . args)
  (let-keywords* args ((key #f))
	(if key
		(sort-by sequence key predicate)
		(sort sequence predicate))))
(define (cl:stable-sort sequence predicate . args)
  (let-keywords* args ((key #f))
	(if key
		(stable-sort-by sequence key predicate)
		(stable-sort sequence predicate))))

(define (cl:fourth lis)
  (if (<= 4 (length lis)) (fourth lis) cl:nil))
(define (cl:fifth lis)
  (if (<= 5 (length lis)) (fifth lis) cl:nil))

(define-macro (cl:nconc . lists) `(append! ,@lists))

(define (cl:adjoin item list . args)
  (let-keywords* args ((test eqv?)
					   (test-not #f)
					   (key #f))
	(when test-not
	  (set! test (lambda (x y) (not (test-not x y)))))
	(lset-adjoin test list item)))

(define cl:mapcar map)
(define mapcar cl:mapcar)
(define (cl:maplist fn . lists)
  (let loop ((rests lists) (result (make-list (length lists) '())))
	(if (null? (car rests)) (map reverse! result)
		(loop (map cdr rests)
			  (map cons (map fn rests) result)
			  ))))
(define (cl:maplist fn . lists)
  (let loop ((rests lists) (result '()))
	(if (null? (car rests)) (reverse! result)
		(loop (map cdr rests)
			  (cons (apply fn rests) result)))))

;(define (cl:mapc fn . lists)
(define cl:mapc for-each)
(define mapc cl:mapc)

(define (cl:mapl fn . lists)
  (let loop ((rests lists))
	(cl:unless (null? (car rests))
			   (apply fn rests)
			   (loop (map cdr rests)))))

;(define (cl:mapcan fn . lists)
(define-macro (cl:mapcan fn . lists)
  `(append-map ,fn ,@lists))
(define mapcan cl:mapcan)

(define-macro (cl:mapcon fn . lists)
  `(apply cl:nconc (maplist ,fn ,@lists)))
(define mapcon cl:mapcon)

(define-macro (cl:count-if pred . lists) ; srfi-1
  `(count ,pred ,@lists))

(provide "naoyat/cl")
;;EOF
