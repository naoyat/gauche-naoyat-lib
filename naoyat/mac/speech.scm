(define-module naoyat.mac.speech
  (use srfi-1) ; zip
  (use gauche.process)

  (use naoyat.list) ; assoc-value

  (export say
		  say-tuned
		  say-katakana-with-tune
		  ))
(select-module naoyat.mac.speech)

;;
;; voice: '(Bruce Deranged Victoria Bahh Bad News Kathy Hysterical Fred Agnes Trinoids Albert Whisper Pipe Organ Cellos Vicki Bubbles Zarvox Princess Junior Good News Bells Ralph Boing)
;;

;;
;; say
;;
(define (say voice text)
  (sys-exec "say" (list "say" "-v" (or voice "Fred") text)))


(define (say-tuned voice tuner)
  (call-with-output-process (list "say" "-v" (or voice "Fred"))
							(lambda (port) 
							  (write "[[inpt TUNE]]" port) (newline port)
							  (tuner port)
							  (write "[[inpt TEXT]]" port) (newline port)
							  )))

;;
;; katakana tuner
;;
(define (kana-to-phoneme kana-with-pitch)
  (define (phoneme kch)
	(let* ((*kana-phonemes* '((#\ア #f a) (#\イ #f i) (#\ウ #f u) (#\エ #f e) (#\オ #f o)
							  (#\カ k a) (#\キ k i) (#\ク k u) (#\ケ k e) (#\コ k o)
							  (#\サ s a) (#\シ S i) (#\ス s u) (#\セ s e) (#\ソ s o)
							  (#\タ t a) (#\チ C i) (#\ツ (t s) u) (#\テ t e) (#\ト t o)
							  (#\ナ n a) (#\ニ n i) (#\ヌ n u) (#\ネ n e) (#\ノ n o)
							  (#\ハ h a) (#\ヒ h i) (#\フ f u) (#\ヘ h e) (#\ホ h o)
							  (#\マ m a) (#\ミ m i) (#\ム m u) (#\メ m e) (#\モ m o)
							  (#\ヤ y a)            (#\ユ y u)            (#\ヨ y o)
							  (#\ラ r a) (#\リ r i) (#\ル r u) (#\レ r e) (#\ロ r o)
							  (#\ワ w a) (#\ヰ w i)           (#\ヱ w e) (#\ヲ w o)
							  (#\ン n #f)
							  (#\ヴ v u)
							  (#\ガ g a) (#\ギ g i) (#\グ g u) (#\ゲ g e) (#\ゴ g o)
							  (#\ザ z a) (#\ジ Z i) (#\ズ z u) (#\ゼ z e) (#\ゾ z o)
							  (#\ダ d a) (#\ヂ Z i) (#\ヅ z u) (#\デ d e) (#\ド d o)
							  (#\バ b a) (#\ビ b i) (#\ブ b u) (#\ベ b e) (#\ボ b o)
							  (#\パ p a) (#\ピ p i) (#\プ p u) (#\ペ p e) (#\ポ p o)
							  
							  (#\ァ #f a) (#\ィ #f i) (#\ゥ #f u) (#\ェ #f e) (#\ォ #f o)
							  (#\ャ y a)  (#\ュ y u)  (#\ョ y o)
							  (#\ー #f #f)
							  (#\ッ @ #f)  (#\　 % #f)
							  (#\。 % #f)  (#\、 % #f)
							  (#\？ % #f) (#\！ % #f)
							  (#\・ % #f)
							  (#\（ % #f)  (#\） % #f)
							  (#\「 % #f)  (#\」 % #f)
							  ))
		   (a (assoc kch *kana-phonemes*)))
	  (if a (cdr a) (list #f #f))))

  (define (extend-vowel v)
	(assoc-value v '((a a 2) (i i 2) (u u 2) (e e 2) (o o 2))))

  (define (actual-phoneme ph)
	(or (assoc-value ph '((a . AA) (i . IY) (u . UW) (e . EH) (o . OW) (r . l)))
		ph))

  (define (tune phoneme pitch)
	(let* ((*freq-base* 128.0)
		   (*mag12* '(1.0 1.05946 1.12246 1.18921 1.259920 1.33484 1.41421 
						  1.49831 1.5874 1.68179 1.7818 1.88775 2.0))
		   (*frequency* (zip
						 '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B)
						 (map (lambda (x) (* *freq-base* x)) *mag12*))))
	  (define (tune-sub ph duration pitch bendable)
		(let* ((freq (car (assoc-value pitch *frequency*)))
			   (tune (if bendable
						 (format #f "~s {D ~s; P ~s:0}" 
								 (actual-phoneme ph) (* 160 duration) freq)
						 (format #f "~s {D ~s; P ~s:0 ~s:99}" 
								 (actual-phoneme ph) (* 160 duration) freq freq))))
		  tune))
		
	  (let ((consonant (car phoneme))
			(vowel (cadr phoneme)))
;		(print "(c,v,b) = (" consonant "," vowel "," bend ")")
		(cond ((and consonant vowel)
			   (string-append
				(if (pair? consonant)
					(string-append
					 (tune-sub (car consonant) 0.2 pitch #f)
					 (tune-sub (cadr consonant) 0.1 pitch #f))
					(tune-sub consonant 0.3 pitch #f))
				(if (pair? vowel)
					(tune-sub (car vowel) 1.7 pitch #t)
					(tune-sub vowel 0.7 pitch #f))
				))
			  ((and (not consonant) vowel) ; vowel only
			   (if (pair? vowel)
				   (tune-sub (car vowel) 2.0 pitch #t)
				   (tune-sub vowel 1.0 pitch #f)))
			  ((and consonant (not vowel)) ; consonant only
			   (tune-sub consonant 1.0 pitch #f))
			  (else ""))
		))); definition of (tune) ends here
  
  (let ((kana (car kana-with-pitch))
		(pitch (cdr kana-with-pitch)))
	(tune (if (pair? kana)
			  (let ((kana1 (car kana))
					(kana2 (cadr kana))
					(kana3 (if (null? (cddr kana)) #f (caddr kana))))
				(let ((p1 (phoneme kana1))
					  (p2 (phoneme kana2)))
				  (let ((c (if (and (memq kana2 '(#\ャ #\ュ #\ョ))
									(not (memq kana1 '(#\チ #\シ #\ヂ \#ジ))))
							   (list (car p1) (car p2))
							   (car p1)))
						(v (if (eq? kana2 #\ー)
							   (extend-vowel (cadr p1))
							   (cadr p2)))
						(b (if (or (eq? kana2 #\ー) (eq? kana3 #\ー)) #t #f)))
					(if kana3
						(list c (extend-vowel v) b)
						(list c v b)))));p1,kana1
			  (phoneme kana));tune
		  pitch)))

(define (small-y? ch) (if (memq ch '(#\ャ #\ュ #\ョ #\ァ #\ィ #\ゥ #\ェ #\ォ)) #t #f))
(define (num? ch) (if (memq ch '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)) #t #f))

(define (katakana-tuner str)
  (define (joint-small-y l) ; 小さいャュョを直前のカナの一部にする
	(if (null? l)
		'()
		(let ((ch (car l)) (rest (cdr l)))
		  (cond ((small-y? ch)
				 (if (num? (car rest))
					 ;* 1 y
					 (cons (car rest)
						   (joint-small-y (cons ch (cdr rest)))) ; 先送り: * y] 1
					 ;* y ; normal
					 (cons (list (car rest) ch)
						   (joint-small-y (cdr rest)))
					 ))
				((eq? #\ー ch)
				 (cond ((num? (car rest)) ; * 1 -
						(cons (car rest)
							  (joint-small-y (cons ch (cdr rest))))) ; 先送り: * -] 1
					   ((small-y? (car rest)) ; * y -
						(if (num? (cadr rest))
										; 1 y - ==> y -] 1
							(cons (cadr rest) 
								  (joint-small-y (cons ch (cons (car rest) (cddr rest))))) ;先送り
										; * y - ; normal
							(cons (list (cadr rest) (car rest) ch)
								  (joint-small-y (cddr rest)))
							))
					   (else ; * -
						 (cons (list (car rest) ch)
							   (joint-small-y (cdr rest))))
					   )); #\ー
				(else
				 (cons ch (joint-small-y rest)))
				);cond
		  ))); joint-small-y
		  
  (define (add-pitch l pitch) ; ピッチ情報を付加
	(if (null? l)
		'()
		(let ((ch (car l))
			  (rest (cdr l)))
;		  (if (char-numeric? ch) ; (if (memq ch '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
		  (if (num? ch)
			  (add-pitch rest ch)
			  (cons (cons ch pitch) (add-pitch rest pitch)))
		  )))
  ; returns tuner
  (lambda (port)
	(map (lambda (s) (write s port) (newline port))
		 (map kana-to-phoneme
			  (add-pitch
			   (reverse! (joint-small-y 
						  (reverse! (string->list str))))
			   #\1))
		 ))
  )

(define (say-katakana-with-tune voice tune-text)
  (say-tuned voice (katakana-tuner tune-text)))

(provide "naoyat/mac/speech")
;;EOF