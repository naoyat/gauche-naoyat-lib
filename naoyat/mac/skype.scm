(define-module naoyat.mac.skype
  (use naoyat.mac.applescript)

  (export skype-send-command
		  skype-launch

		  skype-call
		  skype-call-jp

		  skype-set-moodtext
		  ;;PSTN
		  skype-ping
		  skype-get
		  skype-show
		  skype-get-profile
		  skype-get-user-profile
		  ))
(select-module naoyat.mac.skype)
;;

(define (applescript-tell-skype script)
  (execute-applescript
   (format #f
"tell application \"Skype\"
  ~a
end" script)))


(define (skype-send-command cmd)
  (applescript-tell-skype
   (format #f "send command \"~a\" script name \"\"" cmd)))

(define (skype-launch)
  (execute-applescript "launch application \"Skype\""))
;  (applescript-tell-skype "launch activate"))

;;
;; Skype Commands
;;

;
; CALL <target>[, <target]*   / Protocol 1 /
;
(define (skype-call target-user)
  (skype-send-command (format #f "CALL ~a" target-user)))

(define (skype-call-jp domestic-number)
  (let ((pstn (PSTN '日本 domestic-number)))
	(skype-call pstn)))




(define (skype-set-moodtext text)
  (let ((text-sjis (ces-convert text 'utf-8 'shift_jis)))
	(skype-send-command (format #f "SET PROFILE MOOD_TEXT ~a" text-sjis))
	))


;;
;; Utilities
;;
(define (PSTN country phone-number)
  (let ((country-code 81))
	(case (string-ref phone-number 0)
	  ((#\0)
	   (format #f "+~d-~a"
			   country-code
			   (substring phone-number 1 (string-size phone-number))
			   ))
	  ((#\+) 
	   phone-number) ; そのまま
	  (else
	   'error))
	))


;
; skype-ping
;   returns #t if pong'ed
;
(define (skype-ping)
  (let ((pong (skype-send-command "PING")))
	(cond ((not (pair? pong)) #f)
		  ((not (string? (car pong))) #f)
		  ((string=? "PONG" (car pong)) #t)
		  (else #f))))

;(skype-call-jp "0743755350")
(define (skype-get target)
  (skype-send-command (format #f "GET ~a" target)))

(define (skype-show target)
  (print target ": " (skype-get target)))

(define (skype-get-profile)
  (map (lambda (s) 
		 (let ((key-value (cdr (string-split (car s) " "))))
		   (cons (string->symbol (car key-value)) (cdr key-value))
		   ))
	   (map (lambda (cmd) (skype-get (format #f "PROFILE ~a" cmd)));lambda
			(list 'PSTN_BALANCE 'PSTN_BALANCE_CURRENCY
				  'FULLNAME 'BIRTHDAY 'SEX 'LANGUAGES 'COUNTRY 'PROVINCE 'CITY
				  'PHONE_HOME 'PHONE_OFFICE 'PHONE_MOBILE
				  'HOMEPAGE 'ABOUT))))
  
(define (skype-get-user-profile username)
  (map (lambda (s) 
		 (let ((key-value (cddr (string-split (car s) " "))))
		   (cons (string->symbol (car key-value)) (cdr key-value))
		   ))
	   (map (lambda (cmd) (skype-get (format #f "USER ~a ~a" username cmd)));lambda
			(list 'HANDLE 'FULLNAME 'BIRTHDAY 'SEX
				  'LANGUAGE 'COUNTRY 'PROVINCE 'CITY 'PHONE_HOME 'PHONE_OFFICE
				  'PHONE_MOBILE 'HOMEPAGE 'ABOUT 'HASCALLEQUIPMENT
				  'BUDDYSTATUS 'ISAUTHORIZED 'ISBLOCKED 'DISPLAYNAME
				  'ONLINESTATUS 'LASTONLINETIMESTAMP 'CAN_LEAVE_VM
				  'SPEEDDIAL 'RECEIVEDAUTHREQUEST))))

(provide "naoyat/mac/skype")
;;EOF