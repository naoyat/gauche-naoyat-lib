(define-module naoyat.mac.applescript
  (use gauche.process)
  (use gauche.charconv)

  (export execute-applescript
		  execute-applescript*
		  execute-applescript-line
		  ))
(select-module naoyat.mac.applescript)

;;
;; Applescript
;;
(define (execute-applescript script)
  (process-output->string-list (list "/usr/bin/osascript" "-e" script)))

(define (execute-applescript* script)
;  (print "script: " script)
;  (call-with-output-process (list "/usr/bin/osascript" "-")
  (call-with-process-io (list "/usr/bin/osascript" "-")
						(lambda (cmd-stdout cmd-stdin)
						  (display script cmd-stdin)
										;							  (newline cmd-stdin)
										;							  (flush cmd-stdin)
						  (close-output-port cmd-stdin)
						  (read-line cmd-stdout #t)
										;							  (read-char cmd-stdout)
						  )))

(define (execute-applescript-line one-liner)
  (call-with-output-process (list "/usr/bin/osascript" "-e" one-liner) (lambda (x) ())))

(provide "naoyat/mac/applescript")
;;EOF
