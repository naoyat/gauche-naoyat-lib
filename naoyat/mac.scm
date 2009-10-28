(define-module naoyat.mac
  (export open-paths-with-command
		  open-file
		  open-image
		  open-movie
		  ))
(select-module naoyat.mac)

;;
;; Mac OS X専用ツール
;;
(define (open-paths-with-command cmd paths)
  (for-each (lambda (path) (sys-system (string-append cmd " " path))) paths))

;;
;; ファイルを開く
;;
(define (open-file . paths) (open-paths-with-command "open" paths))

(define open-image open-file)

(define (open-movie . paths) (open-paths-with-command "/Applications/QuickTime\\ Player.app/Contents/MacOS/QuickTime\\ Player" paths))


(provide "naoyat/mac")
;;EOF
