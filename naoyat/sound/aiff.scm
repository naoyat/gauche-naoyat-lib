;;
;; AIFF (Audio Interchange File Format) library, by naoya_t
;;
;;   AIFF-C (圧縮したやつ)には対応してない
;;   WAVはこれに似てるらしいのでそのうち対応
;;
(define-module naoyat.sound.aiff
  (use srfi-1)
  (use srfi-4) ;uvector
  (use srfi-11) ;; let-values
  (use gauche.uvector)
  (use binary.io) ;default-endianの設定とか
  (use util.match)

  (use naoyat.binary.io) ;read-f80 write-f80

  (export save-as-aiff-file ;; file-name sample-rate sample-bit-size stereo-mode sound-vecs
		  save-as-aiff* ;; file-name data sec
		  open-aiff-file ; to load
		  ))
(select-module naoyat.sound.aiff)

;; body
(default-endian 'big-endian)

;(define-constant SIGNATURE-FORM #x464f524d)
;(define-constant SIGNATURE-AIFF #x41494646)
;(define-constant SIGNATURE-COMM #x434f4d4d)
;(define-constant SIGNATURE-MARK #x4d41524b)
;(define-constant SIGNATURE-APPL #x4150504c)
;(define-constant SIGNATURE-SSND #x53534e44)

(define (read-id)
  (let loop ((i 4) (chars '()))
	(if (zero? i)
		(string->symbol (list->string (reverse! chars)))
		(let1 ch (read-char)
		  (if (eof-object? ch) ch
			  (loop (- i 1) (cons ch chars)))))))

(define (write-id id) ; id must be symbol
  (define (write-four-chars chars)
	(let loop ((n 4) (chars chars))
	  (unless (zero? n)
		(write-char (car chars))
		(loop (- n 1) (cdr chars)))))
  (cond [(string? id) (write-four-chars (string->list id))]
		[(symbol? id) (write-four-chars (string->list (symbol->string id)))]
		[else (write-u32 id)]))

(define (read-chunk-head)
  (let1 chunk-id (read-id)
	(if (or (eof-object? chunk-id) (not chunk-id))
		(values chunk-id #f)
		(let1 chunk-size (read-u32 #f 'big-endian)
		  (if (= chunk-size #xffffffff)
			  (values #f #f)
			  (values chunk-id chunk-size))))))

(define (write-chunk-head chunk-id chunk-size)
  (cond [(string? chunk-id) (write-id chunk-id)]
		[(symbol? chunk-id) (write-id (symbol->string chunk-id))]
		[else (write-u32 chunk-id)])
  (write-u32 chunk-size))

(define (write-block-s16-bigendian . vecs)
  (let* ([vec1 (car vecs)]
		 [vec2 (if (null? (cdr vecs)) #f (cadr vecs))] ; stereo-mode
		 [len (s16vector-length vec1)])
	(dotimes (i len)
	  (write-s16 (s16vector-ref vec1 i))
	  (when vec2
		(write-s16 (s16vector-ref vec2 i))))
	(if vec2 (values vec1 vec2) vec1)))

(define (write-block-s8 . vecs)
  (let* ([vec1 (car vecs)]
		 [vec2 (if (null? (cdr vecs)) #f (cadr vecs))] ; stereo-mode
		 [len (s8vector-length vec1)])
	(dotimes (i len)
	  (write-s8 (s8vector-ref vec1 i))
	  (when vec2
		(write-s8 (s8vector-ref vec2 i))))
	(if vec2 (values vec1 vec2) vec1)))

(define (save-as-aiff-file file-name sample-rate sample-bit-size stereo-mode sound-vecs)
#;  (format #t "(save-as-aiff-file file-name:~s sample-rate:~d sample-bit-size:~d stereo-mode:~a sound-vecs:~a)\n"
		  file-name sample-rate sample-bit-size stereo-mode sound-vecs)
  (let* ([length-func (if (= sample-bit-size 16) s16vector-length s8vector-length)]
		 [write-block-func (if (= sample-bit-size 16) write-block-s16-bigendian write-block-s8)] ; write-block だとうまく行かなかった
		 [num-sample-frames (apply + (map length-func sound-vecs))]
		 [sample-bytes (ash sample-bit-size -3)]
		 [sound-chunk-size (+ 8 (* sample-bytes num-sample-frames (if stereo-mode 2 1)))]
		 )
	(with-output-to-file file-name
	  (lambda ()
		(write-chunk-head 'FORM (+ 4 26 8 sound-chunk-size))
		(write-id 'AIFF)

		(write-chunk-head 'COMM 18)
		(write-u16 (if stereo-mode 2 1)) ; numChannels
		(write-u32 num-sample-frames) ; numSampleFrames
		(write-u16 sample-bit-size)
		(write-f80 sample-rate)

		(write-chunk-head 'SSND sound-chunk-size)
		(write-u32 0) ; offset
		(write-u32 0) ; blocksize
		(if stereo-mode
			(for-each write-block-func sound-vecs sound-vecs)
			(for-each write-block-func sound-vecs))
		))))

(define (save-as-aiff* file-name data sec)
  (let* ([s16vec (list->s16vector (map (lambda (x) (clamp (round->exact (* x 32768)) -32767 32767)) data))]
		 [datalen (s16vector-length s16vec)]
		 [rate (/ datalen sec)] ;; datalenでsec秒
		 [bitsize 16])
	(save-as-aiff-file file-name rate bitsize #f (list s16vec))))

;;;
(define (open-aiff-file aiff-file-name)
  (let ([iport (open-input-file aiff-file-name :if-does-not-exist #f :buffering :modest)]
		[chunks '()]
		[num-channels 0] [num-sample-frames 0] [sample-size 0] [sample-rate 0]
		[sample-bytes 0] [half-sample-frames 0] [half-sample-rate 0])

	(define (read-comm-chunk loc chunk-size)
	  ;; The Common Chunk
	  (let* ([%num-channels (read-u16 #f 'big-endian)]
			 [%num-sample-frames (read-u32 #f 'big-endian)]
			 [%sample-size (read-u16 #f 'big-endian)]
			 [%sample-rate (read-f80)])
		(let ([%half-sample-frames (ash %num-sample-frames -1)]
			  [%half-sample-rate (/ %sample-rate 2)]
			  [%sample-bytes (ash (+ %sample-size 7) -3)])

		  (set! num-channels %num-channels)
		  (set! num-sample-frames %num-sample-frames)
		  (set! half-sample-frames %half-sample-frames)
		  (set! sample-size %sample-size)
		  (set! sample-rate %sample-rate)
		  (set! half-sample-rate %half-sample-rate)
		  (set! sample-bytes %sample-bytes)

		  `(COMM ,loc
				 (num-channels . ,num-channels)
				 (num-sample-frames . ,num-sample-frames)
				 (half-sample-frames . ,half-sample-frames)
				 (sample-size . ,sample-size)
				 (sample-rate . ,sample-rate)
				 (half-sample-rate . ,half-sample-rate)
				 (sample-bytes . ,sample-bytes)))))
	
	(define (read-mark-chunk loc chunk-size)
	  ;; The Marker Chunk
	  (let* ([num-markers (read-u16 #f 'big-endian)]
			 [markers '()])
		(port-seek iport (- chunk-size 2) SEEK_CUR)
		`(MARK ,loc
			   (num-markers . ,num-markers)
			   (markers . ,markers))))
	
	(define (read-appl-chunk loc chunk-size)
	  (let1 buf (make-u8vector chunk-size 0) ; 36とか
		(read-block! buf)
		`(APPL ,loc (buf . ,buf))))
	
	(define (read-ssnd-chunk loc chunk-size)
	  ;; Sound Data Chunk
	  (let* ([offset (read-u32 #f 'big-endian)]
			 [block-size (read-u32 #f 'big-endian)])
		;; 残り (chunk-size - 8) bytes は WaveformData...
		(port-seek iport (- chunk-size 8) SEEK_CUR)
		`(SSND ,loc
			   (offset . ,offset)
			   (block-size . ,block-size)) ))
		   
	(define (read-ssnd-actually t l)
	  (let1 ssnd-chunk (assoc 'SSND chunks)
		(port-seek iport (+ (cadr ssnd-chunk) 8) SEEK_SET)
		
		;(print ssnd-chunk)

#;		(format #t "ssnd block (offset = ~d, size = ~d) ...\n"
				(assoc 'offset ssnd-chunk)
				(assoc 'block-size ssnd-chunk))
#;		(format #t "  num-sample-frames = ~d (~d bytes)\n"
				num-sample-frames
				(* num-sample-frames sample-bytes num-channels))

		(let1 frame-offset (round->exact (* t sample-rate))
		  (if (<= num-sample-frames frame-offset) #f
			  (let1 frame-length (round->exact (* l sample-rate))
				(when (< num-sample-frames (+ frame-offset frame-length))
				  (set! frame-length (- num-sample-frames frame-offset)))
				(let1 data (make-vector num-channels)
				  (dotimes (ch num-channels) (vector-set! data ch (make-s16vector frame-length 0)))
				  (port-seek iport (* frame-offset sample-bytes num-channels) SEEK_CUR)
				  (dotimes (i frame-length)
					(dotimes (ch num-channels)
					  (s16vector-set! (vector-ref data ch) i (read-s16 iport 'big-endian))))
				  data))))))

	(define (chunk-desc chunk-id) (assoc chunk-id chunks))

	(if iport
		(with-input-from-port iport
		  (lambda ()
			(let-values ([(form-chunk-id form-chunk-size) (read-chunk-head)])
			  (unless form-chunk-id	(error "AIFF: Invalid header"))
			  (unless (eq? form-chunk-id 'FORM) (error "AIFF: Invalid form id " form-chunk-id))
			  ;;(format #t "form chunk id: ~s size: ~d\n" form-chunk-id form-chunk-size)
			  (let1 form-type (read-id)
				(unless (eq? form-type 'AIFF) (error "AIFF: unknown form type " form-type))
				(let loop ()
				  (let-values ([(chunk-id chunk-size) (read-chunk-head)])
					(when (eof-object? chunk-id) (set! chunk-id #f)) ;; end
					(if chunk-id
						(let1 loc (port-tell iport)
						  ;;(format #t "AIFF chunk ~a, size ~d <~d>\n" chunk-id chunk-size loc)
						  (case chunk-id
							[(COMM) (push! chunks (read-comm-chunk loc chunk-size))]
							[(MARK) (push! chunks (read-mark-chunk loc chunk-size))]
							[(APPL) (push! chunks (read-appl-chunk loc chunk-size))]
							[(SSND) (push! chunks (read-ssnd-chunk loc chunk-size))]
							[else #;(format #t "skipping chunk ~a (size = ~d)...\n" chunk-id chunk-size)
								  (port-seek iport chunk-size SEEK_CUR)])
						  (loop));
						'done);fi
					))))))
		#f)

	;; dispatcher
	(lambda (m)
	  (case m
		((file-name) aiff-file-name)
		((sample-rate) sample-rate)
		((sample-bit-size) sample-size)
		((stereo-mode) (= 2 num-channels))
		;;((rewind) #f)
		((read) read-ssnd-actually)
		((chunk-desc) chunk-desc)
		(else #f)))
	))

(provide "naoyat/sound/aiff")
;;EOF
