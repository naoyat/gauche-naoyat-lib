(use gauche.test)
(use naoyat.test)
(test-start "naoyat.mac.speech")

(use naoyat.mac.speech)
(test-module 'naoyat.mac.speech)

(manual-test*
 "Ralph says コンニチワ"
 (lambda () (say-katakana-with-tune "Ralph" "3コ5ンニチワ")))
(manual-test*
 "Victoria says サヨウナラ"
 (lambda () (say-katakana-with-tune "Victoria" "3サ5ヨーナラ")))
 
(test-end)
