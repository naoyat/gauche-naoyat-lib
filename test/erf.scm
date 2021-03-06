(use gauche.test)
(use naoyat.test)
(test-start "naoyat.math.erf")

(use naoyat.math.erf)
(test-module 'naoyat.math.erf)

(use srfi-1)
;(use naoyat.math.h)

(test-section "erf")
(test* "erf(0.0) = 0.0" 0.0000000 (erf 0.0) ~7=)
(test* "erf(0.05) = 0.056372" 0.0563720 (erf 0.05) ~7=)
(test* "erf(0.10) = 0.112463" 0.1124629 (erf 0.1) ~7=)
(test* "erf(0.15) = 0.167996" 0.1679960 (erf 0.15) ~7=)
(test* "erf(0.20) = 0.222703" 0.2227026 (erf 0.2) ~7=)
(test* "erf(0.25) = 0.276326" 0.2763264 (erf 0.25) ~7=)
(test* "erf(0.30) = 0.328627" 0.3286268 (erf 0.3) ~7=)
(test* "erf(0.35) = 0.379382" 0.3793821 (erf 0.35) ~7=)
(test* "erf(0.40) = 0.428392" 0.4283924 (erf 0.4) ~7=)
(test* "erf(0.45) = 0.475482" 0.4754817 (erf 0.45) ~7=)
(test* "erf(0.50) = 0.5205" 0.5204999 (erf 0.5) ~7=)
(test* "erf(0.55) = 0.563323" 0.5633234 (erf 0.55) ~7=)
(test* "erf(0.60) = 0.603856" 0.6038561 (erf 0.6) ~7=)
(test* "erf(0.65) = 0.642029" 0.6420293 (erf 0.65) ~7=)
(test* "erf(0.70) = 0.677801" 0.6778012 (erf 0.7) ~7=)
(test* "erf(0.75) = 0.711156" 0.7111556 (erf 0.75) ~7=)
(test* "erf(0.80) = 0.742101" 0.7421010 (erf 0.8) ~7=)
(test* "erf(0.85) = 0.770668" 0.7706681 (erf 0.85) ~7=)
(test* "erf(0.90) = 0.796908" 0.7969082 (erf 0.9) ~7=)
(test* "erf(0.95) = 0.820891" 0.8208908 (erf 0.95) ~7=)
(test* "erf(1.0) = 0.842701" 0.8427008 (erf 1) ~7=)
(test* "erf(1.1) = 0.880205" 0.8802051 (erf 1.1) ~7=)
(test* "erf(1.2) = 0.910314" 0.9103140 (erf 1.2) ~7=)
(test* "erf(1.3) = 0.934008" 0.9340079 (erf 1.3) ~7=)
(test* "erf(1.4) = 0.952285" 0.9522851 (erf 1.4) ~7=)
(test* "erf(1.5) = 0.966105" 0.9661051 (erf 1.5) ~7=)
(test* "erf(1.6) = 0.976348" 0.9763484 (erf 1.6) ~7=)
(test* "erf(1.7) = 0.983791" 0.9837905 (erf 1.7) ~7=)
(test* "erf(1.8) = 0.98909" 0.9890905 (erf 1.8) ~7=)
(test* "erf(1.9) = 0.99279" 0.9927904 (erf 1.9) ~7=)
(test* "erf(2.0) = 0.995322" 0.9953223 (erf 2) ~7=)
(test* "erf(2.1) = 0.99702" 0.9970205 (erf 2.1) ~7=)
(test* "erf(2.2) = 0.998137" 0.9981372 (erf 2.2) ~7=)
(test* "erf(2.3) = 0.998857" 0.9988568 (erf 2.3) ~7=)
(test* "erf(2.4) = 0.999312" 0.9993115 (erf 2.4) ~7=)
(test* "erf(2.5) = 0.999593" 0.9995930 (erf 2.5) ~7=)
(test* "erf(2.6) = 0.999764" 0.9997640 (erf 2.6) ~7=)
(test* "erf(2.7) = 0.999866" 0.9998657 (erf 2.7) ~7=)
(test* "erf(2.8) = 0.999925" 0.9999250 (erf 2.8) ~7=)
(test* "erf(2.9) = 0.999959" 0.9999589 (erf 2.9) ~7=)
(test* "erf(3.0) = 0.999978" 0.9999779 (erf 3) ~7=)
(test* "erf(3.1) = 0.999988" 0.9999884 (erf 3.1) ~7=)
(test* "erf(3.2) = 0.999994" 0.9999940 (erf 3.2) ~7=)
(test* "erf(3.3) = 0.999997" 0.9999969 (erf 3.3) ~7=)
(test* "erf(3.4) = 0.999999" 0.9999985 (erf 3.4) ~7=)
(test* "erf(3.5) = 0.999999" 0.9999993 (erf 3.5) ~7=)

(test-section "erfc")
(test* "erfc(0.0) = 1.0000000" 1.0000000 (erfc 0.0) ~7=)
(test* "erfc(0.05) = 0.9436280" 0.9436280 (erfc 0.05) ~7=)
(test* "erfc(0.10) = 0.8875371" 0.8875371 (erfc 0.10) ~7=)
(test* "erfc(0.15) = 0.8320040" 0.8320040 (erfc 0.15) ~7=)
(test* "erfc(0.20) = 0.7772974" 0.7772974 (erfc 0.20) ~7=)
(test* "erfc(0.25) = 0.7236736" 0.7236736 (erfc 0.25) ~7=)
(test* "erfc(0.30) = 0.6713732" 0.6713732 (erfc 0.30) ~7=)
(test* "erfc(0.35) = 0.6206179" 0.6206179 (erfc 0.35) ~7=)
(test* "erfc(0.40) = 0.5716076" 0.5716076 (erfc 0.40) ~7=)
(test* "erfc(0.45) = 0.5245183" 0.5245183 (erfc 0.45) ~7=)
(test* "erfc(0.50) = 0.4795001" 0.4795001 (erfc 0.50) ~7=)
(test* "erfc(0.55) = 0.4366766" 0.4366766 (erfc 0.55) ~7=)
(test* "erfc(0.60) = 0.3961439" 0.3961439 (erfc 0.60) ~7=)
(test* "erfc(0.65) = 0.3579707" 0.3579707 (erfc 0.65) ~7=)
(test* "erfc(0.70) = 0.3221988" 0.3221988 (erfc 0.70) ~7=)
(test* "erfc(0.75) = 0.2888444" 0.2888444 (erfc 0.75) ~7=)
(test* "erfc(0.80) = 0.2578990" 0.2578990 (erfc 0.80) ~7=)
(test* "erfc(0.85) = 0.2293319" 0.2293319 (erfc 0.85) ~7=)
(test* "erfc(0.90) = 0.2030918" 0.2030918 (erfc 0.90) ~7=)
(test* "erfc(0.95) = 0.1791092" 0.1791092 (erfc 0.95) ~7=)
(test* "erfc(1.0) = 0.1572992" 0.1572992 (erfc 1.0) ~7=)
(test* "erfc(1.1) = 0.1197949" 0.1197949 (erfc 1.1) ~7=)
(test* "erfc(1.2) = 0.0896860" 0.0896860 (erfc 1.2) ~7=)
(test* "erfc(1.3) = 0.0659921" 0.0659921 (erfc 1.3) ~7=)
(test* "erfc(1.4) = 0.0477149" 0.0477149 (erfc 1.4) ~7=)
(test* "erfc(1.5) = 0.0338949" 0.0338949 (erfc 1.5) ~7=)
(test* "erfc(1.6) = 0.0236516" 0.0236516 (erfc 1.6) ~7=)
(test* "erfc(1.7) = 0.0162095" 0.0162095 (erfc 1.7) ~7=)
(test* "erfc(1.8) = 0.0109095" 0.0109095 (erfc 1.8) ~7=)
(test* "erfc(1.9) = 0.0072096" 0.0072096 (erfc 1.9) ~7=)
(test* "erfc(2.0) = 0.0046777" 0.0046777 (erfc 2.0) ~7=)
(test* "erfc(2.1) = 0.0029795" 0.0029795 (erfc 2.1) ~7=)
(test* "erfc(2.2) = 0.0018628" 0.0018628 (erfc 2.2) ~7=)
(test* "erfc(2.3) = 0.0011432" 0.0011432 (erfc 2.3) ~7=)
(test* "erfc(2.4) = 0.0006885" 0.0006885 (erfc 2.4) ~7=)
(test* "erfc(2.5) = 0.0004070" 0.0004070 (erfc 2.5) ~7=)
(test* "erfc(2.6) = 0.0002360" 0.0002360 (erfc 2.6) ~7=)
(test* "erfc(2.7) = 0.0001343" 0.0001343 (erfc 2.7) ~7=)
(test* "erfc(2.8) = 0.0000750" 0.0000750 (erfc 2.8) ~7=)
(test* "erfc(2.9) = 0.0000411" 0.0000411 (erfc 2.9) ~7=)
(test* "erfc(3.0) = 0.0000221" 0.0000221 (erfc 3.0) ~7=)
(test* "erfc(3.1) = 0.0000116" 0.0000116 (erfc 3.1) ~7=)
(test* "erfc(3.2) = 0.0000060" 0.0000060 (erfc 3.2) ~7=)
(test* "erfc(3.3) = 0.0000031" 0.0000031 (erfc 3.3) ~7=)
(test* "erfc(3.4) = 0.0000015" 0.0000015 (erfc 3.4) ~7=)
(test* "erfc(3.5) = 0.0000007" 0.0000007 (erfc 3.5) ~7=)


;(test-section "ck")
;(test* "ck: c0..c9" '(1 1 7/6 127/90 4369/2520
;						34807/16200 20036983/7484400 2280356863/681080400
;						49020204823/11675664000 65967241200001/12504636144000)
;	   (reverse! (calc-cks 9))
;	   (cut every~= 1e-12 <> <>))

(test-section "inverse erf")
(test* "erf-1(0)" 0.0 (inverse-erf 0.0) ~8=)
(test* "erf-1(1/2)" 0.47693627 (inverse-erf 1/2) ~8=)

(test-section "probit function")
(test* "probit(0.025)" -1.959964 (probit 0.025) ~7=)
(test* "probit(0.975)" 1.959964 (probit 0.975) ~7=)

(test-end)
