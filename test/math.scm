(use gauche.test)
(test-start "math")

(use naoyat.math)
(test-module 'naoyat.math)

(test-section "numbers-between")
(test* "[0..4]" '(0 1 2 3 4) (numbers-between 0 4))
(test* "[1..3]" '(1 2 3) (numbers-between 1 3))
(test* "[2..2]" '(2) (numbers-between 2 2))
(test* "[3..1]" '() (numbers-between 3 1))

(test-section "smallest")
(test* "smallest" 3 (smallest '(8 3 7 5 4)))
(test* "smallest" 18 (smallest '(18 53 47 35 24)))
(test* "smallest" 1 (smallest '(1)))

(test-section "largest")
(test* "largest" 8 (largest '(8 3 7 5 4)))
(test* "largest" 53 (largest '(18 53 47 35 24)))
(test* "largest" 1 (largest '(1)))

(test-section "divisors")
(test* "(divisors 1)" '(1) (divisors 1))
(test* "(divisors 3)" '(1 3) (divisors 3))
(test* "(divisors 6)" '(1 2 3 6) (divisors 6))
(test* "(divisors 10)" '(1 2 5 10) (divisors 10))
(test* "(divisors 15)" '(1 3 5 15) (divisors 15))
(test* "(divisors 21)" '(1 3 7 21) (divisors 21))
(test* "(divisors 28)" '(1 2 4 7 14 28) (divisors 28))

(test-section "nCk")
(test* "4C2" 6 (C 4 2))
(test* "40C20" 137846528820 (C 40 20))


(test-end)