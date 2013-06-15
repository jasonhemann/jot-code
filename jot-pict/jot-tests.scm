
(test "beta 1"
  ((beta '(lambda (x) x) 'x 'y) 0)
  '(y . 0))

(test "beta 2"
  ((beta '(lambda (x) x) 'x 'x) 0)
  '((lambda (x) x) . 0))

(test "beta 3"
  ((beta '(lambda (x) x) 'x '(lambda (x) y)) 0)
  '((lambda (x) y) . 0))

(test "beta 4"
  ((beta '(lambda (x) x) 'x '(lambda (y) y)) 0)
  '((lambda (y) y) . 1))

(test "beta 5"
  ((beta '(lambda (x) x) 'x '(lambda (y) (lambda (z) y))) 0)
  '((lambda (y) (lambda (z) y)) . 2))

(test "beta 7"
  ((beta '(lambda (x) x) 'x '(x x)) 0)
  '(((lambda (x) x) (lambda (x) x)) . 2))

(test "bv-wnf 1"
  ((bv-wnf 'x) 0)
  '(x . 0))

(test "bv-wnf 2"
  ((bv-wnf '(lambda (x) x)) 0)
  '((lambda (x) x) . 0))

(test "bv-wnf 3"
  ((bv-wnf '(x ((lambda (x) x) y))) 0)
  '((x y) . 1))

(test "bv-wnf 4"
  ((bv-wnf '((a b) ((lambda (x) x) y))) 0)
  '(((a b) y) . 1))

(test "bv-wnf 5"
  ((bv-wnf '((lambda (x) x) (a b))) 0)
  '((a b) . 1))

(test "bv-wnf 6"
  ((bv-wnf '((lambda (x) x) ((lambda (y) y) z))) 0)
  '(z . 2))

(test "bn-whnf 1"
  ((bn-whnf 'x) 0)
  '(x . 0))

(test "bn-whnf 2"
  ((bn-whnf '(lambda (x) x)) 0)
  '((lambda (x) x) . 0))

(test "bn-whnf 3"
  ((bn-whnf '(x ((lambda (x) x) y))) 0)
  '((x ((lambda (x) x) y)) . 0))

(test "bn-whnf 4"
  ((bn-whnf '((a b) ((lambda (x) x) y))) 0)
  '(((a b) ((lambda (x) x) y)) . 0))

(test "bn-whnf 5"
  ((bn-whnf '((lambda (x) x) (a b))) 0)
  '((a b) . 1))

(test "bn-whnf 6"
  ((bn-whnf '((lambda (x) x) ((lambda (y) y) z))) 0)
  '(z . 2))

(test "ao-nf 1"
  ((ao-nf 'x) 0)
  '(x . 0))

(test "ao-nf 2"
  ((ao-nf '(lambda (x) x)) 0)
  '((lambda (x) x) . 0))

(test "ao-nf 3"
  ((ao-nf '(x ((lambda (x) x) y))) 0)
  '((x y) . 1))

(test "ao-nf 4"
  ((ao-nf '((a b) ((lambda (x) x) y))) 0)
  '(((a b) y) . 1))

(test "ao-nf 5"
  ((ao-nf '((lambda (x) x) (a b))) 0)
  '((a b) . 1))

(test "ao-nf 6"
  ((ao-nf '((lambda (x) x) ((lambda (y) y) z))) 0)
  '(z . 2))

(test "no-nf 1"
  ((no-nf 'x) 0)
  '(x . 0))

(test "no-nf 2"
  ((no-nf '(lambda (x) x)) 0)
  '((lambda (x) x) . 0))

(test "no-nf 3"
  ((no-nf '(x ((lambda (x) x) y))) 0)
  '((x y) . 1))

(test "no-nf 4"
  ((no-nf '((a b) ((lambda (x) x) y))) 0)
  '(((a b) y) . 1))

(test "no-nf 5"
  ((no-nf '((lambda (x) x) (a b))) 0)
  '((a b) . 1))

(test "no-nf 6"
  ((no-nf '((lambda (x) x) ((lambda (y) y) z))) 0)
  '(z . 2))

(test "hn-nf 1"
  ((hn-nf 'x) 0)
  '(x . 0))

(test "hn-nf 2"
  ((hn-nf '(lambda (x) x)) 0)
  '((lambda (x) x) . 0))

(test "hn-nf 3"
  ((hn-nf '(x ((lambda (x) x) y))) 0)
  '((x y) . 1))

(test "hn-nf 4"
  ((hn-nf '((a b) ((lambda (x) x) y))) 0)
  '(((a b) y) . 1))

(test "hn-nf 5"
  ((hn-nf '((lambda (x) x) (a b))) 0)
  '((a b) . 1))

(test "hn-nf 6"
  ((hn-nf '((lambda (x) x) ((lambda (y) y) z))) 0)
  '(z . 2))

(test "jot-bv-wnf-interface 1"
  (jot-bv-wnf-interface '(0))
  '((lambda (y)
      (lambda (z) (((lambda (x) (lambda (y) x)) z) (y z))))
    .
    10))

(test "jot-bv-wnf-interface 2"
  (jot-bv-wnf-interface '(1))
  '((lambda (x) (lambda (y) ((lambda (x) x) (x y)))) . 0))

(test "jot-bv-wnf-interface 3"
  (jot-bv-wnf-interface '(1 1))
  '((lambda (x)
      (lambda (y)
        ((lambda (x) (lambda (y) ((lambda (x) x) (x y)))) (x y))))
    .
    0))

(test "jot-bv-wnf-interface 4"
  (jot-bv-wnf-interface '(1 0))
  '((lambda (y)
      (lambda (z) (((lambda (x) (lambda (y) x)) z) (y z))))
    .
    23))

(test "jot-bv-wnf-interface 5"
  (jot-bv-wnf-interface '(0 1))
  '((lambda (x)
      (lambda (y)
        ((lambda (y)
           (lambda (z) (((lambda (x) (lambda (y) x)) z) (y z))))
         (x y))))
    .
    10))

(test "jot-bv-wnf-interface 6"
  (jot-bv-wnf-interface '(0 0))
  '((lambda (x) (lambda (y) x)) . 43))

(test "jot-bv-wnf-interface 7"
  (jot-bv-wnf-interface '(1 1 1 0 0))
  '((lambda (x) (lambda (y) x)) . 89))

(test "jot-bv-wnf-interface 8"
  (jot-bv-wnf-interface '(1 1 1 1 1 0 0 0))
  '((lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))) . 118))

(test "jot-no-nf-interface 1"
  (jot-no-nf-interface '(0))
  '((lambda (y) (lambda (z) z)) . 13))

(test "jot-no-nf-interface 2"
  (jot-no-nf-interface '(1))
  '((lambda (x) (lambda (y) (x y))) . 1))

(test "jot-no-nf-interface 3"
  (jot-no-nf-interface '(1 0))
  '((lambda (y) (lambda (z) z)) . 21))

(test "jot-no-nf-interface 4"
  (jot-no-nf-interface '(0 1))
  '((lambda (x) (lambda (y) (lambda (z) z))) . 15))

(test "jot-no-nf-interface 5"
  (jot-no-nf-interface '(0 0))
  '((lambda (x) (lambda (y) x)) . 16))

(test "jot-no-nf-interface 6"
  (jot-no-nf-interface '(1 1 1 0 0))
  '((lambda (x) (lambda (y) x)) . 74))

(test "jot-no-nf-interface 7"
  (jot-no-nf-interface '(1 1 1 1 1 0 0 0))
  '((lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))) . 157))

(test "jot-bn-whnf-interface 1"
  (jot-bn-whnf-interface '(0))
  '((lambda (y) (lambda (z) (((lambda (x) (lambda (y) x)) z) (y z)))) . 10))

(test "jot-bn-whnf-interface 2"
  (jot-bn-whnf-interface '(1))
  '((lambda (x) (lambda (y) ((lambda (x) x) (x y)))) . 0))

(test "jot-bn-whnf-interface 3"
  (jot-bn-whnf-interface '(1 0))
  '((lambda (y) (lambda (z) (((lambda (x) (lambda (y) x)) z) (y z)))) . 23))

(test "jot-bn-whnf-interface 4"
  (jot-bn-whnf-interface '(0 1))
  '((lambda (x) (lambda (y) ((lambda (y) (lambda (z) (((lambda (x) (lambda (y) x)) z) (y z)))) (x y)))) . 10))

(test "jot-bn-whnf-interface 5"
  (jot-bn-whnf-interface '(0 0))
  '((lambda (x) (lambda (y) x)) . 34))

(test "jot-bn-whnf-interface 6"
  (jot-bn-whnf-interface '(1 1 1 0 0))
  '((lambda (x) (lambda (y) x)) . 82))

(test "jot-bn-whnf-interface 7"
  (jot-bn-whnf-interface '(1 1 1 1 1 0 0 0))
  '((lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))) . 133))

(test "jot-he-hnf (0)"
  (jot-he-hnf-interface '(0))
  '((lambda (y) (lambda (z) z)) . 13))

(test "jot-he-hnf (1)"
  (jot-he-hnf-interface '(1))
  '((lambda (x) (lambda (y) ((lambda (x) x) (x y)))) . 0))

;; (test "jot-he-hnf (1 0)"
;;   (jot-he-hnf-interface '(1 0))
;;   '(lambda (g0) (lambda (z) z)))

(test "jot-he-hnf (0 1)"
  (jot-he-hnf-interface '(0 1))
  '((lambda (x) (lambda (y) ((lambda (y) (lambda (z) z)) (x y)))) . 13))

(test "jot-he-hnf (0 0)"
  (jot-he-hnf-interface '(0 0))
  '((lambda (x) (lambda (y) x)) . 16))

(test "jot-he-hnf K"
  (jot-he-hnf-interface '(1 1 1 0 0))
  '((lambda (x) (lambda (y) x)) . 78))

(test "jot-he-hnf S"
  (jot-he-hnf-interface '(1 1 1 1 1 0 0 0))
  '((lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))) . 161))

(test "jot-ha-nf (0)"
  (jot-ha-nf-interface '(0))
  '((lambda (y) (lambda (z) z)) . 13))

(test "jot-ha-nf (1)"
  (jot-ha-nf-interface '(1))
  '((lambda (x) (lambda (y) (x y))) . 1))

(test "jot-ha-nf (1 0)"
  (jot-ha-nf-interface '(1 0))
  '((lambda (y) (lambda (z) z)) . 21))

(test "jot-ha-nf (0 1)"
  (jot-ha-nf-interface '(0 1))
  '((lambda (x) (lambda (y) (lambda (z) z))) . 15))

(test "jot-ha-nf (0 0)"
  (jot-ha-nf-interface '(0 0))
  '((lambda (x) (lambda (y) x)) . 16))

(test "jot-ha-nf K"
  (jot-ha-nf-interface '(1 1 1 0 0))
  '((lambda (x) (lambda (y) x)) . 74))

(test "jot-ha-nf S"
  (jot-ha-nf-interface '(1 1 1 1 1 0 0 0))
  '((lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))) . 157))

(test "jot-ao-nf-interface 1"
  (jot-ao-nf-interface '(0))
  '((lambda (y) (lambda (z) z)) . 13))

(test "jot-ao-nf-interface 2"
  (jot-ao-nf-interface '(1))
  '((lambda (x) (lambda (y) (x y))) . 1))

;; (test "jot-ao-nf-interface 3"
;;   (jot-ao-nf-interface '(1 1))
;;   '((lambda (x) (lambda (y) (lambda (g0) ((x y) g0)))) . 8))

;; (test "jot-ao-nf-interface 4"
;;   (jot-ao-nf-interface '(1 0))
;;   '((lambda (g2) (lambda (z) z)) . 34))

(test "jot-ao-nf-interface 5"
  (jot-ao-nf-interface '(0 1))
  '((lambda (x) (lambda (y) (lambda (z) z))) . 15))

(test "jot-ao-nf-interface 6"
  (jot-ao-nf-interface '(0 0))
  '((lambda (x) (lambda (y) x)) . 16))

(test "jot-ao-nf-interface 7"
  (jot-ao-nf-interface '(1 1 1 0 0))
  '((lambda (x) (lambda (y) x)) . 78))

(test "jot-ao-nf-interface 8"
  (jot-ao-nf-interface '(1 1 1 1 1 0 0 0))
  '((lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))) . 161))
;;

(test "jot-hn-nf-interface 2"
  (jot-hn-nf-interface '(1))
  '((lambda (x) (lambda (y) (x y))) . 1))

;; (test "jot-hn-nf-interface 3"
;;   (jot-hn-nf-interface '(1 0))
;;   '((lambda (g0) (lambda (z) z)) . 34))

(test "jot-hn-nf-interface 4"
  (jot-hn-nf-interface '(0 1))
  '((lambda (x) (lambda (y) (lambda (z) z))) . 15))

(test "jot-hn-nf-interface 5"
  (jot-hn-nf-interface '(0 0))
  '((lambda (x) (lambda (y) x)) . 16))

(test "jot-hn-nf-interface 6"
  (jot-hn-nf-interface '(1 1 1 0 0))
  '((lambda (x) (lambda (y) x)) . 78))

(test "jot-hn-nf-interface 7"
  (jot-hn-nf-interface '(1 1 1 1 1 0 0 0))
  '((lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))) . 161))

