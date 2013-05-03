
(test "free? sym"
  (free? 'x 'x)
  #t)

(test "free? sym"
  (free? 'x '(lambda (x) x))
  #f)

(test "free? sym"
  (free? 'y '(lambda (x) x))
  #f)

(test "free? sym"
  (free? 'x '(lambda (y) x))
  #t)

(test "free? sym"
  (free? 'x '(x (lambda (y) y)))
  #t)

(test "free? sym"
  (free? 'x '((lambda (y) y) x))
  #t)

(test "not-free? sym"
  (not-free? 'x 'x)
  #f)

(test "not-free? sym"
  (not-free? 'x '(lambda (x) x))
  #t)

(test "not-free? sym"
  (not-free? 'y '(lambda (x) x))
  #t)

(test "not-free? sym"
  (not-free? 'x '(lambda (y) x))
  #f)

(test "not-free? sym"
  (not-free? 'x '(x (lambda (y) y)))
  #f)

(test "not-free? sym"
  (not-free? 'x '((lambda (y) y) x))
  #f)

(test "beta sym 1"
  (beta '(lambda (x) x) 'x 'y)
  'y)

(test "beta sym 2"
  (beta '(lambda (x) x) 'x 'x)
  '(lambda (x) x))

(test "beta lambda 1"
  (beta '(lambda (x) x) 'x '(lambda (x) y))
  '(lambda (x) y))

(test "beta lambda 2"
  (beta '(lambda (x) x) 'x '(lambda (y) y))
  '(lambda (y) y))

(test "beta lambda 3"
  (beta '(lambda (x) x) 'x '(lambda (y) (lambda (z) y)))
  '(lambda (y) (lambda (z) y)))

;; (test "beta lambda 4"
;;   (beta '(lambda (x) y) 'x '(lambda (y) x))
;;   'how-to-test) ;; simply eval it with appropriate arguments and make sure you get the right values back out. But IIRC this was awful and nasty. 

(test "beta app"
  (beta '(lambda (x) x) 'x '(x x))
  '((lambda (x) x) (lambda (x) x)))

(test "bn sym"
  (bn 'x)
  'x)

(test "bn I"
  (bn '(lambda (x) x))
  '(lambda (x) x))

(test "bn app-1"
  (bn '(x ((lambda (x) x) y)))
  '(x ((lambda (x) x) y)))

(test "bn app-2"
  (bn '((a b) ((lambda (x) x) y)))
  '((a b) ((lambda (x) x) y)))

(test "bn app-3"
  (bn '((lambda (x) x) (a b)))
  '(a b))

(test "bn nested-app"
  (bn '((lambda (x) x) ((lambda (y) y) z)))
  'z)

(test "norm sym"
  (norm 'x)
  'x)

(test "norm I"
  (norm '(lambda (x) x))
  '(lambda (x) x))

(test "norm app-1"
  (norm '(x ((lambda (x) x) y)))
  '(x y))

(test "norm app-2"
  (norm '((a b) ((lambda (x) x) y)))
  '((a b) y))

(test "norm app-3"
  (norm '((lambda (x) x) (a b)))
  '(a b))

(test "norm nested-app"
  (norm '((lambda (x) x) ((lambda (y) y) z)))
  'z)

(test "jot (0)"
  (jot-interface '(0))
  '(lambda (y) (lambda (z) z)))

(test "jot (1)"
  (jot-interface '(1))
  '(lambda (x) (lambda (y) (x y))))

(test "jot (1 1)"
  ((((eval (jot-interface '(1 1))) (lambda (f) (lambda (g) (+ f g)))) 2) 3)
  '5)

(test "jot (1 0)"
  (jot-interface '(1 0))
  '(lambda (y) (lambda (z) z)))

(test "jot (0 1)"
  (jot-interface '(0 1))
  '(lambda (x) (lambda (y) (lambda (z) z))))

(test "jot (0 0)"
  (jot-interface '(0 0))
  '(lambda (x) (lambda (y) x)))

(test "jot K"
  (jot-interface '(1 1 1 0 0))
  '(lambda (x) (lambda (y) x)))

(test "jot S"
  (jot-interface '(1 1 1 1 1 0 0 0))
  '(lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))



(test "10 freeo" 
  (run 10 (q) (fresh (a b) (freeo a b) (== `(,a ,b) q)))
  '(((_.0 _.0) (sym _.0)) ((_.0 (lambda (_.1) _.0)) (=/= ((_.0 _.1))) (sym _.0 _.1))
  ((_.0 (_.0 _.1)) (sym _.0))
  ((_.0 (lambda (_.1) (lambda (_.2) _.0)))
    (=/= ((_.0 _.1)) ((_.0 _.2)))
    (sym _.0 _.1 _.2))
  ((_.0 (_.1 _.0)) (=/= ((_.1 _.0))) (sym _.0 _.1))
  ((_.0 (lambda (_.1) (_.0 _.2)))
    (=/= ((_.0 _.1)))
    (sym _.0 _.1))
  ((_.0 ((lambda (_.1) _.0) _.2))
    (=/= ((_.0 _.1)))
    (sym _.0 _.1))
  ((_.0 (lambda (_.1) (lambda (_.2) (lambda (_.3) _.0))))
    (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.0 _.3)))
    (sym _.0 _.1 _.2 _.3))
  ((_.0 ((_.0 _.1) _.2)) (sym _.0))
  ((_.0 (lambda (_.1) (_.2 _.0)))
    (=/= ((_.0 _.1)) ((_.2 _.0)))
    (sym _.0 _.1 _.2))))

(test "10 not-freeo"
  (run 10 (q) (fresh (a b) (not-freeo a b) (== `(,a ,b) q)))
  '(((_.0 _.1) (=/= ((_.1 _.0))) (sym _.0 _.1)) ((_.0 (lambda (_.0) _.1)) (sym _.0))
  ((_.0 (lambda (_.1) _.2))
    (=/= ((_.0 _.1)) ((_.2 _.0)))
    (sym _.0 _.1 _.2))
  ((_.0 (_.1 _.2))
    (=/= ((_.1 _.0)) ((_.2 _.0)))
    (sym _.0 _.1 _.2))
  ((_.0 (lambda (_.1) (lambda (_.0) _.2)))
    (=/= ((_.0 _.1)))
    (sym _.0 _.1))
  ((_.0 (lambda (_.1) (lambda (_.2) _.3)))
    (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.3 _.0)))
    (sym _.0 _.1 _.2 _.3))
  ((_.0 (_.1 (lambda (_.0) _.2)))
    (=/= ((_.1 _.0)))
    (sym _.0 _.1))
  ((_.0 ((lambda (_.0) _.1) _.2))
    (=/= ((_.2 _.0)))
    (sym _.0 _.2))
  ((_.0 (lambda (_.1) (_.2 _.3)))
    (=/= ((_.0 _.1)) ((_.2 _.0)) ((_.3 _.0)))
    (sym _.0 _.1 _.2 _.3))
  ((_.0 (lambda (_.1) (lambda (_.2) (lambda (_.0) _.3))))
    (=/= ((_.0 _.1)) ((_.0 _.2)))
    (sym _.0 _.1 _.2))))

(test "10 wwfo"
  (run 10 (q) (wffo q))
  '((_.0 (sym _.0)) ((lambda (_.0) _.1) (sym _.0 _.1))
    ((_.0 _.1) (sym _.0 _.1))
    ((lambda (_.0) (lambda (_.1) _.2)) (sym _.0 _.1 _.2))
    ((lambda (_.0) (_.1 _.2)) (sym _.0 _.1 _.2))
    ((_.0 (lambda (_.1) _.2)) (sym _.0 _.1 _.2))
    (((lambda (_.0) _.1) _.2) (sym _.0 _.1 _.2))
    ((lambda (_.0) (lambda (_.1) (lambda (_.2) _.3)))
     (sym _.0 _.1 _.2 _.3))
    ((_.0 (_.1 _.2)) (sym _.0 _.1 _.2))
    ((lambda (_.0) (lambda (_.1) (_.2 _.3)))
     (sym _.0 _.1 _.2 _.3))))

(test "beta-symbol-test-1"
  (run 10 (q) (fresh (x y)
		(symbolo* x y)
		(=/= x y)
		(betao `(lambda (,x) ,x) x y q)))
  '((_.0 (sym _.0))))

(test "beta-symbol-test-2"
  (run 10 (q) (fresh (x y)
		(symbolo x)
		(betao `(lambda (,x) ,x) x x q)))
  '(((lambda (_.0) _.0) (sym _.0))))

(test "beta-lambda-test1"
  (run 10 (q) (fresh (x y)
		(symbolo* x y)
		(=/= x y)
		(betao `(lambda (,x) ,x) x `(lambda (,x) ,y) q)))
  '(((lambda (_.0) _.1) (=/= ((_.0 _.1))) (sym _.0 _.1))))

(test "beta-lambda-test2"
  (run 10 (q) (fresh (x y) (symbolo* x y) (=/= x y)
		     (betao `(lambda (,x) ,x) x `(lambda (,y) ,y) q)))
  '(((lambda (_.0) _.0) (sym _.0))))

(test "beta-lambda-test3"
  (run 10 (q) (fresh (x y z)
		(symbolo* x y z)
		(=/=* x y z)
		(betao `(lambda (,x) ,x) x `(lambda (,y) (lambda (,z) ,y)) q)))
  '(((lambda (_.0) (lambda (_.1) _.0)) 
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))))

(test "beta-app-test"
  (run 10 (q) (fresh (x y z)
		(symbolo* x y z)
		(=/=* x y z)
		(betao `(lambda (,x) ,x) x `((lambda (,y) (lambda (,z) ,y)) (lambda (,x) ,x)) q)))
  '((((lambda (_.0) (lambda (_.1) _.0)) (lambda (_.2) _.2))
   (=/= ((_.0 _.1)) ((_.2 _.0)) ((_.2 _.1)))
   (sym _.0 _.1 _.2))))

(test "bno-var"
  (run 1 (q) (fresh (i o) (bno i o) (== `(,i ,o) q)))
  '(((_.0 _.0) (sym _.0))))

(test "bno-abstraction"
  (run 10 (q) (fresh (i) (symbolo i) (bno `(lambda (,i) ,i) q)))
  '(((lambda (_.0) _.0) (sym _.0))))

(test "bno-app1"
  (run 10 (q) (fresh (i) (symbolo i) (bno `((lambda (,i) ,i) (lambda (,i) ,i)) q)))
  '(((lambda (_.0) _.0) (sym _.0))))

(test "bno-app2"
  (run 10 (q) (fresh (x y) (symbolo* x y) (bno `((lambda (,x) ,x) (lambda (,y) ,y)) q)))
  '(((lambda (_.0) _.0) (sym _.0))))

(test "bno-nested-app"
  (run 10 (q) (fresh (x y z)
		(symbolo* x y z)
		(=/=* x y z)
		(bno `((lambda (,x) ((lambda (,y) (lambda (,z) ,y)) (lambda (,x) ,x))) (lambda (,x) ,x)) q)))
  '(((lambda (_.0) (lambda (_.1) _.1))
   (=/= ((_.1 _.0)))
   (sym _.0 _.1))))

(test "bno-nested-app-big"
  (run 10 (q) (fresh (a b c d)
	       (symbolo* a b c d)
	       (=/=* a b c d)
	       (bno `((lambda (,d) ((lambda (,b) (,a ,c)) (,d ,c))) ((lambda (,a) ,a) ,a)) q)))
  '(((_.0 _.1) (=/= ((_.0 _.1))) (sym _.0 _.1))))

(test "bno-backward lambda"
  (run 20 (q) (fresh (x) (symbolo x) (bno q `(lambda (,x) ,x))))
  '(((lambda (_.0) _.0) (sym _.0)) (((lambda (_.0) _.0) (lambda (_.1) _.1)) (sym _.0 _.1))
  (((lambda (_.0) (lambda (_.0) _.0)) _.1) (sym _.0))
  ((((lambda (_.0) _.0) (lambda (_.1) _.1))
     (lambda (_.2) _.2))
    (sym _.0 _.1 _.2))
  (((lambda (_.0) _.0)
     ((lambda (_.1) _.1) (lambda (_.2) _.2)))
    (sym _.0 _.1 _.2))
  ((((lambda (_.0) _.0) (lambda (_.1) (lambda (_.1) _.1)))
     _.2)
    (sym _.0 _.1))
  (((lambda (_.0) _.0)
     ((lambda (_.1) (lambda (_.1) _.1)) _.2))
    (sym _.0 _.1))
  ((((lambda (_.0) (lambda (_.0) _.0)) _.1)
     (lambda (_.2) _.2))
    (sym _.0 _.2))
  ((((lambda (_.0) _.0) (lambda (_.1) _.1))
     ((lambda (_.2) _.2) (lambda (_.3) _.3)))
    (sym _.0 _.1 _.2 _.3))
  (((lambda (_.0) (_.0 _.0)) (lambda (_.1) _.1))
    (sym _.0 _.1))
  (((lambda (_.0) (lambda (_.1) _.1)) _.2)
    (=/= ((_.0 _.1)))
    (sym _.0 _.1))
  ((((lambda (_.0) (lambda (_.0) (lambda (_.0) _.0))) _.1)
     _.2)
    (sym _.0))
  (((lambda (_.0) (_.0 _.0))
     (lambda (_.1) (lambda (_.1) _.1)))
    (sym _.0 _.1))
  (((lambda (_.0) _.0)
     (((lambda (_.1) _.1) (lambda (_.2) _.2))
       (lambda (_.3) _.3)))
    (sym _.0 _.1 _.2 _.3))
  (((lambda (_.0) _.0)
     ((lambda (_.1) _.1)
       ((lambda (_.2) _.2) (lambda (_.3) _.3))))
    (sym _.0 _.1 _.2 _.3))
  ((((lambda (_.0) _.0) (lambda (_.1) _.1))
     ((lambda (_.2) (lambda (_.2) _.2)) _.3))
    (sym _.0 _.1 _.2))
  ((((lambda (_.0) (lambda (_.0) _.0)) _.1)
     ((lambda (_.2) _.2) (lambda (_.3) _.3)))
    (sym _.0 _.2 _.3))
  ((((lambda (_.0) _.0) (lambda (_.1) (_.1 _.1)))
     (lambda (_.2) _.2))
    (sym _.0 _.1 _.2))
  (((lambda (_.0) _.0)
     (((lambda (_.1) _.1) (lambda (_.2) (lambda (_.2) _.2)))
       _.3))
    (sym _.0 _.1 _.2))
  ((((lambda (_.0) _.0) (lambda (_.1) (lambda (_.2) _.2)))
     _.3)
    (=/= ((_.1 _.2)))
    (sym _.0 _.1 _.2))))

(test "bno-backward-symbol"
  (run 10  (q) (fresh (x) (symbolo x) (bno q x)))
  '((_.0 (sym _.0)) (((lambda (_.0) _.0) _.1) (sym _.0 _.1))
  (((lambda (_.0) _.1) _.2) (=/= ((_.0 _.1))) (sym _.0 _.1))
  (((lambda (_.0) _.0) ((lambda (_.1) _.1) _.2))
    (sym _.0 _.1 _.2))
  ((((lambda (_.0) _.0) (lambda (_.1) _.1)) _.2)
    (sym _.0 _.1 _.2))
  ((((lambda (_.0) _.0) (lambda (_.1) _.2)) _.3)
    (=/= ((_.1 _.2)))
    (sym _.0 _.1 _.2))
  (((lambda (_.0) _.0) ((lambda (_.1) _.2) _.3))
    (=/= ((_.1 _.2)))
    (sym _.0 _.1 _.2))
  ((((lambda (_.0) (lambda (_.0) _.0)) _.1) _.2)
    (sym _.0 _.2))
  ((((lambda (_.0) _.0) (lambda (_.1) _.1))
     ((lambda (_.2) _.2) _.3))
    (sym _.0 _.1 _.2 _.3))
  ((((lambda (_.0) (lambda (_.0) _.1)) _.2) _.3)
    (=/= ((_.0 _.1)))
    (sym _.0 _.1))))

(test "normo-var"
  (run 1 (q) (fresh (i o) (normo i o) (== `(,i ,o) q)))
  '(((_.0 _.0) (sym _.0))))

(test "normo-abstraction"
  (run 10 (q) (fresh (i) (symbolo i) (normo `(lambda (,i) ,i) q)))
  '(((lambda (_.0) _.0) (sym _.0))))

(test "normo-app1"
  (run 10 (q) (fresh (i) (symbolo i) (normo `((lambda (,i) ,i) (lambda (,i) ,i)) q)))
  '(((lambda (_.0) _.0) (sym _.0))))

(test "normo-app2"
  (run 10 (q) (fresh (x y) (symbolo* x y) (normo `((lambda (,x) ,x) (lambda (,y) ,y)) q)))
  '(((lambda (_.0) _.0) (sym _.0))))

(test "normo-nested-app"
  (run 10 (q) (fresh (x y z)
		(symbolo* x y z)
		(=/=* x y z)
		(normo `((lambda (,x) ((lambda (,y) (lambda (,z) ,y)) (lambda (,x) ,x))) (lambda (,x) ,x)) q)))
  '(((lambda (_.0) (lambda (_.1) _.1))
   (=/= ((_.1 _.0)))
   (sym _.0 _.1))))

(test "normo-nested-app-big"
  (run 10 (q) (fresh (a b c d)
	       (symbolo* a b c d)
	       (=/=* a b c d)
	       (normo `((lambda (,d) ((lambda (,b) (,a ,c)) (,d ,c))) ((lambda (,a) ,a) ,a)) q)))
  '(((_.0 _.1) (=/= ((_.0 _.1))) (sym _.0 _.1))))

(test "normo-backward lambda"
  (run 20 (q) (fresh (x) (symbolo x) (normo q `(lambda (,x) ,x))))
  '(((lambda (_.0) _.0) (sym _.0)) ((lambda (_.0) ((lambda (_.1) _.1) _.0)) (sym _.0 _.1))
  ((lambda (_.0) ((lambda (_.1) _.0) _.2))
    (=/= ((_.1 _.0)))
    (sym _.0 _.1))
  (((lambda (_.0) _.0) (lambda (_.1) _.1)) (sym _.0 _.1))
  (((lambda (_.0) (lambda (_.0) _.0)) _.1) (sym _.0))
  ((lambda (_.0)
     ((lambda (_.1) _.1) ((lambda (_.2) _.2) _.0)))
    (sym _.0 _.1 _.2))
  ((lambda (_.0)
     (((lambda (_.1) _.1) (lambda (_.2) _.2)) _.0))
    (sym _.0 _.1 _.2))
  ((lambda (_.0)
     (((lambda (_.1) _.1) (lambda (_.2) _.0)) _.3))
    (=/= ((_.2 _.0)))
    (sym _.0 _.1 _.2))
  ((lambda (_.0)
     ((lambda (_.1) _.1) ((lambda (_.2) _.0) _.3)))
    (=/= ((_.2 _.0)))
    (sym _.0 _.1 _.2))
  ((((lambda (_.0) _.0) (lambda (_.1) _.1))
     (lambda (_.2) _.2))
    (sym _.0 _.1 _.2))
  ((((lambda (_.0) _.0) (lambda (_.1) (lambda (_.1) _.1)))
     _.2)
    (sym _.0 _.1))
  (((lambda (_.0) _.0)
     (lambda (_.1) ((lambda (_.2) _.2) _.1)))
    (sym _.0 _.1 _.2))
  (((lambda (_.0) (lambda (_.0) ((lambda (_.1) _.1) _.0)))
     _.2)
    (sym _.0 _.1))
  (((lambda (_.0) _.0)
     (lambda (_.1) ((lambda (_.2) _.1) _.3)))
    (=/= ((_.2 _.1)))
    (sym _.0 _.1 _.2))
  ((lambda (_.0)
     (((lambda (_.1) (lambda (_.1) _.1)) _.2) _.0))
    (sym _.0 _.1))
  (((lambda (_.0) (lambda (_.0) ((lambda (_.1) _.0) _.2)))
     _.3)
    (=/= ((_.1 _.0)))
    (sym _.0 _.1))
  ((lambda (_.0)
     (((lambda (_.1) _.1) (lambda (_.2) _.2))
       ((lambda (_.3) _.3) _.0)))
    (sym _.0 _.1 _.2 _.3))
  ((lambda (_.0)
     (((lambda (_.1) (lambda (_.1) _.0)) _.2) _.3))
    (=/= ((_.1 _.0)))
    (sym _.0 _.1))
  (((lambda (_.0) _.0)
     ((lambda (_.1) _.1) (lambda (_.2) _.2)))
    (sym _.0 _.1 _.2))
  ((lambda (_.0)
     ((lambda (_.1) (_.1 _.1)) (lambda (_.2) _.0)))
    (=/= ((_.2 _.0)))
    (sym _.0 _.1 _.2))))

(test "normo-backward-symbol"
  (run 10  (q) (fresh (x) (symbolo x) (normo q x)))
  '((_.0 (sym _.0)) (((lambda (_.0) _.0) _.1) (sym _.0 _.1))
  (((lambda (_.0) _.1) _.2) (=/= ((_.0 _.1))) (sym _.0 _.1))
  (((lambda (_.0) _.0) ((lambda (_.1) _.1) _.2))
    (sym _.0 _.1 _.2))
  ((((lambda (_.0) _.0) (lambda (_.1) _.1)) _.2)
    (sym _.0 _.1 _.2))
  ((((lambda (_.0) _.0) (lambda (_.1) _.2)) _.3)
    (=/= ((_.1 _.2)))
    (sym _.0 _.1 _.2))
  (((lambda (_.0) _.0) ((lambda (_.1) _.2) _.3))
    (=/= ((_.1 _.2)))
    (sym _.0 _.1 _.2))
  ((((lambda (_.0) (lambda (_.0) _.0)) _.1) _.2)
    (sym _.0 _.2))
  ((((lambda (_.0) _.0) (lambda (_.1) _.1))
     ((lambda (_.2) _.2) _.3))
    (sym _.0 _.1 _.2 _.3))
  ((((lambda (_.0) (lambda (_.0) _.1)) _.2) _.3)
    (=/= ((_.0 _.1)))
    (sym _.0 _.1))))

(test "church-oleg 0"
  (run 10 (q) (church->olego `(lambda (s) (lambda (z) z)) q))
  '(()))

(test "church-oleg 1"
  (run 10 (q) (church->olego `(lambda (s) (lambda (z) (s z))) q))
  '((1)))

(test "church-oleg bigger"
  (run 10 (q) (church->olego `(lambda (s) (lambda (z) (s (s (s (s z)))))) q))
  '((0 0 1)))

(test "church-oleg backward 0"
  (run 1 (q) (church->olego q '()))
  '(((lambda (_.0) (lambda (_.1) _.1))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))))

(test "church-oleg backward"
  (run 1 (q) (church->olego q '(0 1)))
  '(((lambda (_.0) (lambda (_.1) (_.0 (_.0 _.1))))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))))

(test "joto 10 (0)"
  (run 10 (q) (joto-interface '(0) q))
  '(((lambda (_.0) (lambda (_.1) _.1))
   (=/= ((_.0 _.1)))
   (sym _.0 _.1))))

(test "joto 10 (1)"
  (run 10 (q) (joto-interface '(1) q))
  '(((lambda (_.0) (lambda (_.1) (_.0 _.1))) 
   (=/= ((_.0 _.1)))
   (sym _.0 _.1))))

(test "joto 10 (1 1)" ;; alpha equivalence?
  (run 10 (q) (joto-interface '(1 1) q))
  '(((lambda (_.0)
    (lambda (_.1) (lambda (_.2) ((_.0 _.1) _.2))))
   (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.1 _.2)))
   (sym _.0 _.1 _.2))
  ((lambda (_.0)
     (lambda (_.1) (lambda (_.2) ((_.0 _.1) _.2))))
    (=/= ((_.0 _.1)) ((_.1 _.2)) ((_.2 _.0)))
    (sym _.0 _.1 _.2))
  ((lambda (_.0)
     (lambda (_.1) (lambda (_.2) ((_.0 _.1) _.2))))
    (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.2 _.1)))
    (sym _.0 _.1 _.2))))

(test "joto 10 (1 0)" ;; not alpha equiv. multiple paths through. Must fix
  (run 10 (q) (joto-interface '(1 0) q))
  '(((lambda (_.0) (lambda (_.1) _.1))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))
    ((lambda (_.0) (lambda (_.1) _.1))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))
    ((lambda (_.0) (lambda (_.1) _.1))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))
    ((lambda (_.0) (lambda (_.1) _.1))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))))

(test "joto 10 (0 1)"
  (run 10 (q) (joto-interface '(0 1) q))
  '(((lambda (_.0) (lambda (_.1) (lambda (_.2) _.2)))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1 _.2))))

(test "joto 10 (0 0)"
  (run 10 (q) (joto-interface '(0 0) q))
  '(((lambda (_.0) (lambda (_.1) _.0))
   (=/= ((_.0 _.1)))
   (sym _.0 _.1))))

(test "joto-K" ;; again, not alpha equiv. WTF? Must be some conde bs
  (run 5 (q) (joto-interface '(1 1 1 0 0) q))
  '(((lambda (_.0) (lambda (_.1) _.0))
   (=/= ((_.0 _.1)))
   (sym _.0 _.1))
  ((lambda (_.0) (lambda (_.1) _.0))
    (=/= ((_.0 _.1)))
    (sym _.0 _.1))
  ((lambda (_.0) (lambda (_.1) _.0))
    (=/= ((_.0 _.1)))
    (sym _.0 _.1))
  ((lambda (_.0) (lambda (_.1) _.0))
    (=/= ((_.0 _.1)))
    (sym _.0 _.1))
  ((lambda (_.0) (lambda (_.1) _.0))
    (=/= ((_.0 _.1)))
    (sym _.0 _.1))))

(test "joto-church"
  (run 1 (q) (fresh (b) (joto-interface q b) (church->olego b q)))
  '((1)))

(test "joto-S"
  (run 3 (q) (joto-interface '(1 1 1 1 1 0 0 0) q))
  '(((lambda (_.0)
    (lambda (_.1) (lambda (_.2) ((_.0 _.2) (_.1 _.2)))))
   (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.1 _.2)))
   (sym _.0 _.1 _.2))
  ((lambda (_.0)
     (lambda (_.1) (lambda (_.2) ((_.0 _.2) (_.1 _.2)))))
    (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.1 _.2)))
    (sym _.0 _.1 _.2))
  ((lambda (_.0)
     (lambda (_.1) (lambda (_.2) ((_.0 _.2) (_.1 _.2)))))
    (=/= ((_.0 _.1)) ((_.0 _.2)) ((_.1 _.2)))
    (sym _.0 _.1 _.2))))


