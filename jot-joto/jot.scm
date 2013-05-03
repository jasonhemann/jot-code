(load "pmatch.scm")
(load "mk.scm")
(load "test.scm")
(load "minikanren.scm")

(define S '(lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
(define K '(lambda (x) (lambda (y) x)))
(define I '(lambda (x) x))

(define free?
  (lambda (x e)
    (pmatch e
      (,e (guard (symbol? e)) (eq? x e))
      ((lambda (,y) ,body) (guard (symbol? y)) (and (not (eq? y x)) (free? x body)))
      ((,rator ,rand) (or (free? x rator) (free? x rand))))))

(define not-free?
  (lambda (x e)
    (pmatch e
      (,e (guard (symbol? e)) (not (eq? e x)))
      ((lambda (,y) ,body) (guard (symbol? y)) (or (eq? y x) (not-free? x body)))
      ((,rator ,rand) (and (not-free? x rator) (not-free? x rand))))))

(define beta
  (lambda (M x e)
    (pmatch e
      (,e (guard (symbol? e) (eq? e x)) M)
      (,e (guard (symbol? e) (not (eq? e x))) e)
      ((lambda (,y) ,body) (guard (eq? x y)) `(lambda (,y) ,body))
      ((lambda (,y) ,body) (guard (not (eq? x y)) (not-free? x body))
       `(lambda (,y) ,(beta M x body)))
      ((lambda (,y) ,body) (guard (not (eq? x y)) (free? x body) (not-free? y M))
       `(lambda (,y) ,(beta M x body)))
      ((lambda (,y) ,body) (guard (not (eq? x y)) (free? x body) (free? y M))
       (let ((g (gensym))) ;; g =/= x
	                   ;; g \not\in fv e
	                   ;; g \not\in fv M
	 `(lambda (,g) ,(beta M x (beta g y body)))))
      ((,rator ,rand) `(,(beta M x rator) ,(beta M x rand))))))

(define bv
  (lambda (exp)
    (pmatch exp
      [,exp (guard (symbol? exp)) exp]
      [(lambda (,x) ,body) (guard (symbol? x)) exp]
      [(,rator ,rand)
       (let ((v-rator (bv rator)))
	 (pmatch v-rator
	   [,v-rator (guard (symbol? v-rator))
		     `(,v-rator ,(bv rand))]
	   [(lambda (,x) ,body) (guard (symbol? x))
	    (bv (beta (bv rand) x body))]
	   [(,v-rat-rat ,v-rat-ran) `(,v-rator ,(bv rand))]))])))

(define bn
  (lambda (exp)
    (pmatch exp
      [,exp (guard (symbol? exp)) exp]
      [(lambda (,x) ,body) (guard (symbol? x)) exp]
      [(,rator ,rand)
       (let ((v-rator (bn rator)))
	 (pmatch v-rator
	   [,v-rator (guard (symbol? v-rator))
		     `(,v-rator ,rand)]
	   [(lambda (,x) ,body) (guard (symbol? x))
	    (bn (beta rand x body))]
	   [(,v-rat-rat ,v-rat-ran) `(,v-rator ,rand)]))])))

(define norm
  (lambda (exp)
    (pmatch exp
      [,exp (guard (symbol? exp)) exp]
      [(lambda (,x) ,body) (guard (symbol? x)) `(lambda (,x) ,(norm body))]
      [(,rator ,rand)
       (let ((v-rator (bn rator)))
	 (pmatch v-rator
	   [,v-rator (guard (symbol? v-rator))
		     `(,v-rator ,(norm rand))]
	   [(lambda (,x) ,body) (guard (symbol? x))
	    (norm (beta rand x body))]
	   [(,v-rat-rat ,v-rat-ran) ;; maybe norm rator, then whole exp?
	    `(,(norm v-rator) ,(norm rand))]))])))

(define jot
  (lambda (bls v)
    (pmatch bls
      (() v)
      ((1 . ,dbls) (let ((n-v (norm `(lambda (x) (lambda (y) (,v (x y)))))))
                     (jot dbls n-v)))
      ((0 . ,dbls) (let ((n-v (norm `((,v (lambda (x) (lambda (y) (lambda (z) ((x z) (y z)))))) (lambda (x) (lambda (y) x))))))
		     (jot dbls n-v))))))

(define jot-bv
  (lambda (bls v)
    (pmatch bls
      (() v)
      ((1 . ,dbls) (jot-bv dbls `(lambda (x) (lambda (y) (,v (x y))))))
      ((0 . ,dbls) (let ((n-v (bv `((,v (lambda (x) (lambda (y) (lambda (z) ((x z) (y z)))))) (lambda (x) (lambda (y) x))))))
		     (jot-bv dbls n-v))))))

(define jot-interface
  (lambda (bls)
    (jot bls '(lambda (x) x))))

(define jot-bv-interface
  (lambda (bls)
    (jot-bv bls '(lambda (x) x))))

(define church?
  (lambda (c)
    (pmatch c
      ((lambda (,s) (lambda (,z) ,body)) (guard (symbol? x) (symbol? y) (not (eq? s z))) (c-help s z body)))))

(define c-help
  (lambda (s z body)
    (pmatch body
      (,body (guard (eq? body z)) #t)
      ((,hd ,rec) (guard (eq? hd s))
       (c-help s z rec)))))

(print-gensym #f)


(define lam->cl
  (lambda (exp)
    (pmatch exp
      [,exp (guard (symbol? exp)) exp]
      [(lambda (,x) ,body) (guard (symbol? x) (not-free? x body)) `(K ,(lam->cl body))]
      [(lambda (,x) ,y) (guard (symbol? x) (eq? x y)) '((S K) K)]
      [(lambda (,x) (lambda (,y) ,body)) (guard (symbol? x) (symbol? y) (free? x body))
       (lam->cl `(lambda (,x) ,(lam->cl `(lambda (,y) ,body))))]
      [(lambda (,x) (,rator ,rand)) (guard (symbol? x) (free? x rator))
       `((S ,(lam->cl `(lambda (,x) ,rator))) ,(lam->cl `(lambda (,x) ,rand)))]
      [(lambda (,x) (,rator ,rand)) (guard (symbol? x) (not-free? x rator) (free? x rand))
       `((S ,(lam->cl `(lambda (,x) ,rator))) ,(lam->cl `(lambda (,x) ,rand)))]
      [(,rator ,rand) `(,(lam->cl rator) ,(lam->cl rand))])))

(define cl->jot
  (lambda (cl)
    (pmatch cl
      [S '(1 1 1 1 1 0 0 0)]
      [K '(1 1 1 0 0)]
      [(,A ,B) (cons 1 (append (cl->jot A) (cl->jot B)))])))


(define-syntax symbolo*
  (syntax-rules ()
    ((_ a as ...) (fresh () (symbolo a) (symbolo as) ...))))

(define-syntax =/=*
  (syntax-rules ()
    ((_ d0 d1) (=/= d0 d1))
    ((_ d0 d1 d2 ...) (fresh () (=/= d0 d1) (=/= d0 d2) ... (=/=* d1 d2 ...)))))

(define-syntax trace-define-mk
  (syntax-rules ()
    ((_ name (lambda (a* ...) body))
     (define name
       (lambda (a* ...)
         (fresh ()
           (project (a* ...)
             (begin
               (printf "~s\n" (list 'name a* ...))
               succeed))
           body))))
    ((_ (name a* ...) body)
     (define (name a* ...)
       (fresh ()
         (project (a* ...)
           (begin
             (printf "~s\n" (list 'name a* ...))
             succeed))
         body)))))

(define freeo
  (lambda (x e)
    (fresh ()
      (symbolo x)
      (conde
	((== x e))
	((fresh (y body)
	   (== `(lambda (,y) ,body) e)
	   (symbolo y)
	   (=/= x y)
	   (freeo x body)))
	((fresh (rator rand)
	   (== `(,rator ,rand) e)
	   (conde
	     ((freeo x rator)) ;; wffo 
	     ((not-freeo x rator) (freeo x rand)))))))))

;; Needs to be a constraint
(define wffo
  (lambda (exp)
    (conde
      ((symbolo exp))
      ((fresh (x body)
	 (== `(lambda (,x) ,body) exp)
	 (symbolo x)
	 (wffo body)))
      ((fresh (rator rand)
	 (== `(,rator ,rand) exp)
	 (wffo rator)
	 (wffo rand))))))

(define not-freeo
  (lambda (x e)
    (fresh ()
      (symbolo x)
      (conde
	((symbolo e) (=/= e x))
	((fresh (y body)
	   (== `(lambda (,y) ,body) e)
	   (symbolo y)
	   (conde
	     ((== x y)) ;; took out wffo body 
	     ((=/= x y) (not-freeo x body)))))
	((fresh (rator rand)
	   (== `(,rator ,rand) e)
	   (not-freeo x rator)
	   (not-freeo x rand)))))))

(define betao
  (lambda (M x e o)
    (fresh ()
      (symbolo x)
      (conde
	((== x e) (== M o)) 
	((symbolo e) (=/= x e)  (== e o)) ;; wffo M
	((fresh (body)
	   (== `(lambda (,x) ,body) e)
	   (== e o) ;; wffo body wffo M 
	   ))
	((fresh (y body)
	   (== `(lambda (,y) ,body) e)
	   (symbolo y)
	   (=/= x y)
	   (fresh (rec1)
	     (conde
	       ((not-freeo x body)
		(betao M x body rec1)
		(== `(lambda (,y) ,rec1) o))
	       ((freeo x body)
		(conde
		  ((not-freeo y M)
		   (betao M x body rec1)
		   (== `(lambda (,y) ,rec1) o))
		  ((freeo y M)
		   (fresh (g rec2)
		     (symbolo g)
		     (=/= g x)
		     (=/= g y)
		     (not-freeo g body) ; changed to body from e
		     (not-freeo g M)
		     (betao g y body rec1)
		     (betao M x rec1 rec2)
		     (== `(lambda (,g) ,rec2) o)))))))))
	((fresh (rator rand)
	   (== `(,rator ,rand) e)
	   (fresh (recrator recrand)
	     (betao M x rand recrand)
	     (betao M x rator recrator)
	     (== `(,recrator ,recrand) o))))))))

(define bvo
  (lambda (exp o)
    (conde
      ((symbolo exp) (== exp o))
      ((fresh (x body)
	 (== `(lambda (,x) ,body) exp)
	 (symbolo x)
	 (== exp o)))
      ((fresh (rator rand v-rator)
	 (== `(,rator ,rand) exp)
	 (bvo rator v-rator)
	 (fresh (v-rand)
	   (bvo rand v-rand)
	   (conde
	     ((symbolo v-rator)
	      (== `(,v-rator ,v-rand) o))
	     ((fresh (x body rec)
		(== `(lambda (,x) ,body) v-rator)
		(symbolo x)
		(betao v-rand x body rec)
		(bvo rec o)))
	     ((fresh (v-rat-rat v-rat-ran)
		(== `(,v-rat-rat ,v-rat-ran) v-rator)
		(== `(,v-rator ,v-rand) o))))))))))

(define bno
  (lambda (exp o)
    (conde
      ((symbolo exp) (== exp o))
      ((fresh (x body)
	 (== `(lambda (,x) ,body) exp)
	 (symbolo x)
	 (== exp o)))
      ((fresh (rator rand v-rator)
	 (== `(,rator ,rand) exp)
	 (bno rator v-rator) ;; perhaps its this step
	 (conde
	   ((symbolo v-rator) (== `(,v-rator ,rand) o))
	   ((fresh (x body b-rec)
	      (== `(lambda (,x) ,body) v-rator)
	      (symbolo x)
	      (betao rand x body b-rec)
	      (bno b-rec o)))
	   ((fresh (v-rat-rat v-rat-ran)
	      (== `(,v-rat-rat ,v-rat-ran) v-rator)
	      (== `(,v-rator ,rand) o)))))))))

(define normo
  (lambda (exp o)
    (conde
      ((symbolo exp) (== exp o))
      ((fresh (x body r-body)
	 (== `(lambda (,x) ,body) exp)
	 (== `(lambda (,x) ,r-body) o)
         (symbolo x)
	 (normo body r-body)))
      ((fresh (rator rand v-rator)
	 (== `(,rator ,rand) exp)
	 (bno rator v-rator) 
	 (conde
	   ((symbolo v-rator)
	    (fresh (v-rand)
	      (== `(,v-rator ,v-rand) o)
	      (normo rand v-rand)))
	   ((fresh (x body rec)
	      (== `(lambda (,x) ,body) v-rator)
	      (symbolo x)
	      (betao rand x body rec)
	      (normo rec o)))
	   ((fresh (v-rat-rat v-rat-ran v-rator-rec v-rand)
	      (== `(,v-rat-rat ,v-rat-ran) v-rator)
	      (== `(,v-rator-rec ,v-rand) o)
	      (normo v-rator v-rator-rec)
	      (normo rand v-rand)))))))))

(define churcho
  (lambda (c)
    (fresh (s z body)
      (symbolo* s z)
      (=/= s z)
      (== `(lambda (,s) (lambda (,z) ,body)) c)
      (c-helpo s z body))))

(define c-helpo
  (lambda (s z body)
    (conde
      ((== body z))
      ((fresh (rec)
	 (== `(,s ,rec) body)
	 (c-helpo s z rec))))))

(define church->olego
  (lambda (c o)
    (fresh (s z body)
      (== `(lambda (,s) (lambda (,z) ,body)) c)
      (symbolo* s z)
      (=/= s z)
      (c-o-helpo s z body o))))

(define c-o-helpo
  (lambda (s z body o)
    (conde
      ((== body z) (== '() o))
      ((fresh (d rec)
	 (== `(,s ,d) body)
	 (c-o-helpo s z d rec)
	 (pluso rec '(1) o))))))



;; using normo
(define joto
  (lambda (bls v o)
    (conde
      ((== '() bls) (== v o))
      ((fresh (dbls x y n-v)
	 (symbolo* x y)
	 (=/= x y)
	 (conde
	   ((== `(1 . ,dbls) bls)
	    (normo `(lambda (,x) (lambda (,y) (,v (,x ,y)))) n-v) 
            (joto dbls n-v o))
	   ((== `(0 . ,dbls) bls)
	    (fresh (z)
	      (symbolo z)
	      (=/= x z)
	      (=/= y z)
	      (normo `((,v (lambda (,x) (lambda (,y) (lambda (,z) ((,x ,z) (,y ,z))))))
	   	       (lambda (,x) (lambda (,y) ,x))) n-v)
	      (joto dbls n-v o)))))))))

(define joto-bv
  (lambda (bls v o)
    (conde
      ((== '() bls) (== v o))
      ((fresh (dbls x y)
	 (symbolo* x y)
	 (=/= x y)
	 (conde
	   ((== `(1 . ,dbls) bls)
            (joto-bv dbls `(lambda (,x) (lambda (,y) (,v (,x ,y)))) o))
	   ((== `(0 . ,dbls) bls)
	    (fresh (z n-v)
	      (symbolo z)
	      (=/= x z)
	      (=/= y z)
	      (bvo `((,v (lambda (,x) (lambda (,y) (lambda (,z) ((,x ,z) (,y ,z))))))
	   	       (lambda (,x) (lambda (,y) ,x))) n-v)
	      (joto-bv dbls n-v o)))))))))

(define joto-interface
  (lambda (bls o)
    (fresh (x)
      (symbolo x)
      (joto bls `(lambda (,x) ,x) o))))

(define joto-bv-interface
  (lambda (bls o)
    (fresh (x)
      (symbolo x)
      (joto-bv bls `(lambda (,x) ,x) o))))

(define appendo
  (lambda (ls1 ls2 o)
    (conde
      ((== '() ls1) (== ls2 o))
      ((fresh (a d rec)
	 (== `(,a . ,d) ls1)
	 (== `(,a . ,rec) o)
	 (appendo d ls2 rec))))))

(define reverseo
  (lambda (ls o)
    (conde
      ((== '() ls) (== '() o))
      ((fresh (a d rec)
	 (== `(,a . ,d) ls)
	 (appendo rec `(,a) o)
	 (reverseo d rec))))))

(load "jot-tests.scm")