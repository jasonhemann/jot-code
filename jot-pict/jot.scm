(load "pmatch.scm")
(load "test.scm")

(define MAX_BETA (sub1 (expt 2 (* 3 8))))

(print-gensym #f)

;; State monad
(define return-state
  (lambda (a)
    (lambda (s)
      `(,a . ,s))))
 
(define bind-state
  (lambda (ma f)
    (lambda (s)
      (let ([vs^ (ma s)])
        (let ([v (car vs^)]
              [s^ (cdr vs^)])
          ((f v) s^))))))
 
(define get-state
  (lambda (s) `(,s . ,s)))
 
(define put-state
  (lambda (new-s)
    (lambda (s)
      `(_ . ,new-s))))

(define-syntax do
  (syntax-rules (<-)
    ((_ bind e) e)
    ((_ bind (v <- e) e* e** ...)
     (bind e (lambda (v) (do bind e* e** ...))))
    ((_ bind e e* e** ...)
     (bind e (lambda (_) (do bind e* e** ...))))))

;; pure helpers
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

;; monadic reducers
(define beta
  (lambda (M x e)
    (pmatch e
      (,e (guard (symbol? e) (eq? e x)) (return-state M))
      (,e (guard (symbol? e) (not (eq? e x))) (return-state e))
      ((lambda (,y) ,body) (guard (eq? x y)) (return-state `(lambda (,y) ,body)))
      ((lambda (,y) ,body) (guard (not (eq? x y)) (not-free? x body))
       (do bind-state
           (s <- get-state)
         (if (< s MAX_BETA)
             (do bind-state
               (put-state (add1 s))
               (rec <- (beta M x body))
               (return-state `(lambda (,y) ,rec)))
             (do bind-state
               (put-state MAX_BETA)
               (return-state '_)))))
      ((lambda (,y) ,body) (guard (not (eq? x y)) (free? x body) (not-free? y M))
       (do bind-state
           (s <- get-state)
           (if (< s MAX_BETA)
             (do bind-state
               (put-state (add1 s))
               (rec <- (beta M x body))
               (return-state `(lambda (,y) ,rec)))
             (do bind-state
               (put-state MAX_BETA)
               (return-state '_)))))
      ((lambda (,y) ,body) (guard (not (eq? x y)) (free? x body) (free? y M))
       (let ((g (gensym))) ;; g =/= x
                           ;; g \not\in fv e
                           ;; g \not\in fv M
         (do bind-state
           (s1 <- get-state)
           (if (< s1 MAX_BETA)
               (do bind-state
                 (put-state (add1 s1))
                 (rec1 <- (beta g y body))
                 (s2 <- get-state)
                 (if (< s2 MAX_BETA)
                     (do bind-state                       
                       (put-state (add1 s2))
                       (rec2 <- (beta M x rec1))
                       (return-state `(lambda (,g) ,rec2)))
                     (do bind-state
                       (put-state MAX_BETA)
                       (return-state '_))))
               (do bind-state
                 (put-state MAX_BETA)
                 (return-state '_))))))
      ((,rator ,rand)
       (do bind-state
         (s1 <- get-state)
         (if (< s1 MAX_BETA)
             (do bind-state                 
               (put-state (add1 s1))
               (brat <- (beta M x rator))
               (s2 <- get-state)
               (if (< s2 MAX_BETA)
                   (do bind-state                       
                     (put-state (add1 s2))
                     (brand <- (beta M x rand))
                     (return-state `(,brat ,brand)))
                   (do bind-state
                     (put-state MAX_BETA)
                     (return-state '_))))
             (do bind-state
               (put-state MAX_BETA)
               (return-state '_))))))))

(define bv-wnf
  (lambda (exp)
    (pmatch exp
      [,exp (guard (symbol? exp)) (return-state exp)]
      [(lambda (,x) ,body) (guard (symbol? x)) (return-state exp)]
      [(,rator ,rand)
       (do bind-state
         (v-rator <- (bv-wnf rator))
         (pmatch v-rator
           [,v-rator (guard (symbol? v-rator))
             (do bind-state
               (v-rand <- (bv-wnf rand))
               (return-state `(,v-rator ,v-rand)))]
           [(lambda (,x) ,body) (guard (symbol? x))
            (do bind-state
              (v-rand <- (bv-wnf rand))
              (s <- get-state)
              (if (< s MAX_BETA)
                  (do bind-state                      
                    (put-state (add1 s))
                    (rec <- (beta v-rand x body))
                    (bv-wnf rec))
                  (do bind-state
                    (put-state MAX_BETA)
                    (return-state '_))))]
           [(,v-rat-rat ,v-rat-ran)
            (do bind-state
                (v-rand <- (bv-wnf rand))
                (return-state `(,v-rator ,v-rand)))]))])))

(define ao-nf
  (lambda (exp)
    (pmatch exp
      [,exp (guard (symbol? exp)) (return-state exp)]
      [(lambda (,x) ,body) (guard (symbol? x))
       (do bind-state
         (v-body <- (ao-nf body))
         (return-state `(lambda (,x) ,v-body)))]
      [(,rator ,rand)
       (do bind-state
         (v-rator <- (ao-nf rator))
         (pmatch v-rator
           [,v-rator (guard (symbol? v-rator))
             (do bind-state
               (v-rand <- (ao-nf rand))
               (return-state `(,v-rator ,v-rand)))]
           [(lambda (,x) ,body) (guard (symbol? x))
            (do bind-state
              (v-rand <- (ao-nf rand))
              (s <- get-state)
              (if (< s MAX_BETA)
                  (do bind-state                      
                    (put-state (add1 s))
                    (rec <- (beta v-rand x body))
                    (ao-nf rec))
                  (do bind-state
                    (put-state MAX_BETA)
                    (return-state '_))))]
           [(,v-rat-rat ,v-rat-ran)
            (do bind-state
                (v-rand <- (ao-nf rand))
                (return-state `(,v-rator ,v-rand)))]))])))

(define he-hnf
  (lambda (exp)
    (pmatch exp
      [,exp (guard (symbol? exp)) (return-state exp)]
      [(lambda (,x) ,body) (guard (symbol? x))
       (do bind-state
         (v-body <- (he-hnf body))
         (return-state `(lambda (,x) ,v-body)))]
      [(,rator ,rand)
       (do bind-state
         (v-rator <- (he-hnf rator))
         (pmatch v-rator
           [,v-rator (guard (symbol? v-rator))
            (return-state `(,v-rator ,rand))]
           [(lambda (,x) ,body) (guard (symbol? x))
            (do bind-state
              (s <- get-state)
              (if (< s MAX_BETA)
                  (do bind-state
                    (put-state (add1 s))
                    (rec <- (beta rand x body))
                    (he-hnf rec))
                  (do bind-state
                    (put-state MAX_BETA)
                    (return-state '_))))]
           [(,rat ,ran)
            (return-state `(,v-rator ,rand))]))])))

(define jot-bv-wnf
  (lambda (bls v)
    (pmatch bls
      (() (return-state v))
      ((1 . ,dbls)
       (jot-bv-wnf dbls `(lambda (x) (lambda (y) (,v (x y))))))
      ((0 . ,dbls)
       (do bind-state
           (n-v <- (bv-wnf `((,v (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
                         (lambda (x) (lambda (y) x)))))
         (jot-bv-wnf dbls n-v))))))

;; bv to wnf
(define jot-bv-wnf-interface
  (lambda (bls)
    ((jot-bv-wnf bls '(lambda (x) x)) 0)))

(define bn-whnf
  (lambda (exp)
    (pmatch exp
      [,exp (guard (symbol? exp)) (return-state exp)]
      [(lambda (,x) ,body) (guard (symbol? x)) (return-state exp)]
      [(,rator ,rand)
       (do bind-state
         (v-rator <- (bn-whnf rator))
         (pmatch v-rator
           [,v-rator (guard (symbol? v-rator))
	    (return-state `(,v-rator ,rand))]
	   [(lambda (,x) ,body) (guard (symbol? x))
            (do bind-state
              (s <- get-state)
              (if (< s MAX_BETA)
                  (do bind-state
                    (put-state (add1 s))
                    (rec <- (beta rand x body))
                    (bn-whnf rec))
                  (do bind-state
                    (put-state MAX_BETA)
                    (return-state '_))))]
	   [(,v-rat-rat ,v-rat-ran) (return-state `(,v-rator ,rand))]))])))

(define no-nf
  (lambda (exp)
    (pmatch exp
      [,exp (guard (symbol? exp)) (return-state exp)]
      [(lambda (,x) ,body) (guard (symbol? x))
       (do bind-state
         (nbody <- (no-nf body))
         (return-state `(lambda (,x) ,nbody)))]
      [(,rator ,rand)
       (do bind-state
         (v-rator <- (bn-whnf rator))
         (pmatch v-rator
	   [,v-rator (guard (symbol? v-rator))
                     (do bind-state
                       (nrand <- (no-nf rand))
                       (return-state `(,v-rator ,nrand)))]
	   [(lambda (,x) ,body) (guard (symbol? x))
            (do bind-state
              (s <- get-state)
              (if (< s MAX_BETA)
                  (do bind-state
                    (put-state (add1 s))
                    (rec <- (beta rand x body))
                    (no-nf rec))
                  (do bind-state
                    (put-state MAX_BETA)
                    (return-state '_))))]
	   [(,v-rat-rat ,v-rat-ran)
            (do bind-state
              (nrat <- (no-nf v-rator))
              (nrand <- (no-nf rand))
              (return-state `(,nrat ,nrand)))]))])))

(define ha-nf
  (lambda (exp)
    (pmatch exp
      [,exp (guard (symbol? exp)) (return-state exp)]
      [(lambda (,x) ,body) (guard (symbol? x))
       (do bind-state
         (nbody <- (ha-nf body))
         (return-state `(lambda (,x) ,nbody)))]
      [(,rator ,rand)
       (do bind-state
         (v-rator <- (bv-wnf rator))
         (pmatch v-rator
	   [,v-rator (guard (symbol? v-rator))
            (do bind-state  
              (nrand <- (ha-nf rand))                       
              (return-state `(,v-rator ,nrand)))]
	   [(lambda (,x) ,body) (guard (symbol? x))
            (do bind-state
              (nrand <- (ha-nf rand))  
              (s <- get-state)
              (if (< s MAX_BETA)
                  (do bind-state
                    (put-state (add1 s))
                    (rec <- (beta nrand x body))
                    (ha-nf rec))
                  (do bind-state
                    (put-state MAX_BETA)
                    (return-state '_))))]
	   [(,v-rat-rat ,v-rat-ran)
            (do bind-state
              (nrat <- (ha-nf v-rator))
              (nrand <- (ha-nf rand))
              (return-state `(,nrat ,nrand)))]))])))

(define hn-nf
  (lambda (exp)
    (pmatch exp
      [,exp (guard (symbol? exp)) (return-state exp)]
      [(lambda (,x) ,body) (guard (symbol? x))
       (do bind-state
         (nbody <- (hn-nf body))
         (return-state `(lambda (,x) ,nbody)))]
      [(,rator ,rand)
       (do bind-state
         (v-rator <- (he-hnf rator))
         (pmatch v-rator
	   [,v-rator (guard (symbol? v-rator))
                     (do bind-state
                       (nrand <- (hn-nf rand))
                       (return-state `(,v-rator ,nrand)))]
	   [(lambda (,x) ,body) (guard (symbol? x))
            (do bind-state
              (s <- get-state)
              (if (< s MAX_BETA)
                  (do bind-state
                    (put-state (add1 s))
                    (rec <- (beta rand x body))
                    (hn-nf rec))
                  (do bind-state
                    (put-state MAX_BETA)
                    (return-state '_))))]
	   [(,v-rat-rat ,v-rat-ran)
            (do bind-state
              (nrat <- (hn-nf v-rator))
              (nrand <- (hn-nf rand))
              (return-state `(,nrat ,nrand)))]))])))

;; no to nf
(define jot-no-nf
  (lambda (bls v)
    (pmatch bls
      (() (return-state v))
      ((1 . ,dbls) (do bind-state
                     (n-v <- (no-nf `(lambda (x) (lambda (y) (,v (x y))))))
                     (jot-no-nf dbls n-v)))
      ((0 . ,dbls) (do bind-state
                     (n-v <- (no-nf `((,v (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
                                       (lambda (x) (lambda (y) x)))))
		     (jot-no-nf dbls n-v))))))

(define jot-no-nf-interface
  (lambda (bls)
    ((jot-no-nf bls '(lambda (x) x)) 0)))

;; no to whnf
(define jot-bn-whnf
  (lambda (bls v)
    (pmatch bls
      (() (return-state v))
      ((1 . ,dbls) (jot-bn-whnf dbls `(lambda (x) (lambda (y) (,v (x y))))))
      ((0 . ,dbls) (do bind-state
                       (n-v <- (bn-whnf `((,v (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
                                     (lambda (x) (lambda (y) x)))))
                       (jot-bn-whnf dbls n-v))))))

(define jot-bn-whnf-interface
  (lambda (bls)
    ((jot-bn-whnf bls '(lambda (x) x)) 0)))

;; head spine to head normal form
(define jot-he-hnf
  (lambda (bls v)
    (pmatch bls
      (() (return-state v))
      ((1 . ,dbls) (jot-he-hnf dbls `(lambda (x) (lambda (y) (,v (x y))))))
      ((0 . ,dbls) (do bind-state
                       (n-v <- (he-hnf `((,v (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
                                     (lambda (x) (lambda (y) x)))))
                       (jot-he-hnf dbls n-v))))))

;; head spine to head normal form
(define jot-he-hnf-interface
  (lambda (bls)
    ((jot-he-hnf bls '(lambda (x) x)) 0)))

;; applicative order reduction to normal form
(define jot-ao-nf
  (lambda (bls v)
    (pmatch bls
      (() (return-state v))
      ((1 . ,dbls) (do bind-state
                     (n-v <- (ao-nf `(lambda (x) (lambda (y) (,v (x y))))))
                     (jot-ao-nf dbls n-v)))
      ((0 . ,dbls) (do bind-state
                       (n-v <- (ao-nf `((,v (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
                                     (lambda (x) (lambda (y) x)))))
                       (jot-ao-nf dbls n-v))))))

;; applicative order reduction to normal form 
(define jot-ao-nf-interface
  (lambda (bls)
    ((jot-ao-nf bls '(lambda (x) x)) 0)))

;; hybrid applicative order reduction to normal form
(define jot-ha-nf
  (lambda (bls v)
    (pmatch bls
      (() (return-state v))
      ((1 . ,dbls) (do bind-state
                     (n-v <- (ha-nf `(lambda (x) (lambda (y) (,v (x y))))))
                     (jot-ha-nf dbls n-v)))
      ((0 . ,dbls) (do bind-state
                       (n-v <- (ha-nf `((,v (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
                                     (lambda (x) (lambda (y) x)))))
                       (jot-ha-nf dbls n-v))))))

;; hybrid applicative order reduction to normal form 
(define jot-ha-nf-interface
  (lambda (bls)
    ((jot-ha-nf bls '(lambda (x) x)) 0)))

;; hybrid normal order reduction to normal form
(define jot-hn-nf
  (lambda (bls v)
    (pmatch bls
      (() (return-state v))
      ((1 . ,dbls) (do bind-state
                     (n-v <- (hn-nf `(lambda (x) (lambda (y) (,v (x y))))))
                     (jot-hn-nf dbls n-v)))
      ((0 . ,dbls) (do bind-state
                       (n-v <- (hn-nf `((,v (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
                                     (lambda (x) (lambda (y) x)))))
                       (jot-hn-nf dbls n-v))))))

;; hybrid normal order reduction to normal form 
(define jot-hn-nf-interface
  (lambda (bls)
    ((jot-hn-nf bls '(lambda (x) x)) 0)))

(load "jot-tests.scm")


