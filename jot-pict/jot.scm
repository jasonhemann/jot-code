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

(define bv
  (lambda (exp)
    (pmatch exp
      [,exp (guard (symbol? exp)) (return-state exp)]
      [(lambda (,x) ,body) (guard (symbol? x)) (return-state exp)]
      [(,rator ,rand)
       (do bind-state
         (v-rator <- (bv rator))
         (pmatch v-rator
           [,v-rator (guard (symbol? v-rator))
             (do bind-state
               (v-rand <- (bv rand))
               (return-state `(,v-rator ,v-rand)))]
           [(lambda (,x) ,body) (guard (symbol? x))
            (do bind-state
              (v-rand <- (bv rand))
              (s <- get-state)
              (if (< s MAX_BETA)
                  (do bind-state                      
                    (put-state (add1 s))
                    (rec <- (beta v-rand x body))
                    (bv rec))
                  (do bind-state
                    (put-state MAX_BETA)
                    (return-state '_))))]
           [(,v-rat-rat ,v-rat-ran)
            (do bind-state
                (v-rand <- (bv rand))
                (return-state `(,v-rator ,v-rand)))]))])))

(define ao
  (lambda (exp)
    (pmatch exp
      [,exp (guard (symbol? exp)) (return-state exp)]
      [(lambda (,x) ,body) (guard (symbol? x))
       (do bind-state
         (v-body <- (ao body))
         (return-state `(lambda (,x) ,v-body)))]
      [(,rator ,rand)
       (do bind-state
         (v-rator <- (ao rator))
         (pmatch v-rator
           [,v-rator (guard (symbol? v-rator))
             (do bind-state
               (v-rand <- (ao rand))
               (return-state `(,v-rator ,v-rand)))]
           [(lambda (,x) ,body) (guard (symbol? x))
            (do bind-state
              (v-rand <- (ao rand))
              (s <- get-state)
              (if (< s MAX_BETA)
                  (do bind-state                      
                    (put-state (add1 s))
                    (rec <- (beta v-rand x body))
                    (ao rec))
                  (do bind-state
                    (put-state MAX_BETA)
                    (return-state '_))))]
           [(,v-rat-rat ,v-rat-ran)
            (do bind-state
                (v-rand <- (ao rand))
                (return-state `(,v-rator ,v-rand)))]))])))

(define he
  (lambda (exp)
    (pmatch exp
      [,exp (guard (symbol? exp)) (return-state exp)]
      [(lambda (,x) ,body) (guard (symbol? x))
       (do bind-state
         (v-body <- (he body))
         (return-state `(lambda (,x) ,v-body)))]
      [(,rator ,rand)
       (do bind-state
         (v-rator <- (he rator))
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
                    (he rec))
                  (do bind-state
                    (put-state MAX_BETA)
                    (return-state '_))))]
           [(,rat ,ran)
            (return-state `(,v-rator ,rand))]))])))

(define jot-bv
  (lambda (bls v)
    (pmatch bls
      (() (return-state v))
      ((1 . ,dbls) (jot-bv dbls `(lambda (x) (lambda (y) (,v (x y))))))
      ((0 . ,dbls) (do bind-state
                       (n-v <- (bv `((,v (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
                                     (lambda (x) (lambda (y) x)))))
                       (jot-bv dbls n-v))))))

;; bv to wnf
(define jot-bv-interface
  (lambda (bls)
    ((jot-bv bls '(lambda (x) x)) 0)))

(define bn
  (lambda (exp)
    (pmatch exp
      [,exp (guard (symbol? exp)) (return-state exp)]
      [(lambda (,x) ,body) (guard (symbol? x)) (return-state exp)]
      [(,rator ,rand)
       (do bind-state
         (v-rator <- (bn rator))
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
                    (bn rec))
                  (do bind-state
                    (put-state MAX_BETA)
                    (return-state '_))))]
	   [(,v-rat-rat ,v-rat-ran) (return-state `(,v-rator ,rand))]))])))

(define norm
  (lambda (exp)
    (pmatch exp
      [,exp (guard (symbol? exp)) (return-state exp)]
      [(lambda (,x) ,body) (guard (symbol? x))
       (do bind-state
         (nbody <- (norm body))
         (return-state `(lambda (,x) ,nbody)))]
      [(,rator ,rand)
       (do bind-state
         (v-rator <- (bn rator))
         (pmatch v-rator
	   [,v-rator (guard (symbol? v-rator))
                     (do bind-state
                       (nrand <- (norm rand))
                       (return-state `(,v-rator ,nrand)))]
	   [(lambda (,x) ,body) (guard (symbol? x))
            (do bind-state
              (s <- get-state)
              (if (< s MAX_BETA)
                  (do bind-state
                    (put-state (add1 s))
                    (rec <- (beta rand x body))
                    (norm rec))
                  (do bind-state
                    (put-state MAX_BETA)
                    (return-state '_))))]
	   [(,v-rat-rat ,v-rat-ran)
            (do bind-state
              (nrat <- (norm v-rator))
              (nrand <- (norm rand))
              (return-state `(,nrat ,nrand)))]))])))

;; no to nf
(define jot
  (lambda (bls v)
    (pmatch bls
      (() (return-state v))
      ((1 . ,dbls) (do bind-state
                     (n-v <- (norm `(lambda (x) (lambda (y) (,v (x y))))))
                     (jot dbls n-v)))
      ((0 . ,dbls) (do bind-state
                     (n-v <- (norm `((,v (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
                                       (lambda (x) (lambda (y) x)))))
		     (jot dbls n-v))))))

(define jot-interface
  (lambda (bls)
    ((jot bls '(lambda (x) x)) 0)))

;; no to whnf
(define jot-bn
  (lambda (bls v)
    (pmatch bls
      (() (return-state v))
      ((1 . ,dbls) (jot-bn dbls `(lambda (x) (lambda (y) (,v (x y))))))
      ((0 . ,dbls) (do bind-state
                       (n-v <- (bn `((,v (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
                                     (lambda (x) (lambda (y) x)))))
                       (jot-bn dbls n-v))))))

(define jot-bn-interface
  (lambda (bls)
    ((jot-bn bls '(lambda (x) x)) 0)))

;; head spine to head normal form
(define jot-he
  (lambda (bls v)
    (pmatch bls
      (() (return-state v))
      ((1 . ,dbls) (jot-he dbls `(lambda (x) (lambda (y) (,v (x y))))))
      ((0 . ,dbls) (do bind-state
                       (n-v <- (he `((,v (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
                                     (lambda (x) (lambda (y) x)))))
                       (jot-he dbls n-v))))))

;; head spine to head normal form
(define jot-he-interface
  (lambda (bls)
    ((jot-he bls '(lambda (x) x)) 0)))

;; applicative order reduction to normal form
(define jot-ao
  (lambda (bls v)
    (pmatch bls
      (() (return-state v))
      ((1 . ,dbls) (do bind-state
                     (n-v <- (ao `(lambda (x) (lambda (y) (,v (x y))))))
                     (jot-ao dbls n-v)))
      ((0 . ,dbls) (do bind-state
                       (n-v <- (ao `((,v (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
                                     (lambda (x) (lambda (y) x)))))
                       (jot-ao dbls n-v))))))

;; applicative order reduction to normal form 
(define jot-ao-interface
  (lambda (bls)
    ((jot-ao bls '(lambda (x) x)) 0)))


(load "jot-tests.scm")

