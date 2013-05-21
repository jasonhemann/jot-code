(load "jot.scm")

(define decimal->lob
  (lambda (n)
    (cond
      [(< n 2) `(,n)]
      [else (let-values (((d m) (div-and-mod n 2)))
              (append (decimal->lob d) `(,m)))])))

(define bit-not
  (lambda (b) (bitwise-xor b 1)))

(define add1b
  (lambda (bn)
    (letrec
        ((add1b-h
          (lambda (bn)
            (cond
              [(null? (cdr bn)) (let ((b (car bn)))
                                  (values b `(,(bit-not b))))]
              [else (let-values [((c rbn) (add1b-h (cdr bn)))]
                      (let ((b (car bn)))                      
                        (cond
                          [(zero? b) (let ((b^ (bitwise-xor c b)))
                                       (values 0 (cons b^ rbn)))]
                          [else (values c (cons (bit-not c) rbn))])))]))))
      (let-values [((c bn^) (add1b-h bn))]
        (cond
          [(zero? c) bn^]
          [else (cons 1 bn^)])))))

(define write-jot
  (lambda (file)
    (let ((port (open-output-file file)))
      (letrec ((write-jot-h
                (lambda (n)
                  (cond
                    ((equal? '(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) n) (close-port port)) ; 1024^2 + 1
                    (else 
                     (let ((betas (cdr (jot-bv-interface n))))
                       (fprintf port "~s~n" betas)
                       (write-jot-h (add1b n))))))))
        (write-jot-h (decimal->lob 0))))))

(write-jot "bv-out-file.txt")