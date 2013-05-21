;;tests for a single expected answer
;;usage: (test "factorial-test" (fact 5) 120))
(define-syntax test
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (let* ((expected expected-result)
            (produced tested-expression))
       (if (equal? expected produced)
           (printf "~s works!\n" title)
           (errorf
            'test
            "Failed ~s: ~a\nExpected: ~a\nComputed: ~a"
            title 'tested-expression expected produced))))))

;;tests for one of many possible answers
;;usage: (multi-test "random-test" (random 2) 0 1 2))
(define-syntax multi-test
  (syntax-rules ()
    ((_ title tested-expression expected-result* ...)
     (let* ((expected* expected-result*)
            ...
            (produced tested-expression))
       (cond
         [(equal? produced expected-result*) (printf "~s works!\n" title)]
         ...
         [else (errorf
                 'test
                 "Failed ~s: ~a\nComputed: ~a"
                 title 'tested-expression produced)])))))

;; Same usage as test; does not stop execution for errors or infinite
;; loops
(define-syntax test/cc
  (syntax-rules ()
    [(_ title tested-expression expected-result)
     (call/cc
      (lambda (k)
        (begin
          (error-handler
           (lambda (who msg . args)
             (fprintf (console-output-port)
                      "Error during test ~s~a: ~a.~%" title
                      (if who (format " in ~s" who) "")
                      (parameterize ([print-level 3] [print-length 6])
                        (apply format msg args)))
             (k (void))))
          (let* ([expected expected-result]
                 [eng (make-engine (lambda () tested-expression))]
                 [produced (eng 100000
                                (lambda (f v) v)
                                (lambda (e)
                                  (printf "Failed ~s: ran out of ticks\n" title)
                                  (k (void))))])
            (cond
              [(equal? produced expected) (printf "~s works!\n" title)]
              [else (printf "Failed ~s: ~a\nExpected: ~a\nComputed: ~a\n"
                            title 'tested-expression expected produced)])))))]))

;;same usage as multi-test; does not stop execution for errors or
;;infinite loops
(define-syntax multi-test/cc
  (syntax-rules ()
    [(_ title tested-expression expected-result* ...)
     (call/cc
      (lambda (k)
        (begin
          (error-handler
           (lambda (who msg . args)
             (fprintf (console-output-port)
                      "Error during test ~s~a: ~a.~%" title
                      (if who (format " in ~s" who) "")
                      (parameterize ([print-level 3] [print-length 6])
                        (apply format msg args)))
             (k (void))))
          (let* ([expected* expected-result*]
                 ...
                 [eng (make-engine (lambda () tested-expression))]
                 [produced (eng 100000
                                (lambda (f v) v)
                                (lambda (e)
                                  (printf "Failed ~s: ran out of ticks\n" title)
                                  (k (void))))])
            (cond
              [(equal? produced expected-result*) (printf "~s works!\n" title)]
              ...
              [else (printf "Failed ~s: ~a\nComputed: ~a"
                            title 'tested-expression produced)])))))]))