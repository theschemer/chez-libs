;; Copyright 2010 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (rnrs eval)
  (srfi :2 and-let*)
  (srfi :64 testing))

(define-syntax must-be-a-syntax-error
  (syntax-rules ()
    ((_ expr)
     (test-assert
       (guard (ex (#T (syntax-violation? ex)))
         (eval 'expr (environment '(rnrs) '(srfi :2 and-let*)))
         'unexpected-return)))))

(test-begin "srfi-2-and-let*")

;; Taken from the reference implementation tests

(test-equal  (and-let* () 1) 1)
(test-equal  (and-let* () 1 2) 2)
(test-equal  (and-let* () ) #T)

(test-equal (let ((x #F)) (and-let* (x))) #F)
(test-equal (let ((x 1)) (and-let* (x))) 1)
(test-equal (and-let* ((x #F)) ) #F)
(test-equal (and-let* ((x 1)) ) 1)
(must-be-a-syntax-error (and-let* ( #F (x 1))) )
(test-equal (and-let* ( (#F) (x 1)) ) #F)
(must-be-a-syntax-error (and-let* (2 (x 1))) )
(test-equal (and-let* ( (2) (x 1)) ) 1)
(test-equal (and-let* ( (x 1) (2)) ) 2)
(test-equal (let ((x #F)) (and-let* (x) x)) #F)
(test-equal (let ((x "")) (and-let* (x) x)) "")
(test-equal (let ((x "")) (and-let* (x)  )) "")
(test-equal (let ((x 1)) (and-let* (x) (+ x 1))) 2)
(test-equal (let ((x #F)) (and-let* (x) (+ x 1))) #F)
(test-equal (let ((x 1)) (and-let* (((positive? x))) (+ x 1))) 2)
(test-equal (let ((x 1)) (and-let* (((positive? x))) )) #T)
(test-equal (let ((x 0)) (and-let* (((positive? x))) (+ x 1))) #F)
(test-equal (let ((x 1)) (and-let* (((positive? x)) (x (+ x 1))) (+ x 1)))  3)
;; Derick thinks variable shadowing should be allowed, because it's a "let*".
#;(must-be-a-syntax-error
  (let ((x 1)) (and-let* (((positive? x)) (x (+ x 1)) (x (+ x 1))) (+ x 1))))

(test-equal (let ((x 1)) (and-let* (x ((positive? x))) (+ x 1))) 2)
(test-equal (let ((x 1)) (and-let* ( ((begin x)) ((positive? x))) (+ x 1))) 2)
(test-equal (let ((x 0)) (and-let* (x ((positive? x))) (+ x 1))) #F)
(test-equal (let ((x #F)) (and-let* (x ((positive? x))) (+ x 1))) #F)
(test-equal (let ((x #F)) (and-let* ( ((begin x)) ((positive? x))) (+ x 1))) #F)

(test-equal  (let ((x 1)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) #F)
(test-equal  (let ((x 0)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) #F)
(test-equal  (let ((x #F)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) #F)
(test-equal  (let ((x 3)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) 3/2)

;; Derick's additional tests

(must-be-a-syntax-error (and-let* (("oops" 1))))
(must-be-a-syntax-error (and-let* ((x 1 2))))
(must-be-a-syntax-error (and-let* ((x 1) . oops)))
(test-equal (let ((x 1))
          (and-let* ((x (+ x 1))
                     (x (+ x 1))
                     (x (+ x 1)))
            (+ x 1)))
        5)
(test-equal (and-let* () (define x 1) (- x)) -1)
(test-equal (and-let* ((x 2) (y (+ 1 x))) (define z (* x y)) (/ z)) 1/6)

(test-end)
