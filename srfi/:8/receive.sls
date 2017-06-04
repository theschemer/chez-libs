;;; SRFI 8 syntax for receiving multiple values

(library
  (srfi :8 receive)
  (export receive)
  (import (rnrs))

  (define-syntax receive
    (syntax-rules ()
                  ((receive formals expression body ...)
                   (call-with-values (lambda () expression)
                                     (lambda formals body ...))))))

