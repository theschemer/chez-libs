;; Reference implementation

(library
  (srfi :31 rec)
  (export rec)
  (import (rnrs))

  (define-syntax rec
    (syntax-rules ()
                  ((rec (NAME . VARIABLES) . BODY)
                   (letrec ( (NAME (lambda VARIABLES . BODY)) ) NAME))
                  ((rec NAME EXPRESSION)
                   (letrec ( (NAME EXPRESSION) ) NAME)))))


