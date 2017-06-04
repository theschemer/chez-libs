;; Reference implementation for srfi 111

(library 
  (srfi :111 boxes)
  (export box box? unbox set-box!)
  (import (rnrs))

  (define-record-type box-type
                      (box value)
                      box?
                      (value unbox set-box!)))
