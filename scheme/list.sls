;;; SRFI-1 list-processing library 			-*- Scheme -*-
;;; Reference implementation
;;;
;;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;;; this code as long as you do not remove this copyright notice or
;;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;;     -Olin

;; Packaged for R7RS as (scheme list) by Peter Lane, 2017
;; And adapted to R6RS format for Chez Scheme.

(library
  (scheme list)
  (export 
    fold-right for-each map remove ; as different semantics from R6RS
    xcons make-list list-tabulate list-copy
    proper-list? circular-list? dotted-list? not-pair? null-list? list=
    circular-list length+
    iota
    first second third fourth fifth sixth seventh eighth ninth tenth
    car+cdr
    take       drop       
    take-right drop-right 
    take!      drop-right!
    split-at   split-at!
    last last-pair
    zip unzip1 unzip2 unzip3 unzip4 unzip5
    count
    append! append-reverse append-reverse! concatenate concatenate! 
    unfold       fold       pair-fold       reduce
    unfold-right pair-fold-right reduce-right
    append-map append-map! map! pair-for-each filter-map map-in-order
    filter! partition! remove! 
    find-tail any every list-index
    take-while drop-while take-while!
    span break span! break!
    delete delete!
    alist-cons alist-copy
    delete-duplicates delete-duplicates!
    alist-delete alist-delete!
    reverse! 
    lset<= lset= lset-adjoin  
    lset-union  lset-intersection  lset-difference  lset-xor  lset-diff+intersection
    lset-union! lset-intersection! lset-difference! lset-xor! lset-diff+intersection!
    )
  (import (srfi :1 lists)))

