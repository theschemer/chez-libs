;; Implementation of hash-table by Peter Lane, 2017
;; -- attempts to build directly on (srfi 69), which is well supported by implementations
;; -- ignores deprecated functions and forbids implementation-specific optional args
;; -- hashtables assumed to be mutable
;; -- as with other implementations of this library, hash-table-set! does not check the 
;;    type of key against the original comparator
;; Both these last could be done with an internal record to hold the hash-table, comparator
;; and mutable flag

;; Parts marked Reference Implementation:
;;; Copyright 2015 William D Clinger.
;;;
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright and permission notice in full.
;;;
;;; I also request that you send me a copy of any improvements that you
;;; make to this software so that they may be incorporated within it to
;;; the benefit of the Scheme community.
;;;

(library
  (scheme hash-table)
  (export 
    make-hash-table
    hash-table
    hash-table-unfold
    alist->hash-table 

    hash-table?
    hash-table-contains?
    hash-table-empty?
    hash-table=?
    hash-table-mutable? 

    hash-table-ref
    hash-table-ref/default 

    hash-table-set!
    hash-table-delete!
    hash-table-intern!
    hash-table-update!
    hash-table-update!/default
    hash-table-pop!
    hash-table-clear! 

    hash-table-size
    hash-table-keys
    hash-table-values
    hash-table-entries
    hash-table-find
    hash-table-count

    hash-table-map
    hash-table-for-each
    hash-table-map!
    hash-table-map->list
    hash-table-fold
    hash-table-prune!

    hash-table-copy
    hash-table-empty-copy
    hash-table->alist 

    hash-table-union!
    hash-table-intersection!
    hash-table-difference!
    hash-table-xor!
    )
  (import (srfi :125 hash-tables)))
