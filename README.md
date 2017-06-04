# Chez-libs

Some libraries and SRFIs for Chez Scheme.  

## Rebottled

* `(rebottled pstk)` - Uses 'tclsh' to create graphical interfaces/programs
* `(rebottled pstk-plotchart)` - Requires 'tklib'

## Scheme

Some SRFI libraries named according to R7RS-large conventions.

* `(scheme list)` for `(srfi :1 lists)`
* `(scheme vector)` for `(srfi :133 vectors)`
* `(scheme sort)` for `(srfi :132 sort)`
* `(scheme set)` for `(srfi :113 sets)`
* `(scheme charset)` for `(srfi :14 char-sets)`
* `(scheme hash-table)` for `(srfi :125 hash-tables)`
* `(scheme ilist)` for `(srfi :116 ilists)`
* `(scheme rlist)` for `(srfi :101 rlists)`
* `(scheme ideque)` for `(srfi :134 ideques)`
* `(scheme text)` for `(srfi :135 texts)`
* `(scheme generator)` for `(srfi :121 generators)`
* `(scheme lseq)` for `(srfi :127 lseqs)`
* `(scheme stream)` for `(srfi :41 streams)`
* `(scheme box)` for `(srfi :111 boxes)`
* `(scheme list-queue)` for `(srfi :117 list-queues)`
* `(scheme ephemeron)` for `(srfi :124 ephemerons)`
* `(scheme comparator)` for `(srfi :128 comparators)`

## SRFIs

The listed clashes are functions with different semantics in R6RS and the SRFI;
"Chez clashes" highlights clashes with chezscheme functions.  When importing,
either exclude from rnrs/chezscheme or from the library, depending on which
interpretation you prefer.

* `(srfi :1 lists)`
  * clashes: fold-right for-each map remove 
* `(srfi :2 and-let*)`
* `(srfi :8 receive)`
* `(srfi :14 char-sets)`
* `(srfi :19 time)`
* `(srfi :26 cut)`
* `(srfi :31 rec)`
* `(srfi :41 streams)`
* `(srfi :42 eager-comprehensions)`
* `(srfi :63 arrays)`
* `(srfi :64 testing)`  
* `(srfi :101 rlists)`
* `(srfi :111 boxes)`
* `(srfi :113 sets)`
  * Follows "post-finalization note no. 2" so -map and -unfold consistent with srfi 125
* `(srfi :116 ilists)`
* `(srfi :117 list-queues)`
* `(srfi :121 generators)`
* `(srfi :124 ephemerons)`
  * Needs some Chez-specific code to be useful
* `(srfi :125 hash-tables)`
  * Chez clashes: hash-table-for-each hash-table-map (from compatibility layer)
* `(srfi :127 lseqs)`
* `(srfi :128 comparators)`
* `(srfi :132 sort)`
  * clashes: vector-sort vector-sort!
* `(srfi :133 vectors)`
  * clashes: vector->list vector-copy vector-fill!
* `(srfi :134 ideques)`
* `(srfi :135 texts)`
* `(srfi :141 integer-division)`
* `(srfi :143 fixnums)`
  * clashes: fxbit-count fxbit-set? fxcopy-bit from `(rnrs arithmetic fixnum)`
* `(srfi :151 bitwise)`

