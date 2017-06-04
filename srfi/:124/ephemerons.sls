;; Trivial implementation of Ephemerons
;; -- needs replacing with implementation-specific version

(library
  (srfi :124 ephemerons)
  (export make-ephemeron
          ephemeron?
          ephemeron-broken?
          ephemeron-key
          ephemeron-datum
          reference-barrier)
  (import (rnrs base))

    (define-record-type (<ephemeron> %make-ephemeron ephemeron?)
                        (fields
                          (immutable key ephemeron-key)
                          (immutable datum ephemeron-datum)
                          (immutable broken? ephemeron-broken?)))

    (define (make-ephemeron key datum) (%make-ephemeron key datum #f))

    (define (reference-barrier key) #t)

    )

