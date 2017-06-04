;; Trivial implementation of Ephemerons
;; -- needs replacing with implementation-specific version

(library
  (scheme ephemeron)
  (export make-ephemeron
          ephemeron?
          ephemeron-broken?
          ephemeron-key
          ephemeron-datum
          reference-barrier)
  (import (srfi :124 ephemerons)))

