
(library 
  (scheme vector)
  (export 
    ;; Constructors 
    vector-copy vector-unfold vector-unfold-right vector-reverse-copy 
    vector-concatenate vector-append vector-append-subvectors
    ;; Predicates 
    vector-empty? vector=
    ;; Iteration 
    vector-fold vector-fold-right vector-map!
    vector-count vector-cumulate
    ;; Searching 
    vector-index vector-index-right vector-skip vector-skip-right 
    vector-binary-search vector-any vector-every vector-partition
    ;; Mutators 
    vector-fill! vector-swap! vector-reverse! 
    vector-copy! vector-reverse-copy! vector-unfold! vector-unfold-right!
    ;; Conversion 
    vector->list vector->string string->vector reverse-vector->list reverse-list->vector)
  (import (srfi :133 vectors)))
    
