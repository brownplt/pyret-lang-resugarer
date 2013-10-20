#lang whalesong

(require
  (except-in whalesong/lang/whalesong raise pi else gensym
             ; why can't we say (struct-in srcloc)?
             srcloc srcloc? srcloc-line srcloc-column srcloc-span srcloc-source srcloc-position make-srcloc)
  "runtime.rkt"
  )

(provide
  #%module-begin
  #%top-interaction
  #%datum
  #%top
  #%app

  [prefix-out r: (all-from-out whalesong/lang/whalesong)]
  (all-from-out "runtime.rkt")
  )
  
