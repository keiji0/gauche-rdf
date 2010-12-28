(define-module rdf.util
  (export make-rdf-bnodeID))

(select-module rdf.util)

(define make-rdf-bnodeID
  (let1 id 0
    (lambda option
      (string->symbol
       (string-append "_:"
                      (x->string (get-optional option ""))
                      (begin (inc! id)
                             (x->string id)))))))

(provide "rdf/util")
