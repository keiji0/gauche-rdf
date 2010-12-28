(define-module rdf.display
  (use util.list)
  (use rdf.store)
  (use rdf.namespace)
  (export rdf-triple-display
          rdf-store-display)
  )

(select-module rdf.display)

(define (rdf-triple-display triples)
  (rdf-store-display (make-rdf-store triples)))
 
(define (rdf-store-display store . out)
  (let ((out (get-optional out (current-output-port)))
        (sindex (rdf-store-by-subject store)))
    (hash-table-for-each sindex
                         (lambda (key triples)
                           (format out "~s\n" (uri->qname key))
                           (dolist (triple triples)
                             (format out "\t~s => ~a\n"
                                     (uri->qname (rdf-triple-predicate triple))
                                     (rdf-triple-object-format
                                      (rdf-triple-object triple))))))))

(define (rdf-triple-object-format obj)
  (cond ((rdf-uri-ref? obj)
         (uri->qname obj))
        ((rdf-literal? obj)
         (apply string-append
                "\"" (x->string (ref obj 'content)) "\""
                (cond-list ((ref obj 'type)
                            => (pa$ string-append "^^"))
                           ((ref obj 'lang)
                            => (pa$ string-append "@")))))
        (else obj)))

(provide "rdf/display")
