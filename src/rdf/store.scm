(define-module rdf.store
  (use srfi-1)
  (use srfi-13)
  (use util.list)
  (export rdf-uri-ref?
          rdf-literal?
          rdf-bnodeID?

          make-rdf-triple
          rdf-triple-subject
          rdf-triple-predicate
          rdf-triple-object
          with-rdf-triple

          <rdf-literal>
          make-rdf-literal

          <rdf-store>
          make-rdf-store
          rdf-store-push!
          rdf-store-delete!
	  rdf-store-merge!

          rdf-store-by-subject
          rdf-store-by-predicate
          rdf-store-by-object
          rdf-store-triples

          rdf-triple-match
          rdf-triple-match-delete!
          )
  )

(select-module rdf.store)

(define (rdf-uri-ref? node) (symbol? node))

(define (rdf-literal? obj)
  (is-a? obj <rdf-literal>))

(define (rdf-bnodeID? node)
  (and (rdf-uri-ref? node)
       (string-prefix? "_" (symbol->string node))))

(define-class <rdf-triple> ()
  ((subject
    :init-keyword :subject
    :accessor     rdf-triple-subject)
   (predicate
    :init-keyword :predicate
    :accessor     rdf-triple-predicate)
   (object
    :init-keyword :object
    :accessor     rdf-triple-object)))

(define-method make-rdf-triple ((sub <symbol>) (pre <symbol>) obj . keywords)
  (make <rdf-triple>
        :subject   sub
        :predicate pre
        :object    (if (rdf-uri-ref? obj)
                       obj
                       (apply make-rdf-literal obj keywords))))

(define (with-rdf-triple triple pred)
  (pred (rdf-triple-subject   triple)
        (rdf-triple-predicate triple)
        (rdf-triple-object    triple)))

(define-class <rdf-literal> ()
  ((content :init-keyword :content
            :init-vlaue #f)
   ;; XML Schama DATA type = <uri-ref>
   (type :init-keyword :type
         :init-value #f)
   ;; XML lang = en, ja, ...
   (lang :init-keyword :lang
         :init-value #f)))

(define (make-rdf-literal content . keywords)
  (apply make <rdf-literal>
         :content content
         keywords))

(define-method object-hash ((obj <rdf-literal>))
  (hash (ref obj 'content)))

(define-method object-equal? ((a <rdf-literal>) (b <rdf-literal>))
  (equal? (ref a 'content)
          (ref b 'content)))

(define-method object-equal? ((a <rdf-literal>) (b <top>))
  (equal? (ref a 'content) b))

(define-method object-equal? ((a <top>) (b <rdf-literal>))
  (object-equal? b a))

(define-class <rdf-store> ()
  ((by-subject
    :init-form (make-rdf-triple-index 'subject)
    :accessor  rdf-store-by-subject)
   (by-predicate
    :init-form (make-rdf-triple-index 'predicate)
    :accessor  rdf-store-by-predicate)
   (by-object
    :init-form (make-rdf-triple-index 'object)
    :accessor  rdf-store-by-object)
   (triples
    :init-form '()
    :accessor  rdf-store-triples)))

(define (make-rdf-store . triples)
  (let1 store (make <rdf-store>)
    (dolist (triple (get-optional triples '()) store)
      (rdf-store-push! store triple))))

(define-method rdf-store-push! ((store <rdf-store>) (triple <rdf-triple>))
  (with-rdf-triple triple
    (lambda (sub pre obj)
      (rdf-triple-index-push! (rdf-store-by-subject store)
                              sub
                              triple)
      (rdf-triple-index-push! (rdf-store-by-predicate store)
                              pre
                              triple)
      (rdf-triple-index-push! (rdf-store-by-object store)
                              obj
                              triple)))
  (push! (rdf-store-triples store) triple))

(define-method rdf-store-delete! ((store <rdf-store>) (triple <rdf-triple>))
  (with-rdf-triple triple
    (lambda (sub pre obj)     
      (rdf-triple-index-delete! (rdf-store-by-subject store)
                                sub
                                triple)
      (rdf-triple-index-delete! (rdf-store-by-predicate store)
                                pre
                                triple)
      (rdf-triple-index-delete! (rdf-store-by-object store)
                                obj
                                triple)))
  (set! (rdf-store-triples store)
        (delete triple (rdf-store-triples store))))

(define-method rdf-store-merge! ((base <rdf-store>) stores)
  (dolist (store stores base)
    (for-each (pa$ rdf-store-push! base)
              (rdf-store-triples store))))

(define (make-rdf-triple-index type)
  (make-hash-table
   (case type
     ((subject)   'eq?)
     ((predicate) 'eq?)
     ((object)    'equal?))))

(define (rdf-triple-index-get index key)
  (hash-table-get index key '()))

(define (rdf-triple-index-push! index key triple)
  (hash-table-push! index key triple))

(define (rdf-triple-index-delete! index key triple)
  (cond ((rdf-triple-index-get index key)
         => (lambda (set)
              (let1 result (delete triple set)
                (if (null? result)
                    (hash-table-delete! index key)
                    (hash-table-put! index key result)))))
        (else #f)))

(define (rdf-triple-match store sub pre obj . keywords)
  (%triple-match store
                 sub
                 pre
                 (and obj
                      (if (or (rdf-uri-ref? obj)
                              (is-a? obj <rdf-literal>))
                          obj
                          (apply make-rdf-literal obj keywords)))))

(define (rdf-triple-match-delete! store sub pre obj)
  (dolist (triple (rdf-triple-match store sub pre obj) store)
    (let1 obj-node (rdf-triple-object triple)
      (and (rdf-bnodeID? obj-node)
           (rdf-triple-match-delete! store obj-node #f #f))
      (rdf-store-delete! store triple))))

(define-method %triple-match ((store <rdf-store>) sub pre obj)
  (define (store-filter triples q ref e?)
    (if q
        (filter (lambda (triple)
                  (e? q (ref triple)))
                triples)
        triples))
  (cond (sub
         (store-filter
          (store-filter (rdf-triple-index-get (rdf-store-by-subject store) sub)
                     pre
                     rdf-triple-predicate
                     eq?)
          obj
          rdf-triple-object
          equal?))
        (pre
         (store-filter (rdf-triple-index-get (rdf-store-by-predicate store) pre)
                    obj
                    rdf-triple-object
                    equal?))
        (obj
         (rdf-triple-index-get (rdf-store-by-object store) obj))
        (else
         (rdf-store-triples store))))

(provide "rdf/store")
