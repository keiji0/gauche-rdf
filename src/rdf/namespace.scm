(define-module rdf.namespace
  (use srfi-1)
  (use gauche.parameter)
  (export namespace-env
          make-namespace
          namespace-get
          namespace-put!
          namespace-export

          qname->uri
          uri->qname
          ))

(select-module rdf.namespace)

(define (make-namespace binds)
  ;; binds = (<symbol> <string>) ...
  (let1 env (make-hash-table)
    (dolist (bind binds env)
      (apply namespace-put! env bind))))

(define (namespace-get env prefix)
  (hash-table-get env prefix #f))

(define (namespace-put! env prefix uri)
  (hash-table-put! env prefix uri))

(define namespace-env
  (make-parameter
    (make-namespace
     '((xhtml   "http://www.w3.org/1999/xhtml")
       (rdf     "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
       (rdfs    "http://www.w3.org/2000/01/rdf-schema#")
       (svg     "http://www.w3.org/2000/svg")
       (xs      "http://www.w3.org/2001/XMLSchema")
       (xsi     "http://www.w3.org/2001/XMLSchema-instance")
       (owl     "http://www.w3.org/2002/07/owl#")
       (geo     "http://www.w3.org/2003/01/geo/wgs84_pos#")
       (air     "http://www.daml.org/2001/10/html/airport-ont#")
       (rss     "http://purl.org/rss/1.0/")
       (content "http://purl.org/rss/1.0/modules/content/")
       (dc      "http://purl.org/dc/elements/1.1/")
       (dcterms "http://purl.org/dc/terms/")
       (dctype  "http://purl.org/dc/dcmitype/")
       (foaf    "http://xmlns.com/foaf/0.1/")
       ))))

(define-macro (namespace-export ns prefix vars)
  `(begin ,@(map (lambda (var)
                   (let ((name (string->symbol #`",|prefix|,|var|"))
                         (uri  (string->symbol #`",|ns|,|var|")))
                     `(define ,name ',uri)))
                 vars)))

(define (qname->uri qname)
  (receive (pre loc)
      ;; qname split
      (rxmatch-let (#/^([\w]+):([^:]+)?/ (x->string qname))
          (#f prefix local)
        (values prefix local))
    (cond ((namespace-get (namespace-env) (string->symbol pre))
           => (lambda (pref)
                (string->symbol (string-append pref loc))))
          (else
           (error "unbound namespace" qname)))))

(define (uri->qname uri)
  (let find-loop ((binds (hash-table-map (namespace-env) cons)))
    (if (null? binds)
        uri
        (receive (key val) (car+cdr (car binds))
          (cond ((string-scan (x->string uri) val 'after)
                 => (lambda (local)
                      (string->symbol #`",|key|:,|local|")))
                (else
                 (find-loop (cdr binds))))))))

(provide "rdf/namespace")
