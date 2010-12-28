(define-module rdf.parser.xml
  (use srfi-1)
  (use srfi-13)
  (use util.list)
  (use sxml.ssax)
  (use sxml.tools)
  (use text.parse)
  (use rdf.namespace)
  (use rdf.util)
  
  (export rdf-xml-parse)
  )

(select-module rdf.parser.xml)

;;; RDF values
(namespace-export "http://www.w3.org/1999/02/22-rdf-syntax-ns#" **rdf:
  (type Property Statement subject
   predicate object Bag Seq Alt value
   List nil first rest XMLLiteral
   ;; CoreSyntaxTerms
   RDF ID about parseType
   resource nodeID datatype
   ;; syntaxTerms
   Description li
   ;; oldTerms
   aboutEach aboutEachPrefix bagID
   ))

(define (rdf-xml-parse port . keywords)
  (let-keywords* keywords ((base-uri #f)
                           (seed '())
                           (seed-proc
                            (lambda (seed sub pre obj . keywords)
                              (cons (list* sub pre obj keywords) seed))))
    (let* ((node (xml->rdf/sxml port))
           (base-uri (or (attr-get-value node 'xml:base)
                         base-uri
                         "")))
      (letrec ((add-triple
                (lambda (sub pre obj . keywords)
                  (set! seed (apply seed-proc seed sub pre obj keywords))))
               (subject-handle
                ;; return subject-uri
                (lambda (node base)
                  (receive (subject-uri _ _ _ base _ property-attrs) (rdf-attribute-parse node base)
                    (begin0 subject-uri
                            ;; Typed node
                            (let1 node-name (sxml:node-name node)
                              (unless (eq? node-name **rdf:Description)
                                (add-triple subject-uri **rdf:type node-name)))
                            ;; Property
                            (for-each (cut apply add-triple subject-uri <>)
                                      (append property-attrs
                                              (map (cut predicate-handle <> base)
                                                   (rdf-object-node-parse node))))
                            ))))

               (predicate-handle
                ;; return (predicate object)
                (lambda (node base)
                  (receive (_ resource-uri parsetype type base lang propertys) (rdf-attribute-parse node base)
                    (let* ((pred-uri (sxml:node-name node))
                           (obj-nodes  (sxml:content node))
                           (obj-node-first (if (null? obj-nodes) '() (car obj-nodes))))
                      (cond
                       ;; rdf:resource property
                       (resource-uri
                        (for-each (cut apply add-triple resource-uri <>)
                                  propertys)
                        (list pred-uri resource-uri))
                       ;; Parsetype
                       (parsetype
                        (case (string->symbol parsetype)
                          ((Literal)
                           (list pred-uri obj-node-first lang ttype))
                          ((Resource)
                           (let1 bnode (make-rdf-bnodeID)
                             (subject-handle
                              (apply make-rdf-statement bnode #f obj-nodes) base)
                             (list pred-uri bnode)))
                          ((Collection)
                           (let1 bnode (make-rdf-bnodeID)
                             (for-each (lambda (subject-node)
                                         (subject-handle subject-node base))
                                       (make-rdf-collection bnode obj-nodes))
                             (list pred-uri bnode)))
                          (else (error "RDF/XML Syntax: Unknown Parsetype" parsetype))))
                       ;; Resource property
                       (else
                        (cond ((null? obj-nodes)
                               (let1 bnode (make-rdf-bnodeID)
                                 (for-each (cut apply add-triple bnode <>)
                                           propertys)
                                 (list pred-uri bnode)))
                              ((null? (cdr obj-nodes))
                               (let1 obj-node (car obj-nodes)
                                 (cond ((pair? obj-node)
                                        (list pred-uri (subject-handle obj-node base)))
                                       (else
                                        (list pred-uri obj-node lang type)))))
                              (else
                               (error "RDF/XML Syntax: Encountered element" obj-nodes))))
                       )))))
               ) ; END letrec
        (begin (for-each (cut subject-handle <> base-uri)
                         (sxml:content node))
               seed)
        ))))

(define (rdf-object-node-parse node)
  ;; rdf:li -> rdf:_1 rdf:...
  (let1 li-id (let1 n 0
                (lambda ()
                  (begin (inc! n)
                         (qname->uri #`"rdf:,|n|"))))
    (map (lambda (obj-node)
           (if (and (pair? obj-node)
                    (eq? **rdf:li (car obj-node)))
               (cons (li-id) (cdr obj-node))
               obj-node))
         (sxml:content node))))

(define (make-rdf-statement sub pre . obj)
  `(,**rdf:Description (@ (,**rdf:about ,(x->string sub)))
                     ,@(if pre
                           (let1 obj (car obj)
                             `((,pre
                                ,(if (rdf-uri-ref? obj)
                                     `(@ (,**rdf:resource ,(x->string obj)))
                                     obj))))
                           obj)))

(define (make-rdf-collection first . rest)
  (let loop ((result '())
             (first first)
             (rest rest))
    (cond ((null? rest)
           (cons (make-rdf-statement first **rdf:rest **rdf:nil)
                 result))
          (else
           (let1 rest-node (make-rdf-bnodeID)
             (loop (cons* (make-rdf-statement first **rdf:first (car rest))
                          (make-rdf-statement first **rdf:rest rest-node)
                          result)
                   rest-node
                   (cdr rest)))))))

(define (make-rdf-about-uri uri base)
  (if (string-null? uri)
      (make-rdf-bnodeID "about")
      (string->symbol
       (cond ((string-prefix? "#" uri)
              (string-append base uri))
             (else
              uri)))))

(define (rdf-attribute-parse node base)
  (define (attr-eq-get attr key . pred)
    (and (eq? (car attr) key)
         ((get-optional pred values) (cadr attr))))
  (receive (attrs propertys)
      (find-fold (sxml:attr-list-u node)
                 ;; 1. rdf:ID|about|nodeID <symbol>
                 (lambda (attr)
                   (let ((key (car attr))
                         (val (cadr attr)))
                     (cond ((eq? key **rdf:ID)
                            (string->symbol
                             (format "~a#~a" base val)))
                           ((eq? key **rdf:about)
                            (make-rdf-about-uri val base))
                           ((eq? key **rdf:nodeID)
                            (make-rdf-bnodeID val))
                           (else #f))))
                 ;; 2. rdf:resource
                 (lambda (attr)
                   (attr-eq-get attr **rdf:resource
                                (cut make-rdf-about-uri <> base)))
                 ;; 3. rdf:parseType ("Literal"|"Resource"|"Collection")?
                 (lambda (attr)
                   (attr-eq-get attr **rdf:parseType))
                 ;; 4. rdf:datatype = `xml schema'
                 (lambda (attr)
                   (attr-eq-get attr **rdf:datatype))
                 ;; 5. xml:base
                 (lambda (attr)
                   (attr-eq-get attr 'xml:base))
                 ;; 6. xml:lang
                 (lambda (attr)
                   (attr-eq-get attr 'xml:lang))
                 ;; 7. propertys ((key . val)*)
                 )
    (apply values
           (append (map-ref$ (cute f-or <> (make-rdf-bnodeID))
                             (map-ref$ (cut f-or <> base) attrs 4)
                             0)
                   (list propertys)))))

(define (xml->rdf/sxml port)

  (define (unres-name->sxml res-name)
    (string->symbol
     (string-append (symbol->string (car res-name))
                    (if (eq? ssax:Prefix-XML (car res-name)) ":" "")
                    (symbol->string (cdr res-name)))))
  
  (define (rdf-document? elem-gi)
    (eq? (unres-name->sxml elem-gi) **rdf:RDF))
  
  (define (ssax:read-entity port entities)
    (let ((value-delimeters '(#\< #\&)))
      (define (entity-skip port)
        (if (not (find-string-from-port? ">" port))
            (parser-error port "Failed to fportd > termportatportg the portternal DTD subset")))
      (if (char=? (peek-char port) #\%)
          (begin (entity-skip port) #f)
          (let1 entity-name (ssax:read-NCName port)
            (ssax:skip-S port)
            (begin0 (and=> (case (read-char port)
                             ((#\")
                              (next-token '() (cons #\" value-delimeters) "XML [1]" port))
                             ((#\')
                              (next-token '() (cons #\' value-delimeters) "XML [1]" port))
                             (else #f))
                           (cut cons entity-name <>))
                    (entity-skip port))))))

  (define (ssax:read-decl port)
    (define (skip-description port)
      (find-string-from-port? ">" port))
    (let loop ((entities '()))
      (ssax:skip-S port)
      (cond ((eq? '#\] (peek-char port))
             (skip-description port)
             entities)
            ((eq? 'ENTITY (xml-token-head (ssax:read-markup-token port)))
             (ssax:skip-S port)
             (loop
              (cond ((ssax:read-entity port entities)
                     => (lambda (entity)
                          (cons entity entities)))
                    (else entities))))
            (else
             (skip-description port)
             (loop result)))))
  
  (let ((document-root-read-flag #f) ;; document root
        (namespaces '()))
    (car
     (sxml:content
      (reverse
       ((ssax:make-parser NEW-LEVEL-SEED
                          (lambda (elem-gi attributes namespaces expected-content seed)
                            (if document-root-read-flag
                                '()
                                (if (rdf-document? elem-gi)
                                    (begin (set! document-root-read-flag #t) '())
                                    (error "Document is not RDF/XML" elem-gi))))
                          FINISH-ELEMENT
                          (lambda (elem-gi attributes namespaces parent-seed seed)
                            (let ((seed (ssax:reverse-collect-str-drop-ws seed))
                                  (attrs (attlist-fold (lambda (attr accum)
                                                         (cons (list (if (symbol? (car attr)) (car attr) (unres-name->sxml (car attr)))
                                                                     (cdr attr))
                                                               accum))
                                                       '() attributes)))
                              (cons (cons (if (symbol? elem-gi) elem-gi (unres-name->sxml elem-gi))
                                          (if (null? attrs) seed (cons (cons '|@| attrs) seed)))
                                    parent-seed)))
                          CHAR-DATA-HANDLER
                          (lambda (string1 string2 seed)
                            (if (string-null? string2)
                                (cons string1 seed)
                                (cons* string2 string1 seed)))
                          DOCTYPE
                          (lambda (port docname systemid internal-subset? seed)
                            (values #f (ssax:read-decl port) '() seed))
                          UNDECL-ROOT
                          (lambda (elem-gi seed)                                 
                            (values #f '() '() seed))
                          PI
                          ((*DEFAULT* lambda (port pi-tag seed) (cons (list '*PI* pi-tag (ssax:read-pi-body-as-string port)) seed))))
        port '()))))))


;; Util

(define (attr-get-value node key)
  (and=> (assq key (sxml:attr-list-u node))
         cadr))

(define (f-or . args)
  (if (null? args)
      #f
      (or (car args)
          (apply f-or (cdr args)))))

(define (and=> x . preds)
  (if (or (eq? x #f) (null? preds)) x
      (apply and=> ((car preds) x)
             (cdr preds))))

(define (map-ref$ pred lis len)
  (receive (right left)
      (split-at lis len)
    (append right
            (cons (pred (car left)) (cdr left)))))

(define (find-fold lis . preds)
  (define (find-partition pred lis)
    (let loop ((rlis  lis)
               (rest  '()))
      (cond ((null? rlis)
             (values #f lis))
            ((pred (car rlis))
             => (lambda (result)
                  (values result
                          (append (reverse rest) (cdr rlis)))))
            (else
             (loop (cdr rlis)
                   (cons (car rlis) rest))))))
  (let loop ((preds  preds)
             (result '())
             (rest   lis))
    (if (null? preds)
        (values (reverse result) rest)
        (receive (a b)
            (find-partition (car preds) rest)
          (loop (cdr preds)
                (cons a result)
                b)))))

(provide "rdf/parser/xml")
