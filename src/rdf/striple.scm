(define-module rdf.striple
  (use srfi-1)
  (use rdf.util)
  (export striple->triples)
  )

(select-module rdf.striple)

;; # <list> => <triples>
;;
;; <URI>        := <symbol>
;; <L>          := <string>
;;
;; <S>          := <URI>
;; <P>          := <URI>
;; <O>          := <URI>|<L>
;; <P->O>       := (<P> <O>)
;; <P->B->O>    := (<P> <O>|<P->B->O>+)
;; <S->O>       := (<S> <P->O>|<P->B->O)
;;
;; Triple              S Triple
;;-----------------------------------------
;;
;; {S, P, O}        := (<S> (<P> <O>))
;;
;; {S, P, BN}
;; {BN, P', O}      := (<S> (<P> (<P> <O>)))

(define (striple->triples lis . tmake)
  (let1 tmake (get-optional tmake list)
    (let rec ((lis lis))
      (let ((sub (car lis))
            (rest (cdr lis)))
        (append-map (lambda (p&o)
                      (let ((pre (car p&o))
                            (rest (cdr p&o)))
                        (if (pair? (car rest))
                            (let1 bnode (make-rdf-bnodeID)
                              (cons (tmake sub pre bnode)
                                    (rec (cons bnode rest))))
                            (list (apply tmake sub p&o)))))
                    rest)))))

(provide "rdf/striple")
