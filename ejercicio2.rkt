#lang eopl

;;Punto 2 del taller

;;Interpretador
(define-datatype sat-expression sat-expression?
  (var-exp (id symbol?))
  (not-exp (operand sat-expression?))
  (and-exp (operands (list sat-expression?)))
  (or-exp (operands (list sat-expression?)))
)


;;Función PARSEBNF
(define (PARSEBNF expP)
  (cond
    [(symbol? expP) (var-exp expP)]
    [(equal? (car expP) 'not)
     (not-exp (PARSEBNF (cadr expP)))]
    [(equal? (car expP) 'and)
     (and-exp (PARSEBNF-list (cdr expP)))]
    [(equal? (car expP) 'or)
     (or-exp (PARSEBNF-list (cdr expP)))]
    [else #f]
  )
)

(define (PARSEBNF-list exps)
  (if (null? exps)
      '()
      (cons (PARSEBNF (car exps)) (PARSEBNF-list (cdr exps)))
  )
)


;;Función UNPARSEBNF
(define UNPARSEBNF
  (lambda (exp)
    (cases sat-expression exp
      (var-exp (id) id)
      (not-exp (operand) (list 'not (UNPARSEBNF operand)))
      (and-exp (operands)
               (cons 'and (UNPARSEBNF-list operands)))
      (or-exp (operands)
              (cons 'or (UNPARSEBNF-list operands)))
      )
    )
  )

(define (UNPARSEBNF-list exps)
  (if (null? exps)
      '()
      (cons (UNPARSEBNF (car exps)) (UNPARSEBNF-list (cdr exps)))
  )
)


