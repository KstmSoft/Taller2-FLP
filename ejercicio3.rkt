#lang eopl

(define (evaluar-literal literal valores-verdad)
  (if (null? valores-verdad)
      #f
      (if (eq? literal 0)
          (not (memq (abs literal) valores-verdad))
          (= (car valores-verdad) 0))))

(define (evaluar-or expresion valores-verdad)
  (cond
    ((null? expresion) #f)
    ((evaluar-literal (car expresion) valores-verdad) #t)
    (else (evaluar-or (cdr expresion) valores-verdad))))

(define (evaluar-sat instancia-fnc valores-verdad)
  (cond
    ((null? instancia-fnc) #t)
    ((not (evaluar-or (car instancia-fnc) valores-verdad)) #f)
    (else (evaluar-sat (cdr instancia-fnc) valores-verdad))))

(define (generar-combinaciones n)
  (if (= n 0)
      (list '())
      (let* ((combinaciones-menor (generar-combinaciones (- n 1)))
             (combinaciones-mayor (map (lambda (combinacion) (cons #t combinacion)) combinaciones-menor))
             (combinaciones-falso (map (lambda (combinacion) (cons #f combinacion)) combinaciones-menor)))
        (append combinaciones-menor combinaciones-mayor combinaciones-falso))))

(define (mi-filter predicado lista)
  (cond
    ((null? lista) '())
    ((predicado (car lista)) (cons (car lista) (mi-filter predicado (cdr lista))))
    (else (mi-filter predicado (cdr lista)))))

(define (EVALUARSAT instancia-fnc)
  (let* ((num-variables (apply max (map abs (apply append instancia-fnc))))
         (combinaciones-verdad (generar-combinaciones num-variables)))
    (mi-filter (lambda (valores-verdad)
                 (evaluar-sat instancia-fnc valores-verdad))
               combinaciones-verdad)))