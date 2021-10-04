;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Parser) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define resolve
  (lambda (varName env)
    (cond
      ((null? env) #f)
      ((eq? (car (car env)) varName) (car (cdr (car env)))) ; have to add the car part so that you don't get the list containing the thing but rather just the thing
      ; it boils down to the value rather than the list containing the value
      (else (resolve varName (cdr env))))))

(define extend-env
  (lambda (lo-vars lo-vals env)
    (cond
      ((null? lo-vars) env)
      (else (extend-env (cdr lo-vars)
                        (cdr lo-vals)
                        (cons (list (car lo-vars) (car lo-vals)) env))))))
                                      

(define do-mathy-stuff-toaster
  (lambda (op num1 num2)
    (cond
      ((eq? op '+) (+ num1 num2))
      ((eq? op '-) (- num1 num2))
      ((eq? op '/) (/ num1 num2))
      ((eq? op '//) (quotient num1 num2))
      ((eq? op '%) (modulo num1 num2))
      ((eq? op '*) (* num1 num2))
      (else #f))))

(define no-parser
  (lambda (no-code)
    (cond
      ((null? no-code) '())
      ((number? no-code) (list 'num-lit-exp no-code))
      ((symbol? no-code) (list 'var-exp no-code)) ;if you are looking at the identifier, output the parsed identifier
      ((eq? (car no-code) 'do-mathy-stuff)
       (list 'math-exp (cadr no-code) (no-parser (caddr no-code)) (no-parser (cadddr no-code))))
      ((eq? (car no-code) 'ask)
       (list 'bool-exp (cadr no-code) 'do (no-parser(caddr no-code)) 'otherwise (no-parser(car(cdddr no-code)))))
      ((eq? (car no-code) 'function) ; this is what you do when you see a function 
       (list 'func-exp
             (append (list 'params) (cadr no-code))
             (list 'body
                   (no-parser (caddr no-code)))))
      (else (list 'call-exp
                  (no-parser (cadr no-code))
                  (map no-parser (cddr no-code)))))))



(define run-parsed-code
  (lambda (parsed-no-code env)
    (cond
      ((eq? (car parsed-no-code) 'num-lit-exp)
       (cadr parsed-no-code))
      ((eq? (car parsed-no-code) 'var-exp); you have a cdr that is equal to a variable & you want to resolve it 
       (resolve (cadr parsed-no-code) env))
      ((eq? (car parsed-no-code) 'math-exp)
       (do-mathy-stuff-toaster (cadr parsed-no-code) (run-parsed-code(caddr parsed-no-code) env) (run-parsed-code(cadddr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'func-exp)
       (run-parsed-code (cadr (caddr parsed-no-code)) env))
      ((eq? (car parsed-no-code) 'bool-exp)
         (cond
          ((eq? (car(cadr parsed-no-code)) 0) (run-parsed-code (cadddr parsed-no-code) env))
          (else (run-parsed-code (car(cdddr (cddr parsed-no-code))) env))))
      (else (run-parsed-code
             (cadr parsed-no-code) ;running the func expression 
             (extend-env (cdr (cadr(cadr parsed-no-code)))
                         (map (lambda (packet) (run-parsed-code (car packet) (cadr packet))) (map (lambda (x) (list x env)) (caddr parsed-no-code)))
                         env))))))
; adding a new name value pair to our env 

(define addMultipleParamsToEnv
  (lambda (params vars env) 
    (cond
      ((null? vars) '())
      ((eq? (car vars) 'num-lit-exp) (cons (append (list (car params)) (cadr vars) (addMultipleParamsToEnv (cdr params) (cdr (cdr vars)) env))))
      (else (cons (append (list (car params)) (list (resolve (cadr vars) env))) (addMultipleParamsToEnv (cdr params) (cdr (cdr vars)) env))))))


(define sample-no-code '(call (function (x) x) (ask (1) a b)))
(define parsed-no-code (no-parser sample-no-code))
;(display parsed-no-code)
(define env '((age 21) (a 7) (b 5) (c 23)))
(run-parsed-code parsed-no-code env)



 
    