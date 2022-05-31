#lang plait

(module+ test
  (print-only-errors #t))

;; abstract syntax -------------------------------

(define-type Op
  (add) (sub) (mul) (div) (eql) (leq))

(define-type Exp
  (numE [n : Number])
  (opE [op : Op] [l : Exp] [r : Exp])
  (ifE [b : Exp] [l : Exp] [r : Exp])
  (varE [x : Symbol])
  (letE [e2 : Exp][body : (Listof (Symbol * Exp))]);let z gwiazdką
  (letN [e2 : Exp][body : (Listof (Symbol * Exp))]);let zwykły
  )

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `SYMBOL s)
     (varE (s-exp->symbol s))]
     [(s-exp-match? `{let ANY ...} s)
     (letN
           (parse (second (s-exp->list s)))
           (parse-let-list (rest (rest (s-exp->list s)))))]
    [(s-exp-match? `{let* ANY ...} s)
     (letE
           (parse (second (s-exp->list s)))
           (parse-let-list (rest (rest (s-exp->list s)))))]
    [(s-exp-match? `{SYMBOL ANY ANY} s)
     (opE (parse-op (s-exp->symbol (first (s-exp->list s))))
          (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-let-list [ss : (Listof S-Exp)]) : (Listof (Symbol * Exp))
  (type-case (Listof S-Exp) ss
    [empty empty]
    [(cons s ss)
     (if (s-exp-match? `{ANY ANY} s)
         (cons (pair (s-exp->symbol (first (s-exp->list s)))
                     (parse (second (s-exp->list s))))
               (parse-let-list ss))
         (error 'parse "invalid input: cond"))]))
  
(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '/) (div)]
    [(eq? op '=) (eql)]
    [(eq? op '<=) (leq)]
    [else (error 'parse "unknown operator")]))
                
;; eval --------------------------------------

;; values

(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean]))

;; primitive operations

(define (op-num-num->proc [f : (Number Number -> Number)]) : (Value Value -> Value)
  (λ (v1 v2)
    (type-case Value v1
      [(numV n1)
       (type-case Value v2
         [(numV n2)
          (numV (f n1 n2))]
         [else
          (error 'eval "type error")])]
      [else
       (error 'eval "type error")])))

(define (op-num-bool->proc [f : (Number Number -> Boolean)]) : (Value Value -> Value)
  (λ (v1 v2)
    (type-case Value v1
      [(numV n1)
       (type-case Value v2
         [(numV n2)
          (boolV (f n1 n2))]
         [else
          (error 'eval "type error")])]
      [else
       (error 'eval "type error")])))

(define (op->proc [op : Op]) : (Value Value -> Value)
  (type-case Op op
    [(add) (op-num-num->proc +)]
    [(sub) (op-num-num->proc -)]
    [(mul) (op-num-num->proc *)]
    [(div) (op-num-num->proc /)]
    [(eql) (op-num-bool->proc =)]
    [(leq) (op-num-bool->proc <=)]))

;; environments

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)

(define (extend-env [env : Env] [x : Symbol] [v : Value]) : Env
  (cons (bind x v) env))

(define (lookup-env [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
    [empty (error 'lookup "unbound variable")]
    [(cons b rst-env) (cond
                        [(eq? n (bind-name b))
                         (bind-val b)]
                        [else (lookup-env n rst-env)])]))

;; evaluation function
(define (pom e2 body env)
     [cond
       [[empty? body] (eval e2 env)]
       [else
        (let [[val-pom (eval (snd (first body)) env)]]
        (pom e2 (rest body)(extend-env env (fst (first body)) val-pom)))]])
;> (eval (parse `{let* (+ y x) {x 2}{y (+ 2 x)}{z 3}}) mt-env)
;- Value
;(numV 6)
;> (eval (parse `{let (+ y x) {x 2}{y (+ 2 x)}{z 3}}) mt-env)
;- Value
;;dla leta:
(define (pom1 e2 body env env-new)
     [cond
       [[empty? body] (eval e2 env-new)]
       [else
        (let [[val-pom (eval (snd (first body)) env)]]
        (pom1 e2 (rest body) env (extend-env env-new (fst (first body)) val-pom)))]])

(define (eval [e : Exp] [env : Env]) : Value
  (type-case Exp e
    [(numE n) (numV n)]
    [(opE o l r) ((op->proc o) (eval l env) (eval r env))]
    [(ifE b l r)
     (type-case Value (eval b env)
       [(boolV v)
        (if v (eval l env) (eval r env))]
       [else
        (error 'eval "type error")])]
    [(varE x)
     (lookup-env x env)]
    [(letN e2 body)
      (pom1 e2 body env env)
     ]
    [(letE e2 body)
     (pom e2 body env)]))

(define (run [e : S-Exp]) : Value
  (eval (parse e) mt-env))



