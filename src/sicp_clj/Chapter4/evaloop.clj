(ns sicp-clj.Chapter4.evaloop)


(declare eval)

(defn error [error-msg]
  (println error-msg)
  (throw IllegalStateException))

(defn pair? [exp]
  (list? exp))                                              ;;The pair? function int clj is list?

(defn car [exp]
  (first exp))

(defn null? [exp]
  (or (nil? exp) (empty? exp)))

(defn cdr [exp]                                             ;;To replace rest to cdr and return nil if empty
  (let [result (rest exp)]
    (if (= result '())
      nil
      result)))

(defn cadr [exp]
  (car (cdr exp)))

(defn caadr [exp]
  (car (car (cdr exp))))

(defn caddr [exp]
  (nth exp 2))

(defn cdadr [exp]
  (cdr (car (cdr exp))))

(defn cdddr [exp]
  (cdr (cdr (cdr exp))))

(defn cddr [exp]
  (cdr (cdr exp)))

(defn cadddr [exp]
  (car (cdr (cdr (cdr exp)))))







(defn tagged-list? [exp tag]
  (if (pair? exp)
    (= (car exp) tag)
    false))

(defn application? [exp]
  (pair? exp))

;;===----------------------------------------------------------------------===
;; Making Environment
;;===----------------------------------------------------------------------===
(defn enclosing-environment [env]
  (cdr env))

(defn first-frame [env]
  (car env))



;;===----------------------------------------------------------------------===
;; BOOLEAN
;;===----------------------------------------------------------------------===
(defn true? [x]
  (not (= x false)))

(defn false? [x]
  (= x false))

;;===----------------------------------------------------------------------===
;; Handle Procedure
;;===----------------------------------------------------------------------===
(defn make-procedure [parameters body env]
  (list 'procedure parameters body env))

(defn compound-procedure? [p]
  (tagged-list? p 'procedure))

(defn procedure-parameters [p]
  (cadr p))

(defn procedure-body [p]
  (caddr p))

(defn procedure-environment [p]
  (cadddr p))

;;===----------------------------------------------------------------------===
;; Operate
;;===----------------------------------------------------------------------===
(defn operator [exp]
  (car exp))

(defn operands [exp]
  (cdr exp))

(defn no-operands? [ops]
  (null? ops))

(defn first-operand [ops]
  (car ops))

(defn rest-operands [ops]
  (cdr ops))

(defn begin? [exp]
  (tagged-list? exp 'begin))

(defn begin-actions [exp]
  (cdr exp))

(defn last-exp? [exp]
  (null? (cdr exp)))

(defn first-exp [seq]
  (car seq))

(defn rest-exps [seq]
  (cdr seq))

(defn make-begin [seq]
  (cons 'begin seq))

(defn sequence->exp [seq]
  (cond (null? seq) seq
        (last-exp? seq) (first-exp seq)
        :else (make-begin seq)))

;;===----------------------------------------------------------------------===
;; IF Expression
;;===----------------------------------------------------------------------===
(defn if? [exp]
  (tagged-list? exp 'if))

(defn if-predicate [exp]
  (cadr exp))

(defn if-consequent [exp]
  (caddr exp))

(defn if-alternative [exp]
  (if (not (null? (cdddr exp)))                            ;;In clj, (rest '()) => '(), so we should use empty?
    (cadddr exp)
    'false))

(defn make-if [predicate consequent alternative]
  (list 'if predicate consequent alternative))

;;===----------------------------------------------------------------------===
;; COND Expression
;;===----------------------------------------------------------------------===
(defn cond? [exp]
  (tagged-list? exp 'cond))

(defn cond-clauses [exp]
  (cdr exp))

(defn cond-predicate [clause]
  (car clause))

(defn cond-actions [clause]
  (cdr clause))

(defn cond-else-clause? [clause]
  (= (cond-predicate clause) 'else))

(defn expand-clauses [clauses]
  (if
    (null? clauses)
    'false
    (let [first (car clauses)
          rest (cdr clauses)]
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error (str "ELSE clause isn't last -- COND->IF " clauses)))
        (make-if (cond-predicate first)
                 (sequence->exp (cond-actions first))
                 (expand-clauses rest))))))

(defn cond->if [exp]
  (expand-clauses (cond-clauses exp)))

;;===----------------------------------------------------------------------===
;; LAMBDA Expression
;;===----------------------------------------------------------------------===
(defn lambda? [exp]
  (tagged-list? exp 'lambda))

(defn lambda-parameters [exp]
  (cadr exp))

(defn lambda-body [exp]
  (cddr exp))

(defn make-lambda [parameters body]
  (cons 'lambda (cons parameters body)))

;; handle definition
(defn definition? [exp]
  (tagged-list? exp 'define))

(defn definition-variable [exp]
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

(defn definition-value [exp]
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
                 (cddr exp))))

;;===----------------------------------------------------------------------===
;; ASSIGN Expression
;;===----------------------------------------------------------------------===
(defn assignment? [exp]
  (tagged-list? exp 'set))

(defn assignment-variable [exp]
  (cadr exp))

(defn assignment-value [exp]
  (caddr exp))

;;===----------------------------------------------------------------------===
;; Number OR String
;;===----------------------------------------------------------------------===
(defn self-evaluating? [exp]
  (cond
    (number? exp) true
    (string? exp) true
    :else false))


(defn quoted? [exp]
  (tagged-list? exp 'quote))

(defn text-of-quotation [exp]
  (cadr exp))

(defn variable? [exp]
  (symbol? exp))



(defn eval-definition [exp env]
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(defn eval-assignment [exp env]
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(defn eval-sequence [exps env]
  (cond (last-exp? exps) (eval (first-exp exps) env)
        :else
          (do
            (eval (first-exp exps) env)
            (eval-sequence (rest-exps exps) env))))

(defn eval-if [exp env]
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(defn list-of-values [exps env]
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(defn apply [procedure arguments]
  (cond

    (primitive-procedure? procedure)
      (apply-primitive-procedure procedure arguments) ;;应用基本过程

    (compound-procedure? procedure)                         ;;递归下降化约高级过程
      (eval-sequence
        (procedure-body procedure)
        (extend-environment
          (procedure-parameters procedure)
          arguments
          (procedure-environment procedure)))

    :else (error (str "Unknown procedure type -- APPLY" procedure))))

(defn eval [exp env]
  (cond
    (self-evaluating? exp) exp                              ;;parse字面量< Number > | < String >

    (variable? exp) (lookup-variable-value exp env)         ;;parse变量

    (quoted? exp) (text-of-quotation exp)                   ;;parse符号

    (assignment? exo) (eval-assignment exp env)             ;;parse变量赋值

    (definition? exp) (eval-definition exp env)             ;;parse函数定义

    (if exp) (eval-if exp env)                              ;;parse if

    (lambda? exp)
      (make-procedure (lambda-parameters exp)               ;;parse lambda表达式
                      (lambda-body exp)
                      env)

    (begin? exp) (eval-sequence (begin-actions exp) env)    ;;parse 串行化语句

    (cond? exp) (eval (cond->if exp) env)

    (application? exp)
      (apply (eval (operator exp) env)
             (list-of-values (operands exp) env))

    :else (error (str  "Unknown expression type -- EVAL " exp))))

