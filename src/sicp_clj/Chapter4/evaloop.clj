(ns sicp-clj.Chapter4.evaloop)


(declare eval)

(defn error [error-msg]
  (println error-msg)
  (throw IllegalStateException))

(defn pair? [exp]
  (list? exp))                                              ;;The pair? function int clj is list?

(defn car [exp]
  (first exp))

(defn cadr [exp]
  (first (rest exp)))

(defn caadr [exp]
  (first (first (rest exp))))

(defn caddr [exp]
  (nth exp 2))

(defn cdadr [exp]
  (rest (first (rest exp))))

(defn cddr [exp]
  (rest (rest exp)))

(defn tagged-list? [exp tag]
  (if (pair? exp)
    (= (first exp) tag)
    false))

;;Handle lambda expression
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








;;  变量赋值
;;  申请变量
;;  parse set! 语句
(defn assignment? [exp]
  (tagged-list? exp 'set))

(defn assignment-variable [exp]
  (cadr exp))

(defn assignment-value [exp]
  (caddr exp))

;; judge a Number or a Strings
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

(defn eval-if [exp env]                                     ;;对if语句求值
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

