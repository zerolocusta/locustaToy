(ns sicp-clj.Chapter4.evaloop)


(declare eval)

(defn error [error-msg]
  (println error-msg)
  (throw IllegalStateException))





(defn list-of-values [exps env]
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(defn apply [procedure arguments]
  (cond

    (primitive-procedure? procedure)
      (apply-primitive-procedure procedure arguments) ;;应用基本过程

    (compound-procedure? procedure)
      (eval-sequence
        (procedure-body procedure)
        (extend-environment
          procedure-parameters procedure)
        arguments
        (procedure-environment procedure))

    :else
    (error (str "Unknown procedure type -- APPLY" procedure))))

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

    :else
      (error (str  "Unknown expression type -- EVAL " exp))
    ))

