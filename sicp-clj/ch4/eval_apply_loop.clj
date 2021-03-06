(ns ch4.eval-apply-loop
  (:require [clojure.pprint :refer [pprint]]))

(declare eval-1)
(declare apply-1)

(defn error [error-msg]
  (pprint error-msg))

(defn pair? [exp]
  (seq? exp))                                              ;;The pair? function int clj is list?

(defn car [exp]
  (first exp))

(defn null? [exp]
  (or (nil? exp) (= exp '())))

(defn cdr [exp]                                           ;;To replace rest to cdr and return nil if empty
  (rest exp))

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
;; Abouts Frame
;; Using hashmap to replace frame in sicp
;;===----------------------------------------------------------------------===
(defn enclosing-environment [env]
  (cdr env))

(defn first-frame [env]
  (car env))

(def the-empty-environment (list))

(defn make-frame [variables values]
  (atom (zipmap variables values)))

;(defn frame-variable [frame]
;  (keys @frame))
;
;(defn frame-values [frame]
;      (vals @frame))

;;The Frame looks like {a 1 b 2 c 3}
(defn add-binding-to-frame! [var val frame]
  (swap! frame assoc var val))

(defn extend-environment [vars vals base-env]
  (let [[varlen vallen] [(count vars) (count vals)]]
    (if (= varlen vallen)
      (cons (make-frame vars vals) base-env)
      (if (< varlen vallen)
        (error (str "Too many args supplied" vars vals))
        (error (str "Too few args supplied" vars vals))))))

(defn lookup-variable-value [var env]
  (println "var " var)
  (letfn [(env-loop [env]
            (if (= env the-empty-environment)
              (error (str "Unbound variable " var))
              (let [this-frame @(first-frame env)]          ;;using contains? instead of get because the value maybe nil
                (if (contains? this-frame var)
                  (get this-frame var)
                  (recur (enclosing-environment env))))))]
    (env-loop env)))


(defn set-variable-value! [var val env]
  (letfn [(env-loop [env]
            (if (= env the-empty-environment)
              (error (str "Unbound variable -- SET " var)))
            (let [this-frame (first-frame env)]
              (if (contains? @this-frame var)
                (add-binding-to-frame! var val this-frame)
                (recur (enclosing-environment env)))))]
    (env-loop env)))

(defn define-variable! [var val env]
  (let [this-frame (first-frame env)]
    (add-binding-to-frame! var val this-frame)))

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
;; Inplementation apply procedure
;;
;;===----------------------------------------------------------------------===

(def apply-in-underlying-clojure apply)

(defn primitive-procedure? [proc]
  (tagged-list? proc 'primitive))

(defn primitive-implementation [proc]
  (cadr proc))

(def primitive-procedure
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))

(def primitive-procedure-names
  (map car primitive-procedure))

(def primitive-procedure-objects
  (map (fn [proc] (list 'primitive (cadr proc))) primitive-procedure))

(defn apply-primitive-procedure [proc args]
  (apply-in-underlying-clojure
    (primitive-implementation proc) args))

;;===----------------------------------------------------------------------===
;; Setting Environment
;;
;;===----------------------------------------------------------------------===

(def setup-environment
  (let [initial-env (extend-environment primitive-procedure-names
                                        primitive-procedure-objects
                                        the-empty-environment)]
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(def the-global-environment setup-environment)


;;===----------------------------------------------------------------------===
;;Driver Loop
;;===----------------------------------------------------------------------===
(defn user-print [object]
  (if (compound-procedure? object)
    (println (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   "<procedure-env>"))
    (println object)))

(def input-prompt "\n\n;;; M-Eval input:\n")
(def output-prompt "\n;;; M-Eval value: ")

(defn prompt-for-input [prompt]
  (println prompt))

(defn announce-output [prompt]
  (println prompt "\n"))

(defn driver-loop []
  (prompt-for-input input-prompt)
  (let [input (read)]
    (let [output (eval-1 input the-global-environment)]
      (announce-output output-prompt)
      (user-print output)))
  (recur))


;;===----------------------------------------------------------------------===
;; BOOLEAN
;;===----------------------------------------------------------------------===
(defn true? [x]
  (not (= x false)))

(defn false? [x]
  (= x false))


;;===----------------------------------------------------------------------===
;; Operator (In lisp , an operator is represent a function
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

;;===----------------------------------------------------------------------===
;; DEFINE Expression
;;===----------------------------------------------------------------------===
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
                    (eval-1 (definition-value exp) env)
                    env)
  'ok)

(defn eval-assignment [exp env]
  (set-variable-value! (assignment-variable exp)
                       (eval-1 (assignment-value exp) env)
                       env)
  'ok)

(defn eval-sequence [exps env]
  (cond (last-exp? exps) (eval-1 (first-exp exps) env)
        :else
        (do
          (eval-1 (first-exp exps) env)
          (eval-sequence (rest-exps exps) env))))

(defn eval-if [exp env]
  (if (true? (eval-1 (if-predicate exp) env))
    (eval-1 (if-consequent exp) env)
    (eval-1 (if-alternative exp) env)))

(defn list-of-values [exps env]
  (if (no-operands? exps)
    '()
    (cons (eval-1 (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

;;===----------------------------------------------------------------------===
;; Eval-Apply Loop
;;===----------------------------------------------------------------------===

(defn apply-1 [procedure arguments]
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

(defn eval-1 [exp env]
  (println "exp " exp)
  (cond
    (self-evaluating? exp) exp                              ;;parse字面量< Number > | < String >

    (variable? exp) (lookup-variable-value exp env)         ;;parse变量

    (quoted? exp) (text-of-quotation exp)                   ;;parse符号

    (assignment? exp) (eval-assignment exp env)             ;;parse变量赋值

    (definition? exp) (eval-definition exp env)             ;;parse函数定义

    (if? exp) (eval-if exp env)                              ;;parse if

    (lambda? exp)
    (make-procedure (lambda-parameters exp)                  ;;parse lambda表达式
                    (lambda-body exp)
                    env)

    (begin? exp) (eval-sequence (begin-actions exp) env)    ;;parse 串行化语句

    (cond? exp) (eval-1 (cond->if exp) env)

    (application? exp)
    (apply-1 (eval-1 (operator exp) env)
            (list-of-values (operands exp) env))

    :else (error (str  "Unknown expression type -- EVAL " exp))))

(driver-loop)

;(define (append x y)
;        (if (null? x)
;          y
;          (cons (car x) (append (cdr x) y))))