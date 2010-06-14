
#|
基本式
数値のような自己評価式
evalは値を得るため、環境から変数を探すひつようがある。


特殊形式

／／／

|#

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp))
    ((assignment? exp) (eval-assignment exp env))
    ((definition? exp) (eval-definition exp env))
    ((if? exp) (eval-if exp env))
    ((lambda? exp)
     (make-procedure (lambda-parameters exp)
             (lambda-body exp)
             env))
    ((begin? exp)
     (eval-sequence (begin-actions exp) env))
    ((cond? exp) (eval (cond->if exp) env))
    ((application? exp)
     (apply (eval (operator exp) env)
        (list-of-values (operands exp) env)))
    (else
     (error "Unknown expression type -- EVAL" exp))))

;; apply
;; 元々のapplyを退避
(define apply-in-underlying-scheme apply)

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   arguments
	   (procedure-environment procedure))))
	(else
	 (error
	  "Unknown procedure type -- APPLY" procedure))))

;; 手続きの引数
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
        (list-of-values (rest-operands exps) env))))
;;条件式
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;;並び
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
    (else (eval (first-exp exps) env)
          (eval-sequence (rest-exps exps) env))))


;;代入と定義
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
               (eval (assignment-value exp) env)
               env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
		    env)
  'ok)

;;4.1
(define (list-of-values-lr exps env)
  (if (no-operands? exps)
      '()
      (let ((evaled-exp (eval (first-operand exps) env)))
	(cons evaled-exp
           (list-of-values-lr (rest-operands exps) env)))))

(define (list-of-values-lr exps env)
  (if (no-operands? exps)
      '()
      (let ((evaled-rest (list-of-values-lr (rest-operands exps) env)))
    (cons (eval (first-operand exps) env)
          evaled-rest))))

;; 式の表現
;; 自己評価式 数値と文字列
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
    ((string? exp) #t)
    (else #f)))
;; 変数
(define (variable? exp) (symbol? exp))
;; クォート式
(define (quoted? exp)
    (tagged-list? exp 'quote))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (text-of-quotation exp)
  (cadr exp))

;;代入
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;;定義
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
           (cddr exp))))



(define (make-definition variable value)
  (list 'define variable value))




;; lambda 式
(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;条件式
(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp) 
  (if (not (null? (cadddr exp)))
      (cadddr exp)
      'false))
      
    ;; cond->if で使用
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; begin
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exp seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else
	 (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;; 手続き作用
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))


;; cond
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause is'nt last --COND->IF" clauses))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))
(define (cond=>? acts)
  (tagged-list? acts '=>))
(define (cond-recievers acts)
  (cadr acts))
(define (cond-send-reciever predicate act)
  (list act predicate))


(define *table* (make-hash-table 'equal?))

(define (get package key)
  (hash-table-get *table* (list package key) #f))
(define (put package key value)
  (hash-table-put! *table* (list package key) value))


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	(else
	 (let ((op (get 'eval (operator exp))))
	   (cond (op (op exp env))
		 ((application? exp)
		  (apply (eval (operator exp) env)
			 (list-of-values (operands exp) env)))
		 (else
		  (error "Unknown expression type -- EVAL" exp)))))))


;; 4.4 and or
;; (and '()) と (and) は区別されなければならない。
;; というより自動的に区別される

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp)  (tagged-list? exp 'or))
(define (and-operands exp) (cdr exp))
(define (or-operands exp) (cdr exp))

(define (eval-and-seq seq env)
  (if (null? seq)     
      'true
      (let ((evaled-value (eval (first-exp seq) env)))
    (cond ((false? evaled-value) 'false)
          ((last-exp? seq) evaled-value)
          (else
           (eval-and-seq (rest-exp seq) env))))))

(define (eval-and exp env)
  (eval-and-seq (and-operands exp) env))

(define (eval-or-seq seq env)
  (if (null? seq)
      'false
      (let ((evaled-value (eval (first-exp seq) env)))
    (cond ((true? evaled-value) evaled-value)
          (else
           (eval-and-seq (rest-exp seq) env))))))



(define (eval-or exp env)
  (define (rest-seq-or seq) (cdr seq))
  (define (first-seq-or seq) (car seq))
  (define (eval-or-seq seq env)
    (if  (null? seq) 'false
     (let ((evaled-or (eval (first-seq-or seq))))
       (cond ((true? evaled-or) evaled-or)
         (else 
          (eval-or-seq (rest-seq-or seq) env)))))))

(define (eval-or exp env)  (eval-or-seq (or-operands exp) env))

;; let
(define (let? exp)
  (tagged-list? exp 'let))

(define (let-params exp)
  (cadr exp))

(define (let-body exp)
  (caddr exp))

(define (first-let-param params)
  (car params))
(define (rest-let-param params)
  (cdr params))

(define (let-param-variable param)
  (car param))
(define (let-param-value param)
  (cadr param))
#|
(define (let-params->var-list params)
  (if (null? params) '()
      (cons (let-param-variable (first-let-param params))
	    (let-params->var-list (rest-let-param params)))))
(define (let-params->val-list params)
  (if (null? params) '()
      (cons (let-param-value (first-let-param params))
	    (let-params->var-list (rest-let-param params)))))
|#
(define (let-params->var-val-list params)
  (if (null? params)
      (values '() '())
      (receive (vars vals) (let-params->var-val-list (cdr params))
	       (values (cons (let-param-variable 
			      (first-let-param params))
			     vars)
		       (cons (let-param-value 
			      (first-let-param params))
			     vals)))))
	 

(define (let->combination exp)
  (let ((body (let-body exp)))
    (receive (vars vals)
	     (let-params->var-val-list (let-params exp))
	     (if (null? vals)
		 (list (make-lambda vars body))
		 (cons (make-lambda vars body) vals)))))

(define (make-let var val body)
  (list 'let (list var val) body))
;; 4.7
;; let*->nested-lets
(define (let*? exp) (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (define (let*-iter params body)
    (if (null? params) body
	(list 'let 
	      (list (car params))
	      (let*-iter (cdr params) body))))
  (let*-iter (let-params exp) (let-body exp)))



(define (install-eval-package)
  ;;手続きの定義は上のほうですんでることにする。 
  ;; interfaceが違うものだけなんとかする。
  (define (eval-lambda exp env)
    (make-procedure (lambda-parameters exp)
            (lambda-body exp) env))
  (define (eval-begin exp env)
    (eval-sequence (begin-actions exp) env))

  (define (eval-cond exp env)
    (eval (cond->if exp) env))
  (define (eval-let exp env)
    (eval (let->combination exp) env))
  (define (eval-let* exp env)
    (eval (let*->nested-lets exp) env))
  (define (eval-quote exp env) (text-of-quotation exp))
  (put 'eval 'quote eval-quote)
  (put 'eval 'set! eval-assignment)
  (put 'eval 'define eval-definition)
  (put 'eval 'if eval-if)
  (put 'eval 'lambda  eval-lambda)
  (put 'eval 'begin eval-begin)
  (put 'eval 'cond eval-cond)
  (put 'eval 'or eval-or)
  (put 'eval 'and eval-and)
)

(install-eval-package)
;; 4-1-3
(define (false? x)
  (eq? x #f))
(define (true? x) 
  (not (false? x)))

;; 仮定
;; (apply-primitive-procedure <proc> <args>)
;; (primitive-procedure? <proc>)

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p)
  (cadr p))

(define (procedure-body p)
  (caddr p))

(define (procedure-environment p)
  (cadddr p))

;; 環境
;; (lookup-variable-value <var> <env>)
;; (extend-environment <variables> <values> <base-env>)
;; (define-variable! <var> <value> <env>)
;; (set-variable-value! <var> <value> <env>)


(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;; frame var & val

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-frame! var val frame)
  (set-car! frame (cons var (frame-variables frame)))
  (set-cdr! frame (cons val (frame-values frame))))
	    
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many argments supplied" var vals)
	  (error "Too few argments supplied" var vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) 
	     (env-loop (enclosing-environment env)))
;;	     (enclosing-environment env))
	    ((eq? var (car vars)) (car vals))
	    (else
	     (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))
		 
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else
	     (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (add-binding-frame! var val frame))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else
	     (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))


(define (setup-environment)
  (let ((initial-env
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
	(list 'cdr cdr)
	(list 'cons cons)
	(list 'null? null?)
	(list '= =)
	(list 'eq? eq?)
	(list '+ +)
	(list '* *)
	(list '- -)
	(list '/ /)
	;; ...
	))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))



(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
		     (procedure-parameters object)
		     (procedure-body object)
		     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))

