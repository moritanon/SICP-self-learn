(use gauche.test)
(test-start "metalinuistic.scm")

;;(require "./metalinguistic.scm")
(load "./metalinguistic.scm")

(test* "eval-1" 1 (eval '1 '()))
(test* "self-evaluating1" #t (self-evaluating? 1))
(test* "self-evaluating2" #t (self-evaluating? "abc"))
(test* "self-evaluating3" #f (self-evaluating? '()))
(test* "self-evaluating4" #f (self-evaluating? 'a))

(test* "variable? 1"  #t (variable? 'a))
(test* "variable? 2" #f (variable? 2))

(test* "tagged-list? 1" #t (tagged-list? '(lambda (a b) (a b)) 'lambda))
(test* "tagged-list? 2" #f (tagged-list? 123 'abc))
(test* "quoted? 1" #f (quoted? '(a b c)))
(test* "quoted? 2" #t (quoted? '(quote a b c)))

(test* "text-of-quotation 1" '(a b c) (text-of-quotation '(quote (a b c))))

(test* "assignment? 1" #t (assignment? '(set! a 10)))
(test* "assignment? 2" #f (assignment? '(a b c)))

(test* "assignment-variable 1" 'a (assignment-variable '(set! a 10)))
(test* "assignment-value 1" 10 (assignment-value '(set! a 10)))

(test* "definition? 1" #t (definition? '(define a 10)))
(test* "definition? 2" #f (definition? '(a b c)))

(test* "definition-variable 1" 'a (definition-variable '(define a 10)))
(test* "definition-variable 2" 'a (definition-variable '(define (a x) (* x x))))
(test* "definition-value 1" 10 (definition-value '(define a 10)))

(test* "make-definitaion 1" '(define a 10) (make-definition 'a 10))

(test* "lambda? 1" #t (lambda? '(lambda (a b c) (+ a b c))))
(test* "lambda-parameters 1" '(a b c) (lambda-parameters '(lambda (a b c) (+ a b c))))
(test* "lambda-body 1" '((+ a b c)) 
       (lambda-body '(lambda (a b c) (+ a b c))))
(test* "lambda-body 2" '(1) (lambda-body '(lambda (a b c) 1)))
(test* "make-lambda 1" '(lambda (a b c) (+ a b c))
       (make-lambda '(a b c) '((+ a b c))))
(test* "make-lambda 2" '(lambda (a b c) . 1)
       (make-lambda '(a b c) 1))

(test* "definition-value 1" '(lambda (x) (* x x))
       (definition-value '(define (a x) (* x x))))



(test* "if? 1" #t (if? '(if a b c)))
(test* "if-predicate 1" 'a (if-predicate '(if  a b c)))
(test* "if-consequent 1" 'b (if-consequent '(if a b c)))
(test* "if-alternative 1" 'c (if-alternative '(if a b c)))

(test* "make-if 1" '(if a b c) (make-if 'a 'b 'c))

(test* "begin? exp 1" #t (begin? '(begin a b)))
(test* "begin-actions 1" '(a b) (begin-actions '(begin a b)))
(test* "last-exp? seq" #f (last-exp? '(a b)))
(test* "last-exp? seq" #t (last-exp? '(a)))

(test* "first-exp 1" 'a (first-exp '(a b)))

(test* "rest-exp 1" '(b) (rest-exp '(a b)))

(test* "sequence->exp 1" '() (sequence->exp '()))
(test* "sequence->exp 2" 'a (sequence->exp '(a)))
(test* "sequence->exp 3" '(begin a b) (sequence->exp '(a b)))
(test* "make-begin 1"  '(begin a b) (make-begin '(a b)))

(test* "application? 1" #t (application? '(a b)))
(test* "application? 2" #f (application? 1))


(test* "operator 1" 'a (operator '(a b)))
(test* "operands 1" '(b) (operands '(a b)))

(test* "no-operands? 1" #f (no-operands? '(1 2)))
(test* "no-operands? 2" #t (no-operands? '()))

(test* "first-operand 1" 'a (first-operand '(a b)))
(test* "rest-operands 1" '(b) (rest-operands '(a b)))

;; cond
(test* "cond? 1" #t (cond? '(cond ((= a b) #t))))
(test* "cond? 2" #f (cond? '(if a b c)))

;; (cond (a x)
;;       (b y)
;;       (else z))  
(test* "cond-clauses 1" '((a x) (b y) (else z))
       (cond-clauses '(cond (a x) (b y) (else z))))

(test* "cond-else-clause? 1" #t (cond-else-clause? '(else z)))
(test* "cond-else-clause? 2" #f (cond-else-clause? '(a x)))

(test* "cond-predicate 1" 'a (cond-predicate '(a x)))
(test* "cond-actions 1" '(x) (cond-actions '(a x)))

(test* "cond->if 1" '(if a x y)
       (cond->if '(cond (a x) (else y))))
(test* "cond->if 2" '(if a x (if b y z))
       (cond->if '(cond (a x) (b y) (else z))))
       
(test* "expand-clauses 1" '(if a x y)
       (expand-clauses '((a x) (else y))))

(test* "expand-clauses 2" '(if a x (if b y z))
       (expand-clauses '((a x) (b y) (else z))))

;;(test-error "expand-clauses 3" (lambda () ((expand-clauses '((a x) (else y) (b z))))))
;; error関数形式のエラーの補足の仕方  -- 課題
;;(test* "" (test-error) (load "./nosuchfile"))
;;(test* "" (test-error <error>) (load "./nosuchfile"))

;; => 以下のように行えばよいらしい
(test* "expand-clauses 3" (test-error) (expand-clauses '((a x) (else y) (b z))))

(test* "expand-clauses 4" '(if a x false) (expand-clauses '((a x))))
(test* "expand-clauses 5" '(if a (begin x y) z) 
       (expand-clauses '((a x y) (else z))))

(test* "and? 1" #t (and? '(and #t)))
(test* "or? 1" #t (or? '(or 0)))

(test* "and-operands 1" '(a b) (and-operands '(and a b)))
(test* "or-operands 1" '(a b) (or-operands '(or a b)))

;; let
(test* "let? 1" #t (let? '(let ((a 1) (b 2)) body)))
(test* "let-params 1" '((a 1) (b 2)) (let-params '(let ((a 1) (b 2)) body)))
(test* "let-body 1" 'body (let-body '(let ((a 1) (b 2)) body)))
(test* "first-let-param 1" '(a 1) (first-let-param '((a 1) (b 2))))
(test* "rest-let-param 1" '((b 2)) (rest-let-param '((a 1) (b 2))))
(test* "let-param-variable 1" 'a (let-param-variable '(a 1)))
(test* "let-param-value 1" 1 (let-param-value '(a 1)))


(test* "let-params->var-val-list 1" 
       (call-with-values (lambda () 
			   (values '(a) '(1))) list)
       (call-with-values (lambda () 
			   (let-params->var-val-list '((a 1)))) list))
(test* "let-params->var-val-list 1.5"
       (receive all
		(values '(a) '(1)) 
		all)
       (receive all
		(let-params->var-val-list '((a 1))) 
		all))
		


(test* "let-params->var-val-list 1" '(a)
       (values-ref (values '(a) '(2)) 0))
       
(test* "let-params->var-val-list 2" 
       (call-with-values (lambda () (values '(a b) '(1 2))) list)
       (call-with-values (lambda () (let-params->var-val-list '((a 1) (b 2)))) list))

(test* "let->combination 1" 
       '((lambda (a) . body) 1)
       (let->combination '(let ((a 1)) body)))
(test* "let->combination 2"
       '((lambda (a b) . body) 1 2)
       (let->combination '(let ((a 1) (b 2)) body)))

(test* "let->combination 3"
       '((lambda () . body))
       (let->combination '(let () body)))

(test* "make-let 1"
       '(let (a 1) body)
       (make-let 'a 1 'body))

(test* "let*->nested-lets 1"
       '(let ((a 1))
	  (let ((b 1))
	    body))
       (let*->nested-lets '(let* ((a 1) (b 1)) body)))
(test* "let*0>nested-lets 2"
       '(let ((a 1)) body)
       (let*->nested-lets '(let* ((a 1)) body))) 
       

(test* "make-procedure 1" '(procedure (a b c) (+ a b c) env)
       (make-procedure '(a b c) '(+ a b c) 'env))

(test* "compound-procedure? 1" #t 
       (compound-procedure? (make-procedure '(a b c) '(+ a b c) 'env)))


(test* "make-frame 1" '((a b c) . (1 2 3))
       (make-frame '(a b c) '(1 2 3)))
(test* "frame-variables 1" '(a b c)
       ;;(frame-variables '((a b c) 1 2 3)))
       (frame-variables (make-frame '(a b c) '(1 2 3))))

(test* "frame-values 1" '(1 2 3) 
       (frame-values (make-frame '(a b c) '(1 2 3))))

(test* "add-binding-frame! 1" '(a b c)
       (let ((frame (make-frame '(b c) '(1 2))))
	 (frame-variables (begin 
			    (add-binding-frame! 'a 1 frame)
			    frame))))
						
(test* "extend-environment 1"
       '(((a b) . (1 2)) . ((x y) . (0 1)))
       (extend-environment '(a b) '(1 2) '((x y) . (0 1))))
			
(test* "extend-environment 2"
       (test-error)
       (extend-environment '(a b c) '(1 2) '()))

(test* "extend-environment 3"
       (test-error)
       (extend-environment '(a b) '(1 2 3) '()))

(test* "first-frame 1" '((x y) 0 1)
       (first-frame 
	(extend-environment '(x y) '(0 1) the-empty-environment)))
(test* "first-frame 2" '((a b) 9 10)
       (first-frame
	(extend-environment 
	 '(a b) '(9 10)
	 (extend-environment '(x y) '(0 1) the-empty-environment))))

(test* "lookup 1" 1
       (lookup-variable-value 
	'y 
	(extend-environment '(x y) '(0 1) the-empty-environment)))

(test* "lookup 2" 0
       (lookup-variable-value
	'x
	(extend-environment 
	 '(a b) '(9 10)
	 (extend-environment '(x y) '(0 1) the-empty-environment))))

(test* "lookup 3" (test-error)
       (lookup-variable-value
	'p
	(extend-environment 
	 '(a b) '(9 10)
	 (extend-environment '(x y) '(0 1) the-empty-environment))))

(define abxy-env
  (extend-environment 
   '(a b) '(9 10)
   (extend-environment '(x y) '(0 1) the-empty-environment)))


(test* "set-variable-value! 1"
       20
       (begin 
	 (set-variable-value! 'b 20 abxy-env)
	 (lookup-variable-value 'b abxy-env)))
(test* "set-variable-value! 2"
       2
       (begin 
	 (set-variable-value! 'y 2 abxy-env)
	 (lookup-variable-value 'y abxy-env)))
(test* "set-variable-value! 1"
       (test-error)
       (set-variable-value! 'p 20 abxy-env))

(test* "define-variable! 1"
       1
       (begin
	 (define-variable! 'a 1 abxy-env)
	 (lookup-variable-value 'a abxy-env)))
(test* "define-variable! 2"
       5
       (begin
	 (define-variable! 'd 5 abxy-env)
	 (lookup-variable-value 'd abxy-env)))




(test-end)

(driver-loop)