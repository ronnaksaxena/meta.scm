;; metacircular interpreter for a subset of scheme


;; An environment is a list of one element.  That element is an association
;; list.  This allows us to add a binding to an environment.

(define call/cc call-with-current-continuation)

;;for generating errors
(define (popl-error . things)
   (display "ERROR: ")
   (for-each display things)
   (newline)
   (*LOOPTOP* 'dontcare))


(define (popl-bind symbol value env)
  (let ((bindings (car env)))
    (set-car! env (cons (list symbol value) bindings)))
  symbol)

(define (popl-get-binding symbol env)
  (assoc symbol (car env)))


;; return the value of a symbol bound in the given environment
(define (popl-env-value symbol env)
  (let ((pr (popl-get-binding symbol env)))
    (if pr
        (cadr pr)
        (popl-error "Symbol not found"))))

;; popl-define: add a binding to the environment
(define (popl-define symbol value env)
  (popl-bind symbol value env)
  symbol)

;; popl-set!  change the value of a binding in an environment

;; pr = (x 7)
;; (set! x 9)


(define (popl-set! symbol value env)
  (let ((pr (popl-get-binding symbol env)))
    (if pr
        (let ((old (cadr pr)))
          (set-car! (cdr pr) value)
          old)
        (popl-error "No binding found for symbol"))))


(define *TOP-ENVIRONMENT*
  (list (list))) ;; totally empty environment

(popl-define '+ + *TOP-ENVIRONMENT*)
(popl-define '- - *TOP-ENVIRONMENT*)
(popl-define '* * *TOP-ENVIRONMENT*)
(popl-define '/ / *TOP-ENVIRONMENT*)
(popl-define '= = *TOP-ENVIRONMENT*)
(popl-define 'cons cons *TOP-ENVIRONMENT*)
(popl-define 'car car *TOP-ENVIRONMENT*)
(popl-define 'cdr cdr *TOP-ENVIRONMENT*)
(popl-define 'eq? eq? *TOP-ENVIRONMENT*)
(popl-define 'equal? equal? *TOP-ENVIRONMENT*)
(popl-define 'null? null? *TOP-ENVIRONMENT*)
(popl-define 'list list *TOP-ENVIRONMENT*)
(popl-define '> > *TOP-ENVIRONMENT*)
(popl-define '< < *TOP-ENVIRONMENT*)
(popl-define 'display display *TOP-ENVIRONMENT*)
(popl-define 'newline newline *TOP-ENVIRONMENT*)



(define (popl-eval-lambda expr env)
   ;; expr is something like:  (lambda (a b c) form1 form2)
   (list 'popl-lambda
         (second expr)
         (cddr expr)
         env))


(define (popl-apply function args)
  ;; function--> (popl-lambda VARS body env)
  ;; verify function is a function.
  ;;checks syntax of args
  (if (not (= (length args) (length (cadr function))))
      (popl-error "Function expected " (length (cadr function)) "arguments, but "
      (length args) " given")
      (let* ((vars (cadr function))
             (body (caddr function))
             (env (cadddr function))
             (newenv (list (car env))) ;; shallow copy.
             (result #!unspecific))
        (for-each
           (lambda (pair) (popl-bind (car pair) (cadr pair) newenv))
                   (zip vars args))  ;; ((x 4) (y 3))
        (for-each
           (lambda (expr) (set! result (popl-eval expr newenv)))
           body)
        result)))


(define (popl-eval-function-call expr env)
  ;; expr--> (function-expr arg1-expr arg2-expr ... argn-expr)
  ;; evaluate all the expressions
  (let ((items (map (lambda (e) (popl-eval e env)) expr)))
    (if (procedure? (car items)) (apply (car items) (cdr items))
                                 (popl-apply (car items) (cdr items)))))

(define (popl-eval-if expr env)
        ;;condition if there is an else statement
  (cond ((= 4 (length expr))
        (let ((condition (popl-eval (cadr expr) env)))
          (if condition
              (popl-eval (caddr expr) env)
              (popl-eval (cadddr expr) env))))
        ;;condition of there is no else statement
        ((= 3 (length expr))
              (let ((condition (popl-eval (cadr expr) env)))
                (if condition
                    (popl-eval (caddr expr) env))))
        ;;otherwise syntax error
        (else (popl-error "invalid syntax"))))


;;helper fn for pople-eval-cond
(define (popl-cond-helper expr env)
  (cond ((null? expr) ())
        ;;check for parts of cond expr
        ((equal? (caar expr) 'else) (popl-eval (cadar expr) env))
        ((popl-eval (caar expr) env) (popl-eval (cadar expr) env))
        (else (popl-cond-helper (cdr expr) env))))

(define (popl-eval-cond expr env)
  (if (> 3 (length expr))
      (popl-error "invalid syntax")
      ;; else does cond loop
      (popl-cond-helper (cdr expr) env)))

;;evaluates let special form
(define (popl-eval-let expr env)
         (let ((varArgs (cadr expr))
               (expers (cddr expr))
               (newenv (list (car env)))
               (result #!unspecific))
               ;;Evaluates and binds all pairs in new environment
               (for-each
                 (lambda (pair)(popl-bind (car pair)(popl-eval (cadr pair) env)newenv))
                 varArgs)
              ;;Sets result to final evaluation of body
               (for-each
                 (lambda (expression) (set! result (popl-eval expression newenv)))
                   expers)
               result))


;;helper fn to change let* to let from class notes 10/08
(define (let*-to-let expr)
 ;;"Given a let*, produce a (possibly nested) let that is the equivalent"
 (let ((bindings (cadr expr)))
   (if (< (length bindings) 2)
     ;;(cons 'let (cdr expr))
     `(let ,@(cdr expr))
     (let* ((firstbinding (car bindings))
            (otherbindings (cdr bindings))
            (innerlet (let*-to-let `(let* ,otherbindings ,@(cddr expr)))))
         `(let (,firstbinding) ,innerlet)))))

;;evaluates let star special form
(define (popl-eval-letStar expr env)
;; break get parts of let expression and evaluate in new environment
(let ((varArgs (cadr expr))
      (expers (cddr expr))
      (newenv (list (car env)))
      (result #!unspecific))
      ;;Evaluates and binds all pairs in new environment
    (for-each (lambda (binding)
                (popl-bind (car binding) (popl-eval (cadr binding) newenv) newenv))
              varArgs)
    (for-each (lambda (body)
                (set! result (popl-eval body newenv))) expers) result))

;;makes everything in expression a list
(define (popl-eval-quote expr env)
    (cond ((null? expr)())
          (else (cons('(cadr expr) (popl-eval-quote (cdr expr) env))))
          ))
;;executes expressions in body until condition is true
(define (body-eval vars step conditionRes body env)
         (if (popl-eval (car conditionRes) env)
         ;;If the condition evaluates as true, returns value and exits loop
           (popl-eval (cadr conditionRes) env)
           (begin
           (for-each
              (lambda (x) (popl-eval (car x) env))
                      (body))
           (for-each
             (lambda (x) (popl-bind (car x) (popl-eval (cadr x) env) env)) (zip vars step))
             ;; ex. ((i (+ 1 i))...)
             ;;continues loop with updated values
            (body-eval vars step conditionRes body env)
           )))

(define (popl-eval-do expr env)
    (if (> 3 (length expr))
         (popl-error "invalid syntax")
           (let* ((bindings (cadr expr))
                  (conditionRes (caddr expr))
                  (body (cdddr expr))
                  (vars
                    (map
                    (lambda (x) (car x))bindings))
                   (init
                      (map
                      (lambda (x) (cadr x))bindings))
                   (step
                      (map
                      (lambda (x) (caddr x))bindings))
                   (newenv (list (car env))) ;; shallow copy.
                  )
                  ;;Binds pairs in new environment and executes body in helper fn
                  (begin
                    (for-each
                     (lambda (pair) (popl-bind (car pair) (popl-eval(cadr pair) env) newenv))
                             (zip vars init))
                  (body-eval vars step conditionRes body newenv)
                  )
              )
        )
)



(define (popl-eval-help expr env)

  (cond ((number? expr) expr)
        ((symbol? expr) (popl-env-value expr env))
        ((pair? expr)
         (cond ((eq? (first expr) 'define)
               (cond ((< (length expr) 3)
                        (popl-error "Ill-formed special form: " expr))
                     ((eq? (second expr) 'bad)
                         (popl-error "Ill-formed special form: " (caddr expr)))
                     (else
                        ;;else if syntax is correct
                        (let ((sym (second expr))
                              (val (popl-eval (third expr) env)))
                          (popl-define sym val env)))))
               ((eq? (first expr) 'quote)
                (second expr))
               ((eq? (first expr) 'lambda)
               ;;(if (< (length expr) 4)
                ;;        (popl-error "Ill-formed special form: " expr)
                (popl-eval-lambda expr env))
               ((eq? (first expr) 'if)
                (popl-eval-if expr env))
               ((eq? (first expr) 'cond)
                (popl-eval-cond expr env))
               ((eq? (first expr) 'let*)
                (popl-eval-letStar expr env))
               ((eq? (first expr) 'let)
                (popl-eval-let expr env))
               ((eq? (first expr) 'quote)
                (popl-eval-quote expr env))
                ((eq? (first expr) 'do)
                 (popl-eval-do expr env))
                 ((eq? (first expr) 'set!)
                  (popl-set! (second expr)(popl-eval (third expr) env) env))
              (else (popl-eval-function-call expr env))))
        (else "I don't know")))


;; A function for writing things for debugging.
;; Takes an indententation level, a string prefix,
;; and other things.

;; Change this to #f before submitting:
(define *ENABLE-DEBUG* #f)
(define (popl-debug-println level prefix . others)
  (if *ENABLE-DEBUG* (begin
    ; indent
    (do ((i 0 (+ i 1)))
        ((= i level))
      (display " "))
    ; write the things
    (display prefix)
    (do ((lst others (cdr lst)))
        ((null? lst))
      (write (car lst)))
    ; newline terminate
    (newline))))

;; This popl-eval is an augmentation that displays what's being
;; evaluated and what's being returned. Hopefully it's helpful
;; for debugging.
(define popl-eval
  (let ((eval-level 0))
    (lambda (expr env)
      (set! eval-level (+ eval-level 1))
      (popl-debug-println eval-level "Evaluating: " expr)
      (let ((result (popl-eval-help expr env)))
        (popl-debug-println eval-level "Returning: " result)
        (set! eval-level (- eval-level 1))
        result))))

;; This popl-repl reimplemented using a do loop body and a helper function

(define (popl-prompt-read)
  (display "H]=> ") (read))

(define *LOOPTOP* #!unspecific)

(define (popl-repl)
  (call/cc (lambda (here) (set! *LOOPTOP* here)))
  (do ((expr (popl-prompt-read) (popl-prompt-read)))
      ((equal? expr '(exit)) "Goodbye")
    (write (popl-eval expr *TOP-ENVIRONMENT*))
    (newline)))





;; end
