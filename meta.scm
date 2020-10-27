;; metacircular interpreter for a subset of scheme


;; An environment is a list of one element.  That element is an association
;; list.  This allows us to add a binding to an environment.

(define (popl-error string)
  (error string))

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
        (set-car! (cdr pr) value)
        (popl-error "No binding found for symbol"))))

(define *TOP-ENVIRONMENT*
  (list (list))) ;; totally empty environment

(popl-define '+ + *TOP-ENVIRONMENT*)
(popl-define '- - *TOP-ENVIRONMENT*)
(popl-define '* * *TOP-ENVIRONMENT*)
(popl-define '/ / *TOP-ENVIRONMENT*)
(popl-define '= = *TOP-ENVIRONMENT*)
(popl-define 'cons cons *TOP-ENVIRONMENT*)

(define (popl-eval-lambda expr env)
   ;; expr is something like:  (lambda (a b c) form1 form2)
   (list 'popl-lambda
         (second expr)
         (cddr expr)
         env))


(define (popl-apply function args)
  ;; function--> (popl-lambda VARS body env)
  ;; verify function is a function.
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
    result))


(define (popl-eval-function-call expr env)
  ;; expr--> (function-expr arg1-expr arg2-expr ... argn-expr)
  ;; evaluate all the expressions
  (let ((items (map (lambda (e) (popl-eval e env)) expr)))
    (if (procedure? (car items)) (apply (car items) (cdr items))
                                 (popl-apply (car items) (cdr items)))))

(define (popl-eval-if expr env)
  ;; you do syntax checking
  (let ((condition (popl-eval (cadr expr) env)))
    (if condition
        (popl-eval (caddr expr) env)
        (popl-eval (cadddr expr) env))))  ;; make sure else clause is present

(define (popl-eval-help expr env)

  (cond ((number? expr) expr)
        ((symbol? expr) (popl-env-value expr env))
        ((pair? expr)
         (cond ((eq? (first expr) 'define)
                (let ((sym (second expr))
                      (val (popl-eval (third expr) env)))
                  (popl-define sym val env)))
               ((eq? (first expr) 'quote)
                (second expr))
               ((eq? (first expr) 'lambda)
                (popl-eval-lambda expr env))
               ((eq? (first expr) 'if)
                (popl-eval-if expr env))
              (else (popl-eval-function-call expr env))))
        (else "I don't know")))


;; A function for writing things for debugging.
;; Takes an indententation level, a string prefix,
;; and other things.

;; Change this to #f before submitting:
(define *ENABLE-DEBUG* #t)
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

(define (popl-repl)
  (do ((expr (popl-prompt-read) (popl-prompt-read)))
      ((equal? expr '(exit)) "Goodbye")
    (write (popl-eval expr *TOP-ENVIRONMENT*))
    (newline)))





;; end
