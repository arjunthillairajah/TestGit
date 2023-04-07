#lang racket

; Tiger Tian
; Felix Huang
;Arjun Thillairajah

; Current Continuation order:
; return break
; ------------
; next return break continue throw

(provide (all-defined-out)) ; used to run a test script on the interpreter
(require "simpleParser.rkt")
; ============================================================
; Main Functions
; interpret - takes a filename, calls parser with filename,
;             evaulates the syntax tree returned by the parser,
;             returns the proper value.
(define interpret
  (lambda (filename)
    (evaluate (parser filename) init-state (lambda (v) v))))

; evaluate - takes syntax tree, state, and return callback,
;            returns the proper value
(define evaluate
  (lambda (tree state return)
    (Mstate (car tree) state (lambda (s) (evaluate (cdr tree) s return)) return default-break default-continue default-throw)))

; ============================================================
; Abstraction Definitions

; Default Continuation Functions

(define default-return
  (lambda (v) v))

(define default-break
  (lambda (s)
    (error 'ScopeError "Break statement in global scope")))

(define default-continue
  (lambda (s)
    (error 'ScopeError "Continue statement in global scope")))

(define default-throw
  (lambda (s e)
    (error 'UncaughtException (format "~v" e))))

; State Abstractions
(define init-layer '(() ()))
(define init-state (list init-layer))
(define top-layer car)
(define vars-state car)
(define vals-state cadr)
(define add-layer (lambda (state) (cons init-layer state)))
(define remove-layer cdr)
(define assemble-layer (lambda (varlist vallist) (cons varlist (cons vallist '()))))

; Statement Abstractions
(define condition cadr)
(define body caddr)
(define else-body cadddr)
(define else-exists? (lambda (expr) (pair? (cdddr expr))))
(define try-block cadr)
(define catch-block caddr)
(define finally-block cadddr)

; Expression Abstractions
(define op car)
(define left cadr)
(define right caddr)
(define right-exists? (lambda (expr) (pair? (cddr expr))))

; ============================================================
; State Functions

; Mstate (expr, state) - Determines the type of the expression, calls
;                            appropriate state evaluation function
(define Mstate
  (lambda (expr state next return break continue throw)
    (cond
      [(null? expr)                 (next state)] ; handle case of empty finally block
      [(equal? (op expr) 'return)   (state-return expr state return)]
      [(equal? (op expr) 'var)      (state-decl expr state next)]
      [(equal? (op expr) '=)        (state-assign expr state next)]
      [(equal? (op expr) 'if)       (state-if expr state next return break continue throw)]
      [(equal? (op expr) 'while)    (state-while expr state next return throw)]
      [(equal? (op expr) 'break)    (break state)]
      [(equal? (op expr) 'continue) (continue state)]
      [(equal? (op expr) 'throw)    (throw state (left expr))]
      [(equal? (op expr) 'begin)    (state-block (cdr expr) state next return break continue throw)]
      [(equal? (op expr) 'try)      (state-try expr state next return break continue throw)]
      [(equal? (op expr) 'catch)    (state-block (body expr) state next return break continue throw)]
      [(equal? (op expr) 'finally)  (state-block (left expr) state next return break continue throw)]
      [else                         (error "Unidentified statement encountered: " expr)])))

; state-return (expr, state, return) - returns value to be returned
(define state-return
  (lambda (expr state return)
    (Mvalue (left expr) state (lambda (v)
        (cond
          [(eq? v #t) (return 'true)]
          [(eq? v #f) (return 'false)]
          [else       (return v)])))))

; state-block (expr, state) - returns state after block execution
(define state-block
  (lambda (expr state next return break continue throw)
    (state-block-helper expr (add-layer state) (lambda (s) (next (remove-layer s)))
                                               return
                                               (lambda (s) (break (remove-layer s)))
                                               (lambda (s) (continue (remove-layer s)))
                                               (lambda (s e) (throw (remove-layer s) e)))))

(define state-block-helper
  (lambda (expr state next return break continue throw)
    (if (or (null? expr) (null? (car expr)))
        (next state) ; no lines remain in block
        (Mstate (car expr) state (lambda (s) ; evaluates the line in (cdr expr)
              (state-block-helper (cdr expr) s next return break continue throw)) ; next calls state-block on the remaining lines in the block
                    return break continue throw)))) ; other continuations remain the same

; state-try
(define state-try
  (lambda (expr state next return break continue throw)
    (let* ([new-break (lambda (s) (Mstate (finally-block expr) s break return break continue throw))] ; new break continuation for try block
           [finally-cont (lambda (s) (Mstate (finally-block expr) s next return break continue throw))] ; finally continuation
           [new-throw (lambda (s e) (Mstate (finally-block expr) s (lambda (s2) (throw s2 e)) return break continue throw))] ; exception in catch block, or exception in try block and no existing catch block
           [this-throw (lambda (s e) (this-throw-helper (catch-block expr) s finally-cont return new-break continue new-throw e))]) ; exception in try block
      (state-block (try-block expr) state finally-cont return new-break continue this-throw))))

; this-throw-helper - handles cases for empty/non-empty catch blocks
(define this-throw-helper
  (lambda (expr state next return break continue throw exception)
    (if (null? expr)
        (throw state exception) ; throw continuation here would be new-throw in state-try
        (Mstate expr (add-binding (caadr expr) exception state return) next return break continue throw))))

; state-decl (expr, state) - returns state with declared variable bound to a value if specified or void otherwise
(define state-decl
  (lambda (expr state next)
    (if (right-exists? expr)
        (Mvalue (right expr) state (lambda (v) ; Get value to bind
                (state-expr (right expr) state (lambda (s) ; Get state after evaluating expression
                (add-binding (left expr) v s next)))))
        (add-binding (left expr) (void) state next)))) ; no specified value

; state-assign (expr, state)
(define state-assign
  (lambda (expr state next)
    (Mvalue (right expr) state (lambda (v) ; Get new value to bind v
        (state-expr (right expr) state (lambda (s) ; Get state after calculating value s
        (cond
          [(eq? v #t) (replace-binding (left expr) 'true s next)]
          [(eq? v #f) (replace-binding (left expr) 'false s next)]
          [else       (replace-binding (left expr) v s next)])))))))
    

;(define state-assign
;  (lambda (expr state next)
;    (if (not (declared? (left expr) (vars-state state)))
;        (error 'AssignmentError "Variable has not been declared.")
;        (Mvalue (right expr) state (lambda (v) ; Get new value to bind v
;            (state-expr (right expr) state (lambda (s1) ; Get state after calculating value s1
;            (remove-binding (left expr) s1 (lambda (s2) ; Get state with previous binding removed s2
;            (cond
;              [(eq? v #t) (add-binding (left expr) 'true s2 next)]
;              [(eq? v #f) (add-binding (left expr) 'false s2 next)]
;              [else       (add-binding (left expr) v s2 next)]))))))))))

; state-if (expr, state)
(define state-if
  (lambda (expr state next return break continue throw)
    (Mbool (condition expr) state (lambda (v) (state-expr (condition expr) state (lambda (s) ; Get value of condition v1 and state after condition v2
        (cond
          [ v                 (Mstate (body expr) s next return break continue throw)]
          [(else-exists? expr) (Mstate (else-body expr) s next return break continue throw)]
          [else                (next s)])))))))

; state-while (expr, state)
(define state-while
  (lambda (expr state next return throw)
    (Mbool (condition expr) state (lambda (v) (state-expr (condition expr) state (lambda (s1) ; Get value of condition v1 and state after condition v2
        (if v
            (Mstate (body expr) s1 (lambda (s2) (state-while expr s2 next return throw)) return next (lambda (s2) (state-while expr s2 next return throw)) throw)
            (next s1))))))))

; state-expr (expr, state) - returns the state after evaluating an expression
;                            handles side effects and nested assign statements
(define state-expr
  (lambda (expr state next)
    (cond
      [(or (number? expr) (boolean? expr) (symbol? expr)) (next state)] ; handles case where terminal is given instead of expression
      [(eq? (op expr) '=)                                 (state-assign expr state next)]
      [else                                               (state-expr (left expr) state ; Get state after evaluating left operand
                                                                      (lambda (v) (if (right-exists? expr) ; If right operand exists
                                                                                      (state-expr (right expr) v next) ; Get state after evaluating right operand
                                                                                      (next v))))]))) ; Return state after left operand otherwise

; ============================================================
; Helper Functions

; declared? (var varlist) - checks if a variable has been declared in the variable list
(define declared?
  (lambda (var state)
    (if (null? state)
        #f
        (declared?-helper var (vars-state (top-layer state)) (lambda () (declared? var (cdr state)))))))

(define declared?-helper
  (lambda (var varlist next)
   (cond
     [(null? varlist) (next)]
     [(eq? var (car varlist)) #t]
     [else (declared?-helper var (cdr varlist) next)])))

; isint? - checks if expression evaluates to a numeric value
(define isint?
  (lambda (expr)
    (or (eq? (op expr) '+)
        (eq? (op expr) '-)
        (eq? (op expr) '*)
        (eq? (op expr) '/)
        (eq? (op expr) '%))))

; isbool? - checks if expression evaluates to a boolean value
(define isbool?
  (lambda (expr)
    (or (boolean? expr)
        (eq? expr 'true)
        (eq? expr 'false)
        (eq? (op expr) '==)
        (eq? (op expr) '!=)
        (eq? (op expr) '<)
        (eq? (op expr) '>)
        (eq? (op expr) '<=)
        (eq? (op expr) '>=)
        (eq? (op expr) '&&)
        (eq? (op expr) '||)
        (eq? (op expr) '!))))

; add-binding (var, val, state) - returns state with target binding added
(define add-binding
  (lambda (var val state return)
    (if (declared? var state)
        (return (error 'DeclarationError "Variable has already been declared."))
        (return (cons (assemble-layer (cons var (vars-state (top-layer state))) (cons val (vals-state (top-layer state)))) (remove-layer state))))))

; remove-binding (var, state) - returns state with target binding removed
(define remove-binding
  (lambda (var state return)
    (remove-binding-helper var (vars-state state) (vals-state state) return)))

(define remove-binding-helper
  (lambda (var varlist vallist return)
    (cond
      [(null? varlist) (return (cons varlist (cons vallist '())))] ; varlist exhausted, var not found -- return original varlist and vallist
      [(eq? var (car varlist)) (return (cons (cdr varlist) (cons (cdr vallist) '())))] ; var found
      ; var not found - build out variable and value lists, and output both as state
      [else (remove-binding-helper var (cdr varlist) (cdr vallist) (lambda (v) (return (cons (cons (car varlist) (car v)) ; build out variable list
                                                                                                 (cons (cons (car vallist) (cadr v)) '())))))]))) ; build out value list


(define replace-binding
  (lambda (var val state return)
    (replace-binding-state-helper var val state return)))

(define replace-binding-state-helper
  (lambda (var val state return)
    (if (null? state)
        (error 'DeclarationError "Variable not found.")
        (replace-binding-layer-helper var val (vars-state (car state)) (vals-state (car state))
                                  (lambda (l1 l2) (return (cons (assemble-layer l1 l2) (cdr state)))) ; return the state with the altered layer
                                  (lambda () (replace-binding var val (cdr state) (lambda (s) (return (cons (car state) s))))))))) ; continue searching the next layer, attach skipped layer to result

(define replace-binding-layer-helper
  (lambda (var val varlist vallist return next) ; return --> value found/value replaced/state exhausted; next --> layer exhausted, proceed to next layer of state
    (cond
      [(null? varlist) (next)] ; varlist exhausted, var not found in scope
      [(eq? var (car varlist)) (return varlist (cons val (cdr vallist)))] ; variable found --> return layer with altered value
      [else (replace-binding-layer-helper var val (cdr varlist) (cdr vallist) (lambda (l1 l2) (return (cons (car varlist) l1)(cons (car vallist) l2))) next)])))

; get-binding (var, state) - returns the value bound to the target variable
(define get-binding
  (lambda (var state return)
    (get-binding-state-helper var state (lambda (v) (cond
                                                      [(void? v) (error 'GetError "Variable used before assigned.")]
                                                      [(eq? v 'true) (return #t)]
                                                      [(eq? v 'false) (return #f)]
                                                      [else (return v)])))))

(define get-binding-state-helper
  (lambda (var state return)
    (if (null? state)
        (error 'DeclarationError "Variable not found.")
        (get-binding-layer-helper var (vars-state (top-layer state)) (vals-state (top-layer state)) return
                                  (lambda () (get-binding var (cdr state) return))))))

(define get-binding-layer-helper
  (lambda (var varlist vallist return next)
    (cond
      [(null? varlist) (next)] ; varlist exhausted, var not found in scope
      [(eq? var (car varlist)) (return (car vallist))]
      [else (get-binding-layer-helper var (cdr varlist) (cdr vallist) return next)])))

;(define get-binding
;  (lambda (var state return)
;    (get-binding-helper var (vars-state state) (vals-state state) return)))

;(define get-binding-helper
;  (lambda (var varlist vallist return)
;    (cond
;      [(null? varlist)                                     (return (error 'DeclarationError "Variable not found."))]
;      [(and (eq? var (car varlist)) (void? (car vallist))) (error 'GetError "Variable used before assigned.")]
;      [(eq? var (car varlist))                             (return (car vallist))]
;      [else                                                (get-binding-helper var (cdr varlist) (cdr vallist) return)])))

; ============================================================
; Value Functions

; Mvalue (expr, state) - determines expression type and calls Mint, Mbool, or get-binding accordingly
(define Mvalue
  (lambda (expr state return)
    (cond
      [(number? expr)     (Mint expr state return)]
      [(boolean? expr)    (Mbool expr state return)]
      [(eq? expr 'true)   (Mbool expr state return)]
      [(eq? expr 'false)  (Mbool expr state return)]
      [(symbol? expr)     (get-binding expr state return)]
      [(eq? (op expr) '=) (Mvalue (right expr) state return)]
      [(isint? expr)      (Mint expr state return)]
      [(isbool? expr)     (Mbool expr state return)]
      [else               (return (error 'unknownOp "Bad Operator"))])))

; Mint (expr, state) - evaluates numeric expression, returns proper value
(define Mint
  (lambda (expr state return)
    (cond
      [(number? expr)     (return expr)]
      [(symbol? expr)     (get-binding expr state return)]
      [(eq? (op expr) '=) (Mvalue (right expr) state return)]
      [(eq? (op expr) '+) (Mint (left expr) state (lambda (v1) (state-expr (left expr) state (lambda (s) (Mint (right expr) s (lambda (v2) (return (+ v1 v2))))))))]
      ; There is a right operand - this is a subtraction
      [(and (eq? (op expr) '-) (right-exists? expr)) (Mint (left expr) state (lambda (v1) (state-expr (left expr) state (lambda (s) (Mint (right expr) s (lambda (v2) (return (- v1 v2))))))))]
      ; There is no right operand - this is a unary operation
      [(eq? (op expr) '-) (Mint (left expr) state (lambda (v) (return (- v))))]
      [(eq? (op expr) '*) (Mint (left expr) state (lambda (v1) (state-expr (left expr) state (lambda (s) (Mint (right expr) s (lambda (v2) (return (* v1 v2))))))))]
      [(eq? (op expr) '/) (Mint (left expr) state (lambda (v1) (state-expr (left expr) state (lambda (s) (Mint (right expr) s (lambda (v2) (return (quotient v1 v2))))))))]
      [(eq? (op expr) '%) (Mint (left expr) state (lambda (v1) (state-expr (left expr) state (lambda (s) (Mint (right expr) s (lambda (v2) (return (remainder v1 v2))))))))]
      [else               (return (error 'unknownOp "Bad Operator"))])))

; Mbool (expr, state) - evaluates boolean expression, returns proper value
(define Mbool
  (lambda (expr state return)
    (cond
      [(boolean? expr)     (return expr)]
      [(eq? expr 'true)    (return #t)]
      [(eq? expr 'false)   (return #f)]
      [(symbol? expr)      (get-binding expr state return)]
      [(eq? (op expr) '=)  (Mvalue (right expr) state return)]
      [(eq? (op expr) '==) (Mvalue (left expr) state (lambda (v1) (state-expr (left expr) state (lambda (s) (Mvalue (right expr) s (lambda (v2) (return (eq? v1 v2))))))))]
      [(eq? (op expr) '!=) (Mvalue (left expr) state (lambda (v1) (state-expr (left expr) state (lambda (s) (Mvalue (right expr) s (lambda (v2) (return (not (eq? v1 v2)))))))))]
      [(eq? (op expr) '<)  (Mint (left expr) state (lambda (v1) (state-expr (left expr) state (lambda (s) (Mint (right expr) s (lambda (v2) (return (< v1 v2))))))))]
      [(eq? (op expr) '>)  (Mint (left expr) state (lambda (v1) (state-expr (left expr) state (lambda (s) (Mint (right expr) s (lambda (v2) (return (> v1 v2))))))))]
      [(eq? (op expr) '<=) (Mint (left expr) state (lambda (v1) (state-expr (left expr) state (lambda (s) (Mint (right expr) s (lambda (v2) (return (<= v1 v2))))))))]
      [(eq? (op expr) '>=) (Mint (left expr) state (lambda (v1) (state-expr (left expr) state (lambda (s) (Mint (right expr) s (lambda (v2) (return (>= v1 v2))))))))]
      [(eq? (op expr) '&&) (Mbool (left expr) state (lambda (v1) (state-expr (left expr) state (lambda (s) (Mbool (right expr) s (lambda (v2) (return (and v1 v2))))))))]
      [(eq? (op expr) '||) (Mbool (left expr) state (lambda (v1) (state-expr (left expr) state (lambda (s) (Mbool (right expr) s (lambda (v2) (return (or v1 v2))))))))]
      [(eq? (op expr) '!)  (Mbool (left expr) state (lambda (v) (return (not v))))]
      [else                (return (error 'unknownOp "Bad Operator"))])))