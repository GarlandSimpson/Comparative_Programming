#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

(define *symbol-table* (make-hash))
(define *function-table* (make-hash))
(define *label-table* (make-hash))
(define *variable-table* (make-hash))


(define (symbol-get key)
        (hash-ref *symbol-table* key))
(define (function-get key)
        (hash-ref *function-table* key))
(define (label-get key)
        (hash-ref *label-table* key))
(define (variable-get key)
        (hash-ref *variable-table* key))


(define (symbol-put! key value)
        (hash-set! *symbol-table* key value))
(define (function-put! key value)
        (hash-set! *function-table* key value))
(define (label-put! key value)
        (hash-set! *label-table* key value))
(define (variable-put! key value)
        (hash-set! *variable-table* key value))

(for-each
    (lambda (pair)
            (symbol-put! (car pair) (cadr pair)))
    `(

        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (=       ,=)
        (<       ,<)
        (<=      ,<=)
        (>=      ,>=)
        (>       ,>)
        (<>      ,(lambda (x y) (not (= x y))))
        (+       ,+)
        (-       ,-)
        (*       ,*)
        (/       ,/)
        (^       ,expt)
        (ceil    ,ceiling)
        (exp     ,exp)
        (floor   ,floor)
        (log     ,log)
        (sqrt    ,sqrt)
        (inputcount ,0)
        (sin     ,sin)
        (asin    ,asin)
        (cos     ,cos)
        (acos    ,acos)
        (tan     ,tan)
        (atan    ,atan)
        (abs     ,abs)
        (round   ,round)
     ))

(define (set-labels program)
    (map (lambda (line)
        (when (not (null? line))
        (when (or (= 2 (length line))
        (= 3 (length line)))
        (hash-set! *label-table* (cadr line) (- (car line) 1 ))
         ))) program
    ))

(define (print-statement program expr linenr)
    (map (lambda (expr)
        (display (scan-expr program expr linenr))) expr)
    (newline)
    )

(define (goto-statement program expr)
    (cond ((pair? expr)
            (when (eq? (car expr) 'done) (exit 0))
            (scan-program program (hash-ref *label-table* (car expr))))
            (else (scan-program program (hash-ref *label-table* expr)))
    ))

(define (let-statement program expr linenr)
    (cond 
        ((symbol? (car expr))
          (hash-set! *symbol-table*
            (car expr) (scan-expr program (cadr expr) linenr)))
        ((pair? (car expr))
          (let ((vec (hash-ref *symbol-table* (car (car expr)))))
            (vector-set! vec (exact-floor (hash-ref *symbol-table*
              (cadr (car expr))))
                (exact-floor (scan-expr program (cadr expr) linenr)))
            (hash-set! *symbol-table* (car (car expr)) vec))))
    )

(define (dim-statement program expr)
    (cond 
       ((pair? expr)
        (cond 
          ((symbol? (cadr expr))
            (let ((vec (make-vector
                (+ (exact-floor (hash-ref *symbol-table* (cadr expr))) 1))))
            (symbol-put! (car expr) vec)))
          ((number? (cadr expr))
            (let ((vec (make-vector (+ (exact-floor (cadr expr)) 1))))
            (symbol-put! (car expr) vec)))))
       (not (pair? expr)
          (let ((vec (make-vector (exact-floor (cadr expr0)))))
          (symbol-put! (car expr) vec)))
    ))

(define (if-statement program expr cond-true linenr)
    (cond ((eq? #t (scan-expr program expr linenr))
        (cond
            ((hash-has-key? *label-table* cond-true)
              (goto-statement program cond-true))
            (else (scan-program program (+ linenr 1)))

    ))))

(define (input-statement program expr)
    (let ((x (read)))
        (when (equal? "" x) (exit 0))
        (when (eof-object? x) (exit 0))
        (when (not (number? x)) (display "Error\n") (exit 1))
        (when (= (hash-ref *symbol-table* 'inputcount) 3)
            (hash-set! *symbol-table* 'inputcount 0))
        (when (< x 0) 
            (hash-set! *symbol-table* 'inputcount 0))
        (when (not (equal? x ""))
            (hash-set! *symbol-table* 'inputcount
                (+ (hash-ref *symbol-table* 'inputcount) 1))
        (cond ((> (hash-ref *symbol-table* 'inputcount) 0)
          (hash-set! *symbol-table* (car expr) x)))
        (when (not (null? (cdr expr)))
            (input-statement program (cdr expr))))
    ))  

(define (scan-expr program expr linenr)
    (cond
        ((symbol? expr) (hash-ref *symbol-table* expr))
        ((string? expr) expr)
        ((number? expr) (+ expr 0.0))
        ((eq? (car expr) 'print)
            (print-statement program (cdr expr) linenr))
        ((eq? (car expr) 'let)
            (let-statement program (cdr expr) linenr))
        ((eq? (car expr) 'goto)
            (goto-statement program (cdr expr)))
        ((eq? (car expr) 'if)
            (if-statement program (cadr expr) (cadr (cdr expr)) linenr))
        ((eq? (car expr) 'input)
            (input-statement program (cdr expr)))
        ((eq? (car expr) 'dim)
            (dim-statement program (cadr expr)))
        ((pair? expr)
         (cond
             ((hash-has-key? *symbol-table* (car expr))
              (let ((head (hash-ref *symbol-table* (car expr))))
              (cond 
                  ((procedure? head)
                      (apply head
                          (map (lambda (expr) (scan-expr program expr linenr)) (cdr expr))))
                  ((vector? head)
                      (vector-ref head 
                          (exact-floor (hash-ref *symbol-table* (cadr expr)))))               
                  ((number? head) head))))
                  ((hash-has-key? *label-table* (car expr)) 
                  (goto-statement program expr))
         (else #f)))
     ))


(define (scan-program program linenr)
    (when (< linenr (length program))
        (let ((line (list-ref program linenr)))
            (cond
                ((= (length line) 1)
                    (scan-program program (+ linenr 1)))
                ((= (length line) 2)
                    (scan-expr program (cadr line) linenr)
                    (scan-program program (+ linenr 1)))
                ((= (length line) 3)
                    (scan-expr program (cadr (cdr line)) linenr)
                    (scan-program program (+ linenr 1)))
            )))
            (exit 0)
    )


(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (set-labels program)
              (scan-program program 0))))
              ;; (write-program-by-line sbprogfile program))))

(main (vector->list (current-command-line-arguments)))
