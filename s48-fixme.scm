
(define (editor-error . message) (error message))
(define within-editor? #f)

(define (procedure-arity-valid? procedure n) #t)

(define (set-variable-value! variable value)
  (let ((value (normalize-variable-value variable value)))
    (without-interrupts
     ;; Not with-editor-interrupts-disabled as we are not within-editor?
     (lambda ()
       (set-variable-%default-value! variable value)
       (set-variable-%value! variable value)
       (invoke-variable-assignment-daemons! #f variable)))))
