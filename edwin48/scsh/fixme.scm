

(define (editor-error . message) (error message))
(define within-editor? #f)
(define current-editor #f)
(define (editor-halt-update?) #f)

(define (procedure-arity-valid? procedure n) #t)

