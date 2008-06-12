(define (command-name->scheme-name name)
  (symbol-append 'edwin-command$ name))

(define (variable-name->scheme-name name)
  (symbol-append 'edwin-variable$ name))

(define (mode-name->scheme-name name)
  (symbol-append 'edwin-mode$ name))

(define (list-ref/default list index default-value)
  (if (> (length list) index)
      (list-ref list index)
      default-value))

(define (expand-variable-definition buffer-local?)
  (lambda (form rename compare)
    (if (not (<= (length form) 6))
        (syntax-error "DEFINE-VARIABLE name description value test normalization"))
    (let* ((%define        (rename 'define))
           (%make-variable (rename 'make-variable))
           (name           (list-ref form 1))
           (scheme-name    (variable-name->scheme-name name))
           (description    (list-ref/default form 2 #f))
           (value          (list-ref/default form 3 #f))
           (test           (list-ref/default form 4 #f))
           (normalization  (list-ref/default form 5 #f)))
      `(,%define ,scheme-name
                (,%make-variable ',name ,description ,value
                                 ,buffer-local? ,test ,normalization)))))

(define (expand-variable-assignment form generator)
  (if (not (<= (length form) 4))
      (syntax-error "ill-formed syntax" form)
      (generator (list-ref form 1)
                 (list-ref/default form 2 #f)
                 (list-ref/default form 3 #f))))
