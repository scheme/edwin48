;;;
;;; Macro used to exporting edwin-specific bindings.
;;;
;;; (edwin:export (variable a)
;;;               (command  b)
;;;               (mode     c))
;;; This is equivalent to
;;;
;;; (export edwin-variable$a
;;;         edwin-command$b
;;;         edwin-mode$c)
;;;
(define-syntax edwin:export
  (lambda (form rename compare)
    `(,(rename 'export)
      ,@(apply append
               (map (lambda (specifier)
                      (let ((category (car specifier))
                            (names    (cdr specifier)))
                        (map (lambda (name)
                               (string->symbol
                                (apply string-append
                                       (map symbol->string
                                            `(edwin - ,category $ ,name)))))
                             names)))
                    (cdr form)))))
  (export))
