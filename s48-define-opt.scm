;;; -*- Mode: Scheme; scheme48-package: define-opt -*-
;;;   (open scheme srfi-1 let-opt)
;;;   (for-syntax (open scheme let-opt (subset signals (syntax-error)) srfi-1))

(define-syntax define*
  (lambda (form rename compare)
    (let* ((signature      (cadr form))
	   (body           (cddr form))
	   (name           (car signature))
	   (arguments      (cdr signature))
	   (required       (lambda (args) (take-while symbol? args)))
	   (optional       (lambda (args) (drop-while symbol? args)))
	   (required-args  (required arguments))
	   (optional-args  (optional arguments))
	   (args           (rename 'args))
	   (%define        (rename 'define))
	   (%let-optionals (rename 'let-optionals)))
      (cond
       ((null? optional-args)
	`(,%define (,name ,@required-args) ,@body))
       ((not (every pair? optional-args))
	(syntax-error "all required arguments must come before optional arguments"))
       (else
	`(,%define (,name ,@required-args . ,args)
		   (,%let-optionals ,args ,optional-args
				    ,@body)))))))