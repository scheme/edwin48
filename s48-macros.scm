;;; -*- Mode: Scheme;
;;;
;;; s48-macros.scm:
;;; Port macros.scm from syntactic closures to explicit-renaming macros
;;;

(define-syntax define-command
  (lambda (form rename compare)
    (if (not (= (length form) 5))
        (syntax-error "DEFINE-COMMAND name description interactive procedure")
        (let* ((name          (list-ref form 1))
               (description   (list-ref form 2))
               (interactive   (list-ref form 3))
               (procedure     (list-ref form 4))
               (scheme-name   (command-name->scheme-name name))
               (%define       (rename 'define))
               (%make-command (rename 'make-command)))
          `(,%define ,scheme-name
                     (,%make-command ',name ,description ',interactive ,procedure))))))

(define-syntax ref-command-object
  (lambda (form rename compare)
    (if (not (and (= (length form) 2)
                  (symbol? (list-ref form 1))))
	(syntax-error "REF-COMMAND-OBJECT name")
        (command-name->scheme-name (list-ref form 1)))))

(define-syntax ref-command
  (lambda (form rename compare)
    (if (not (= (length form) 2))
	(syntax-error "REF-COMMAND name")
        (let* ((%command-procedure  (rename 'command-procedure))
               (%ref-command-object (rename 'ref-command-object))
               (command-name        (list-ref form 1))
               (command             `(,%ref-command-object ,command-name)))
          `(,%command-procedure ,command)))))

(define-syntax define-variable            (expand-variable-definition #f))
(define-syntax define-variable-per-buffer (expand-variable-definition #t))

(define-syntax ref-variable-object
  (lambda (form rename compare)
    (if (not (and (= (length form) 2)
                  (symbol? (list-ref form 1))))
	(syntax-error "REF-VARIABLE-OBJECT name")
        (variable-name->scheme-name (list-ref form 1)))))

(define-syntax ref-variable
  (lambda (form rename compare)
    (let* ((%ref-variable-object
            (rename 'ref-variable-object))
           (name     (list-ref form 1))
           (variable `(,%ref-variable-object ,name)))
      (case (length form)
	((2)
	 (let* ((%value   (rename 'variable-value)))
	   `(,%value ,variable)))
	((3)
	 (let* ((%value   (rename 'variable-local-value))
		(buffer   (list-ref form 2)))
	   `(,%value ,buffer ,variable)))
	(else (syntax-error "REF-VARIABLE [buffer] variable "))))))

(define-syntax set-variable!
  (lambda (form rename compare)
    (expand-variable-assignment
     form
     (lambda (name value buffer)
       (let* ((%ref-variable-object
               (rename 'ref-variable-object))
              (%set-variable-local-value!
               (rename 'set-variable-local-value!))
              (%set-variable-value!
               (rename 'set-variable-value!))
              (variable `(,%ref-variable-object ,name)))
         (if buffer
             `(,%set-variable-local-value! ,buffer ,variable, value)
             `(,%set-variable-value! ,variable ,value)))))))

(define-syntax local-set-variable!
  (lambda (form rename compare)
    (expand-variable-assignment
     form
     (lambda (name value buffer)
       (let* ((%ref-variable-object
               (rename 'ref-variable-object))
              (%define-variable-local-value!
               (rename 'define-variable-local-value!))
              (%current-buffer
               (rename 'current-buffer))
              (variable `(,%ref-variable-object ,name)))
         `(,define-variable-local-value!
            (or ,buffer `(,%current-buffer))
            ,name ,value))))))

(define-syntax ref-mode-object
  (lambda (form rename compare)
    (if (not (and (= (length form) 2)
                  (symbol? (list-ref form 1))))
	(syntax-error "REF-MODE-OBJECT name")
        (mode-name->scheme-name (list-ref form 1)))))
