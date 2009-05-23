;;; -*- Mode: Scheme; scheme48-package: edwin48:variable -*-
;;;
;;; Edwin variables
;;;

(define-record-type* variable
  (%make-variable)
  (name
   %description
   %value
   buffer-local?
   initial-value
   %default-value
   assignment-daemons
   value-validity-test
   value-normalization))

(define (variable-description variable)
  (let ((desc (variable-%description variable)))
    (if (description? desc)
	desc
	(let ((new (->doc-string (symbol-name (variable-name variable)) desc)))
	  (if new
	      (set-variable-%description! variable new))
	  new))))

(define variable-value variable-%value)
(define variable-default-value variable-%default-value)

(define (variable-name-string variable)
  (editor-name/internal->external (symbol-name (variable-name variable))))

(define* (make-variable name description value buffer-local?
			(test #f) (normalization #f))
  (let* ((sname (symbol-name name))
	 (variable
	  (or (string-table-get editor-variables sname)
	      (let ((variable (%make-variable)))
		(string-table-put! editor-variables sname variable)
		variable))))
    (set-variable-name! variable name)
    (set-variable-%description! variable (doc-string->posn sname description))
    (set-variable-%value! variable value)
    (set-variable-buffer-local?! variable buffer-local?)
    (set-variable-initial-value! variable value)
    (set-variable-%default-value! variable value)
    (set-variable-assignment-daemons! variable '())
    (set-variable-value-validity-test! variable test)
    (set-variable-value-normalization! variable normalization)
    variable))

(define (make-variable-buffer-local! variable)
  (set-variable-buffer-local?! variable #t))

(define (normalize-variable-value variable value)
  (if (and (variable-value-validity-test variable)
	   (not ((variable-value-validity-test variable) value)))
      (editor-error "Invalid value for " (variable-name-string variable)
		    ": " value))
  (if (variable-value-normalization variable)
      ((variable-value-normalization variable) value)
      value))

(define (add-variable-assignment-daemon! variable daemon)
  (let ((daemons (variable-assignment-daemons variable)))
    (if (not (memq daemon daemons))
	(set-variable-assignment-daemons! variable (cons daemon daemons)))))

(define (invoke-variable-assignment-daemons! buffer variable)
  (if within-editor?
      (do ((daemons (variable-assignment-daemons variable) (cdr daemons)))
	  ((null? daemons))
	((car daemons) buffer variable))))

(define editor-variables
  (make-string-table 50))

(define* (name->variable name (if-undefined 'INTERN))
  (or (string-table-get editor-variables (symbol-name name))
      (case if-undefined
	((#F) #f)
	((ERROR) (error "Undefined variable:" name))
	((INTERN) (make-variable name "" #f #f))
	(else (error:bad-range-argument if-undefined 'NAME->VARIABLE)))))

(define (->variable object)
  (if (variable? object)
      object
      (name->variable object)))

(define (variable-permanent-local! variable)
  (hash-table-set! permanent-local-variables variable #t))

(define (variable-permanent-local? variable)
  (hash-table-ref/default permanent-local-variables variable #f))

(define permanent-local-variables
  (make-hash-table eq?))

