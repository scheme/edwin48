;;; -*- mode: scheme; scheme48-package: (config) -*-

;;;;;; Alternative record type definition macro

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (expand-define-record-type* form rename compare)
  ((call-with-current-continuation
     (lambda (lose)
       (lambda ()
         (let-fluid $lose (lambda (message . irritants)
                            (lose
                             (lambda ()
                               ;; SYNTAX-ERROR is silly in Scheme48.
                               (apply syntax-error
                                      "invalid DEFINE-RECORD-TYPE form"
                                      form
                                      message irritants)
                               form)))
           (lambda ()
             (destructure (( (keyword type-name
                                      (conser-name . conser-args)
                                      other-fields)
                             form))
               (receive (needs-conser-layer? arg-tags vars inits)
                   (compute-vars+inits conser-args other-fields)
                 (let ((real-conser
                        (if needs-conser-layer?
                            (rename (symbol-append '% conser-name))
                            conser-name)))
                   `(,(rename 'begin)
                     (,(rename 'define-record-type) ,type-name
                      ;; Scheme48 convention
                      ,(symbol-append ': type-name)
                      (,real-conser ,@arg-tags)
                      ,(symbol-append type-name '?)
                      ,@(generate-field-specs conser-args
                                              other-fields
                                              type-name))
                     ,@(if needs-conser-layer?
                           `((,(rename 'define) (,conser-name ,@vars)
                              (,real-conser ,@inits)))
                           '()))))))))))))

(define $lose (make-fluid #f))
(define (lose msg . irritants) (apply (fluid $lose) msg irritants))

(define (compute-vars+inits conser-args other-fields)
  (let ((vars (reverse-map
               (lambda (x)
                 (cond ((symbol? x) x)
                       ((and (pair? x)
                             (symbol? (car x))
                             (null? (cdr x)))
                        (car x))
                       (else (lose '(invalid maker argument specifier)
                                   x))))
               conser-args)))
    (let loop ((fields other-fields)
               (needs-conser-layer? #f)
               (arg-tags vars)
               (inits vars))
      (if (null? fields)
          (values needs-conser-layer?
                  (reverse arg-tags)
                  (reverse vars)
                  (reverse inits))
          (let ((field (car fields)))
            (cond ((symbol? field)
                   (loop (cdr fields)
                         needs-conser-layer?
                         arg-tags
                         inits))
                  ((and (pair? field)
                        (symbol? (car field))
                        (pair? (cdr field))
                        (null? (cddr field)))
                   (loop (cdr fields)
                         #t
                         (cons (car field) arg-tags)
                         (cons (cadr field) inits)))
                  (else
                   (lose '(invalid field specifier)
                         field))))))))

(define (reverse-map proc list)
  (let loop ((list list) (tail '()))
    (if (null? list)
        tail
        (loop (cdr list) (cons (proc (car list)) tail)))))

(define (generate-field-specs conser-args other-fields type-name)
  (append (map (lambda (x)
                 (receive (tag set?)
                     (if (pair? x)
                         (values (car x) #t)
                         (values x #f))
                   `(,tag ,(make-field-accessor type-name
                                                tag)
                          ,@(if set?
                                (list (make-field-setter
                                       type-name
                                       tag))
                                '()))))
               conser-args)
          (map (lambda (x)
                 (let ((tag (if (pair? x) (car x) x)))
                   `(,tag ,(make-field-accessor type-name tag)
                          ,(make-field-setter   type-name tag))))
               other-fields)))

(define (make-field-accessor type-name tag)
  (symbol-append type-name '- tag))

(define (make-field-setter type-name tag)
  (symbol-append 'set- type-name '- tag '!))

(define (symbol-append . symbols)
  (string->symbol (apply string-append (map symbol->string symbols))))
