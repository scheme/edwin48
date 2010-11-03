;;; -*- mode: scheme; scheme48-package: (config) -*-

;;;;;; Apropos: searching for bound names

;;; This code is written by Taylor Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-structure apropos (export apropos apropos-all
                                  with-apropos-verbosity
                                  set-apropos-verbosity!)
  (open scheme
        fluids
        cells
        signals
        features
        packages
        packages-internal
        (subset package-commands-internal (config-package))
        bindings
        locations
        disclosers
        tables
        sorting
        destructuring
        meta-types
        (subset display-conditions (limited-write))
        extended-ports
        )
  (optimize auto-integrate)
  (begin

(define $apropos-verbosity
        (make-fluid (make-cell '(value))))

(define (apropos-verbosity) (fluid-cell-ref $apropos-verbosity))

(define (set-apropos-verbosity! verbosity)
  (guarantee-apropos-verbosity verbosity)
  (fluid-cell-set! $apropos-verbosity verbosity))

(define (with-apropos-verbosity verbosity thunk)
  (guarantee-apropos-verbosity verbosity)
  (let-fluid $apropos-verbosity (make-cell verbosity) thunk))

(define (guarantee-apropos-verbosity verbosity)
  (let loop ((v verbosity))
    (cond ((null? v) #t)
          ((pair? v)
           (let ((x (car v)))
             (if (and (symbol? x)
                      (memq x '(inferred-type
                                exported-type
                                value
                                origin)))
                 (loop (cdr v))
                 (error "invalid APROPOS verbosity specifier" x))))
          (else (error "invalid APROPOS verbosity" verbosity)))))

(define (apropos-all id . config)
  (let ((substring (apropos-of id))
        (config (if (null? config)
                    (config-package)
                    (car config))))
    (call-with-current-tracking-output-port
      (lambda (out)
        (let ((body (lambda (name binding)
                      (if (binding? binding)
                          (let ((loc (binding-place binding)))
                            (if (and (location? loc)
                                     (location-assigned? loc)
                                     (structure? (contents loc)))
                                (*apropos-structure
                                 substring
                                 (contents loc)
                                 out
                                 "Exported by" #f)))))))
          (for-each-definition body config)
          (for-each (lambda (struct)
                      (for-each-export (lambda (name type binding)
                                         (body name binding))
                                       struct))
                    (package-opens config)))))))

(define (apropos id . where)
  (let ((substring (apropos-of id))
        (where (and (not (null? where))
                    (car where))))
    (if (not where)
        (apropos-package substring (interaction-environment))
        (cond ((package?   where) (apropos-package   substring where))
              ((structure? where) (apropos-structure substring where))
              (else
               (apply call-error "invalid APROPOS location argument"
                      apropos id where))))))

(define (apropos-package substring package)
  (call-with-current-tracking-output-port
    (lambda (out)
      (fresh-line out)
      (newline out)
      (display "** Local bindings in " out)
      (write package out)
      (newline out)
      (let ((entries '()))
        (for-each-definition
         (lambda (name binding)
           (if (apropos? name substring)
               (set! entries
                     (cons (list name
                                 #f     ; no exported type
                                 binding)
                           entries))))
         package)
        ;; Remove ORIGIN because we know it's from this package.
        (with-apropos-verbosity (delq 'origin (apropos-verbosity))
          (lambda ()
            (display-entries entries (package-uid package) out))))
      (for-each (lambda (struct)
                  (fresh-line out)
                  (*apropos-structure substring struct out
                                      "Imported from" #f))
                (package-opens package)))))

(define (apropos-structure substring struct)
  (call-with-current-tracking-output-port
    (lambda (out)
      (*apropos-structure substring struct out "Exported by" #t))))

(define (*apropos-structure substring struct out banner force-banner?)
  (let ((entries '()))
    (for-each-export (lambda (name exported-type binding)
                       (if (apropos? name substring)
                           (set! entries
                                 (cons (list name
                                             exported-type
                                             binding)
                                       entries))))
                     struct)
    (cond ((pair? entries)
           (fresh-line out)
           (newline out)
           (display "** " out)
           (display banner out)
           (write-char #\space out)
           (write struct out)
           (newline out)
           (display-entries entries
                            (package-uid (structure-package struct))
                            out)))))

(define (display-entries entries pkg-uid out)
  ((lambda (body)
     (for-each body
               (list-sort! (lambda (a b)
                             (string<? (symbol->string (car a))
                                       (symbol->string (car b))))
                           entries)))
   (lambda (entry)
     (destructure (( (name exported-type binding)
                     entry))
       (display "  " out)
       (write name out)
       (if binding
           (let ((verbosity (apropos-verbosity))
                 (inferred-type (binding-type binding))
                 (location (binding-place binding)))
             (cond ((null? verbosity)
                    (newline out))
                   ((null? (cdr verbosity))
                    (display-simple-entry inferred-type exported-type
                                          name location pkg-uid
                                          (car verbosity)
                                          out))
                   (else
                    (display-compound-entry inferred-type exported-type
                                            name location pkg-uid
                                            verbosity
                                            out))))
           (begin (indent-to 29 out)
                  (display " (not yet defined)" out)))
       (newline out)))))

(define (display-simple-entry inferred-type exported-type
                              exported-name location pkg-uid
                              verbosity
                              out)
  (indent-to 29 out)
  ;++ want XCASE
  (case verbosity
    ((inferred-type exported-type)
     (display " : " out)
     (display-type (if (eq? verbosity 'inferred-type)
                       inferred-type
                       exported-type)
                   out))
    ((value)
     (display-value location inferred-type out))
    ((origin)
     (cond ((location-origin location pkg-uid)
            => (lambda (origin)
                 (display " originally " out)
                 (display origin out)))))))

(define (display-compound-entry exported-type inferred-type
                                exported-name location pkg-uid
                                verbosity
                                out)
  (if (memq 'value verbosity)
      (begin (indent-to 29 out)
             (display-value location inferred-type out)))
  (cond ((and (memq 'inferred-type verbosity)
              inferred-type)
         => (lambda (type)
              (newline out)
              (display "    inferred type: " out)
              (display-type inferred-type out))))
  (cond ((and (memq 'exported-type verbosity)
              exported-type)
         => (lambda (type)
              (newline out)
              (display "    exported type: " out)
              (display-type exported-type out))))
  (cond ((and (memq 'origin verbosity)
              (location-origin location pkg-uid))
         => (lambda (origin)
              (newline out)
              (display "    originally " out)
              (display origin out)))))

(define (display-value location inferred-type out)
  (cond ((eq? inferred-type syntax-type)
         (display " (syntax)" out))
        ((location? location)
         (cond ((location-assigned? location)
                (display " = " out)
                (limited-write (contents location)
                               out
                               (apropos-depth)
                               (apropos-breadth)))
               ((location-defined? location)
                (display " (unassigned)" out))
               (else
                (display " (undefined)" out))))
        (else
         (display " (peculiar binding)" out))))

(define (display-type type out)
  (write (if (eq? type undeclared-type)
             ':undeclared
             (type->sexp type #t))
         out))

(define (location-origin location pkg-uid)
  (cond ((location-info location)
         => (lambda (info)
              (and (not (eqv? (cdr info) pkg-uid))
                   (string-append
                    (cond ((car info) => symbol->string)
                          (else ""))
                    (if (car info) " " "")
                    "from "
                    (cond ((table-ref package-name-table
                                      (cdr info))
                           => symbol->string)
                          (else
                           (number->string (cdr info))))))))
        (else #f)))

(define (apropos-of id)
  (cond ((string? id)
         (canonicalize-case id))
        ((symbol? id)
         (symbol->string id))
        (else
         (error "invalid apropos matcher" id))))

(define canonicalize-case
        (let ((casify (if (char=? (string-ref (symbol->string 't) 0)
                                  #\T)
                          char-upcase
                          char-downcase)))
          (lambda (string)
            (let* ((len (string-length string))
                   (result (make-string len)))
              (do ((i 0 (+ i 1)))
                  ((= i len) result)
                (string-set! result i
                             (casify (string-ref string i))))))))

(define (apropos? name smaller)
  (let* ((larger (symbol->string name))
         (larger-len (string-length larger))
         (smaller-len (string-length smaller)))
    (cond ((< larger-len smaller-len)
           #f)
          ((= larger-len smaller-len)
           (string=? larger smaller))
          (else
           (let loop ((i 0))
             (cond ((> (+ i smaller-len)
                       larger-len)
                    #f)
                   ((string=? (substring larger i (+ i smaller-len))
                              smaller)
                    #t)
                   (else
                    (loop (+ i 1)))))))))

(define (delq obj list)
  (if (null? list)
      '()
      (let ((elt (car list)))
        (if (eq? elt obj)
            (delq obj (cdr list))
            (cons elt (delq obj (cdr list)))))))



;;; I/O utilities

(define (call-with-current-tracking-output-port receiver)
  (let ((out (current-output-port)))
    (if (current-column out)
        (receiver out)
        (let ((out (make-tracking-output-port out)))
          (call-with-values (lambda () (receiver out))
            (lambda results
              (force-output out)
              (apply values results)))))))

(define (indent-to column port)
  (if (> (current-column port) column)
      (newline port))
  (display (make-string (- column (current-column port)) #\space)
           port))

(define (apropos-depth) 3)
(define (apropos-breadth) 4)

))