;;; -*- Mode: Scheme; scheme48-package: pathname -*-

;;;; Pathnames

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

; ,open define-record-type* simple-signals util methods receiving fluids cells

(define-record-type* pathname
  (%make-pathname origin directory filename)
  ())

(define-record-discloser :pathname
  (lambda (pathname)
    (cons (if (pathname-origin pathname)
              'PATHNAME
              'RELATIVE-PATHNAME)
          (%pathname-namelist pathname))))

(define (make-pathname origin directory filename)
  (or (%parse-namelist origin directory filename)
      (call-error "invalid arguments"
                  make-pathname origin directory filename)))

(define (make-relative-pathname directory filename)
  (make-pathname #f directory filename))

(define (maybe-object->pathname obj . fs-type-option)
  (cond ((pathname? obj) obj)
        ((symbol?   obj) (make-relative-pathname '() obj))
        ((string?   obj) (apply parse-namestring obj fs-type-option))
        ((pair?     obj) (parse-namelist obj))
        (else #f)))

(define (object->pathname obj . fs-type-option)
  (or (apply maybe-object->pathname obj fs-type-option)
      (apply call-error "unable to coerce to pathname"
             object->pathname obj fs-type-option)))

(define (pathname-complete? pathname)
  (and (pathname-origin pathname)
       (cond ((pathname-filename pathname)
              => filename-complete?)
             (else #f))))

(define (filename-complete? filename)
  (and (filename-base filename)
       #t))

;;;; Filenames

(define-record-type* filename
  (%make-filename base types version)
  ())

(define-record-discloser :filename
  (lambda (filename)
    `(FILENAME ,(disclose-filename filename #f))))

(define (disclose-filename filename flatten?)
  (let ((base (filename-base filename))
        (types (filename-types filename))
        (version (filename-version filename)))
    (if (and flatten? base (not (or version (pair? types))))
        base
        `(,base ,(if (and (pair? types)
                          (null? (cdr types)))
                     (car types)
                     '())
                ,@(if version (list version) '())))))

(define (make-filename base types . version)
  (let ((version (if (pair? version) (car version) #f)))
    (if (not (and (filename-base? base)
                  (filename-types? types)
                  (filename-version? version)))
        (call-error "invalid arguments" make-filename base types version)
        (%make-filename base
                        (if (or (string? types)
                                (symbol? types))
                            (list types)
                            types)
                        version))))

(define (filename-base? obj)
  (or (not obj)
      (string? obj)
      (symbol? obj)))

(define (filename-types? obj)
  (or (string? obj)
      (symbol? obj)
      (let loop ((obj obj))
        (cond ((pair? obj)
               (and (or (string? (car obj))
                        (symbol? (car obj)))
                    (loop (cdr obj))))
              ((null? obj) #t)
              (else #f)))))

(define (filename-version? obj)
  (or (not obj)
      (eq? obj 'OLDEST)
      (eq? obj 'NEWEST)
      (and (integer? obj)
           (exact? obj)
           (<= 0 obj))))

(define (filename-type filename)
  (let ((types (filename-types filename)))
    (if (pair? types)
        (last types)
        #f)))

;;;; Namelists

;;; <namelist> -> (<file namelist>)
;;;             | (<directory namelist> <file namelist>)
;;;             | (<origin> <directory namelist> <file namelist>)
;;;
;;; <file namelist> -> #F
;;;                  | <file base namelist>
;;;                  | (<file base namelist>)
;;;                  | (<file base namelist> <file types>)
;;;                  | (<file base namelist> <file types> <file version>)
;;;
;;; <file base namelist> -> <component>
;;;
;;; <file types> -> <component>
;;;               | (<component>*)
;;;
;;; <file version> -> #F
;;;                 | <exact, non-negative integer>
;;;                 | OLDEST
;;;                 | NEWEST
;;;
;;; <directory namelist> -> <component>
;;;                       | (<file namelist>*)
;;;
;;; <component> -> <string> | <symbol>

;++ This is kind of grody and would like to be rewritten with a decent pattern
;++ matcher.

(define (parse-namelist namelist)
  (cond ((not (pair? namelist)) #f)
        ((null? (cdr namelist))
         (%parse-namelist #f '() (car namelist)))
        ((not (pair? (cdr namelist))) #f)
        ((null? (cddr namelist))
         (%parse-namelist #f (car namelist) (cadr namelist)))
        ((not (pair? (cddr namelist))) #f)
        ((null? (cdddr namelist))
         (%parse-namelist (car namelist) (cadr namelist) (caddr namelist)))
        (else #f)))

(define (%parse-namelist origin directory filename)
  ((massage-directory directory)
   (lambda () #f)
   (lambda (directory)
     (let ((win (lambda (filename)
                  (%make-pathname origin directory filename))))
       (if (not filename)
           (win #f)
           ((massage-filename filename) (lambda () #f) win))))))

(define (massage-directory directory)
  (lambda (lose win)
    (cond ((filename? directory)
           (win (list directory)))
          ((or (string? directory) (symbol? directory))
           (win (list (make-filename directory #f))))
          (else
           (let loop ((in directory) (out '()))
             (cond ((pair? in)
                    ((massage-filename (car in))
                     lose
                     (lambda (filename)
                       (loop (cdr in) (cons filename out)))))
                   ((null? in) (win (reverse out)))
                   (else (lose))))))))

(define (massage-filename filename)
  (lambda (lose win)
    (cond ((filename? filename) (win filename))
          ((or (string? filename) (symbol? filename))
           (win (%make-filename filename #f #f)))
          ((parse-file-namelist filename) => win)
          (else (lose)))))

(define (parse-file-namelist filename)
  (cond ((not (and (pair? filename)
                   (filename-base? (car filename))))
         #f)
        ((null? (cdr filename))
         (make-filename (car filename) '()))
        ((not (and (pair? (cdr filename))
                   (filename-types? (cadr filename))))
         #f)
        ((null? (cddr filename))
         (make-filename (car filename) (cadr filename)))
        ((not (and (pair? (cddr filename))
                   (filename-version? (caddr filename))))
         #f)
        (else
         (make-filename (car filename) (cadr filename) (caddr filename)))))

(define (pathname-namelist pathname)
  (cond ((maybe-object->pathname pathname) => %pathname-namelist)
        (else (call-error "invalid pathname argument"
                          pathname-namelist pathname))))

(define (maybe-pathname-namelist pathname)
  (cond ((maybe-object->pathname pathname) => %pathname-namelist)
        (else #f)))

(define (%pathname-namelist pathname)
  (let ((origin (pathname-origin pathname))
        (directory (pathname-directory pathname))
        (filename (pathname-filename pathname)))
    `(,@(if origin (list origin) '())
      ,@(if (null? directory)
            (if origin '(()) '())
            (list (map (lambda (filename)
                         (disclose-filename filename #t))
                       directory)))
      ,(if filename
           (disclose-filename filename #t)
           #f))))

;;;; Substituting Components

(define (pathname-with-origin pathname origin)
  (cond ((maybe-object->pathname pathname)
         => (lambda (pathname)
              (make-pathname origin
                             (pathname-directory pathname)
                             (pathname-filename pathname))))
        (else
         (call-error "invalid pathname argument"
                     pathname-with-origin pathname origin))))

(define (pathname-with-directory pathname directory)
  (define (lose message)
    (call-error message pathname-with-directory pathname directory))
  (cond ((maybe-object->pathname pathname)
         => (lambda (pathname)
              (cond ((massage-directory directory)
                     => (lambda (directory)
                          (make-pathname (pathname-origin pathname)
                                         directory
                                         (pathname-filename pathname))))
                    (else (lose "invalid directory argument")))))
        ((massage-directory directory)
         (lose "invalid pathname argument"))
        (else
         (lose "invalid arguments"))))

(define (pathname-with-filename pathname filename)
  (define (lose message)
    (call-error message pathname-with-filename pathname filename))
  (cond ((maybe-object->pathname pathname)
         => (lambda (pathname)
              (cond ((massage-filename filename)
                     => (lambda (filename)
                          (make-pathname (pathname-origin pathname)
                                         (pathname-directory pathname)
                                         filename)))
                    (else (lose "invalid filename argument")))))
        ((massage-filename filename)
         (lose "invalid pathname argument"))
        (else
         (lose "invalid arguments"))))

;;;; Directory Pathnames

(define (directory-pathname? pathname)
  (cond ((maybe-object->pathname pathname)
         => (lambda (pathname)
              (not (pathname-filename pathname))))
        (else
         (call-error "invalid pathname argument"
                     directory-pathname? pathname))))

(define (pathname-as-directory pathname)
  (cond ((maybe-object->pathname pathname)
         => (lambda (pathname)
              (cond ((pathname-filename pathname)
                     => (lambda (filename)
                          (make-pathname (pathname-origin pathname)
                                         (append (pathname-directory pathname)
                                                 (list filename))
                                         #f)))
                    (else pathname))))
        (else
         (call-error "invalid pathname argument"
                     pathname-as-directory pathname))))

(define (pathname-container pathname)
  (cond ((maybe-object->pathname pathname)
         => (lambda (*pathname)
              (let loop ((*pathname *pathname))
                (let ((origin (pathname-origin *pathname))
                      (directory (pathname-directory *pathname))
                      (filename (pathname-filename *pathname)))
                  (cond (filename
                         (make-pathname origin directory #f))
                        ((pair? directory)
                         (make-pathname origin (drop-last directory) #f))
                        (else
                         (let ((expansion (expand-pathname *pathname)))
                           (if (pathname-eq? expansion *pathname)
                               #f
                               (loop expansion)))))))))
        (else
         (call-error "invalid pathname argument"
                     pathname-container pathname))))

(define (drop-last list)
  (let recur ((list list))
    (let ((tail (cdr list)))
      (if (null? tail)
          '()
          (cons (car list) (recur tail))))))

;;;; Expansion

(define (expand-pathname pathname)
  (cond ((maybe-object->pathname pathname)
         => (lambda (*pathname)
              (let ((origin (pathname-origin *pathname)))
                (define (win default-pathname)
                  (merge-pathnames (pathname-with-origin *pathname #f)
                                   default-pathname))
                (cond ((not origin) *pathname)
                      ((pair? origin)
                       (cond ((expand-parameterized-origin origin) => win)
                             (else *pathname)))
                      (else
                       (cond ((expand-simple-origin origin) => win)
                             (else *pathname)))))))
        (else
         (call-error "invalid pathname argument" expand-pathname pathname))))

(define (expand-parameterized-origin origin)
  (let ((try (lambda (expander)
               (if expander
                   (expander origin)
                   #f))))
    (or (try (assv-value (car origin) (local-pathname-expanders)))
        (try (assv-value (car origin) (global-pathname-expanders)))
        (try (host-origin-expander (car origin))))))

(define (expand-simple-origin origin)
  (let ((try (lambda (expansion)
               (if (procedure? expansion)
                   (expansion)
                   expansion))))
    (or (try (assv-value origin (local-pathname-expansions)))
        (try (assv-value origin (global-pathname-expansions)))
        (try (host-origin-expansion origin)))))

(define (assv-value key alist)
  (cond ((assv key alist) => cdr)
        (else #f)))

(define (expand-pathname* pathname)
  (cond ((maybe-object->pathname pathname)
         => (lambda (pathname)
              (let loop ((pathname pathname))
                (let ((expansion (expand-pathname pathname)))
                  (if (pathname-eq? expansion pathname)
                      pathname
                      (loop expansion))))))
        (else
         (call-error "invalid pathname argument" expand-pathname* pathname))))

;++ Check argument types...clean out #F...

(define $global-pathname-expansions (make-fluid (make-cell '())))
(define $global-pathname-expanders (make-fluid (make-cell '())))

(define (global-pathname-expansions)
  (fluid-cell-ref $global-pathname-expansions))

(define (global-pathname-expanders)
  (fluid-cell-ref $global-pathname-expanders))

(define (define-global-pathname-expansion origin expansion)
  (let ((expansions (global-pathname-expansions)))
    (cond ((assq origin expansions)
           => (lambda (probe)
                (warn "redefining global pathname expansion"
                      origin
                      `(old: ,(cdr probe))
                      `(new: ,expansion))
                (set-cdr! probe expansion)))
          (else
           (fluid-cell-set! $global-pathname-expansions
                            (cons (cons origin expansion) expansions))))))

(define (define-global-pathname-expander origin-key expander)
  (let ((expanders (global-pathname-expanders)))
    (cond ((assq origin-key expanders)
           => (lambda (probe)
                (warn "redefining global pathname expander"
                      origin-key
                      `(old: ,(cdr probe))
                      `(new: ,expander))
                (set-cdr! probe expander)))
          (else
           (fluid-cell-set! $global-pathname-expanders
                            (cons (cons origin-key expander) expanders))))))

(define (with-pathname-origins-preserved thunk)
  (let-fluids $global-pathname-expansions
                (make-cell (global-pathname-expansions))
              $global-pathname-expanders
                (make-cell (global-pathname-expanders))
    thunk))

(define $local-pathname-expansions (make-fluid '()))
(define $local-pathname-expanders (make-fluid '()))

(define (local-pathname-expansions) (fluid $local-pathname-expansions))
(define (local-pathname-expanders) (fluid $local-pathname-expanders))

(define (with-local-pathname-expansion origin expansion thunk)
  (let-fluid $local-pathname-expansions
      (cons (cons origin expansion) (local-pathname-expansions))
    thunk))

(define (with-local-pathname-expander origin-key expander thunk)
  (let-fluid $local-pathname-expanders
      (cons (cons origin-key expander) (local-pathname-expanders))
    thunk))

;;;; Comparison

(define (pathname-eq? pathname-a pathname-b)
  (let ((*pathname-a (maybe-object->pathname pathname-a))
        (*pathname-b (maybe-object->pathname pathname-b)))
    (if (not (and *pathname-a *pathname-b))
        (call-error "invalid pathname arguments"
                    pathname-eq? pathname-a pathname-b)
        (%pathname-eq? pathname-a pathname-b))))

(define (pathname-eqv? pathname-a pathname-b)
  (let ((*pathname-a (maybe-object->pathname pathname-a))
        (*pathname-b (maybe-object->pathname pathname-b)))
    (if (not (and *pathname-a *pathname-b))
        (call-error "invalid pathname arguments"
                    pathname-eqv? pathname-a pathname-b)
        (%pathname-eq? (expand-pathname* *pathname-a)
                       (expand-pathname* *pathname-b)))))

(define (%pathname-eq? pathname-a pathname-b)
  (and (equal? (pathname-origin pathname-a)
               (pathname-origin pathname-b))
       (let loop ((directory-a (pathname-directory pathname-a))
                  (directory-b (pathname-directory pathname-b)))
         (if (and (pair? directory-a)
                  (pair? directory-b))
             (and (filename-eq? (car directory-a) (car directory-b))
                  (loop         (cdr directory-a) (cdr directory-b)))
             (and (null? directory-a)
                  (null? directory-b))))
       (let ((filename-a (pathname-filename pathname-a))
             (filename-b (pathname-filename pathname-b)))
         (if (and filename-a filename-b)
             (filename-eq? filename-a filename-b)
             (not (or filename-a filename-b))))))

(define (filename-eq? filename-a filename-b)
  (and (equal? (filename-base    filename-a) (filename-base    filename-b))
       (equal? (filename-types   filename-a) (filename-types   filename-a))
       (eqv?   (filename-version filename-a) (filename-version filename-b))))

;;;; Merging

(define (merge-pathnames pathname default-pathname)
  (let ((*pathname (maybe-object->pathname pathname))
        (*default-pathname (maybe-object->pathname default-pathname)))
    (if (not (and *pathname *default-pathname))
        (call-error "invalid arguments"
                    merge-pathnames pathname default-pathname)
        (let ((origin (pathname-origin *pathname))
              (directory (pathname-directory *pathname))
              (filename
               (merge-filenames (pathname-filename *pathname)
                                (pathname-filename *default-pathname))))
          (if origin
              (make-pathname origin directory filename)
              (make-pathname (pathname-origin *default-pathname)
                             (append (pathname-directory *default-pathname)
                                     directory)
                             filename))))))

(define (merge-filenames filename default-filename)
  (cond ((not filename) default-filename)
        ((not default-filename) filename)
        (else
         (let ((base (filename-base filename))
               (types (filename-types filename))
               (version (filename-version filename)))
           (make-filename (or base (filename-base default-filename))
                          (if (null? types)
                              (filename-types default-filename)
                              types)
                          (or version
                              (and (not base)
                                   (filename-version default-filename))))))))

(define (enough-pathname pathname default-pathname)
  (let ((*pathname (maybe-object->pathname pathname))
        (*default-pathname (maybe-object->pathname default-pathname)))
    (if (not (and *pathname *default-pathname))
        (call-error "invalid arguments"
                    enough-pathname pathname default-pathname)
        (let ((origin (pathname-origin *pathname))
              (directory (pathname-directory *pathname))
              (filename
               (enough-filename (pathname-filename *pathname)
                                (pathname-filename *default-pathname))))
          (if (and origin
                   (not (equal? origin (pathname-origin *default-pathname))))
              (make-pathname origin directory filename)
              (make-pathname
               #f
               (enough-directory directory
                                 (pathname-directory *default-pathname))
               filename))))))

(define (enough-directory directory default-directory)
  (strip-common-prefix directory default-directory filename-eq?))

(define (enough-filename filename default-filename)
  (if (not default-filename)
      filename
      (let ((enough (lambda (accessor)
                      (let ((component (accessor filename))
                            (default-component (accessor default-filename)))
                        (and (not (equal? component default-component))
                             component)))))
        (make-filename (enough filename-base)
                       (enough filename-types)
                       (enough filename-version)))))

(define (strip-common-prefix datum default-datum equal?)
  (let loop ((components datum) (default-components default-datum))
    (cond ((not (pair? default-components))
           components)
          ((and (pair? components)
                (equal? (car components)
                        (car default-components)))
           (loop (cdr components)
                 (cdr default-components)))
          (else datum))))

;;;; Namestrings

(define (parse-namestring namestring . fs-type-option)
  (%parse-namestring (file-system-type-option fs-type-option) namestring))

(define (pathname-namestring pathname . fs-type-option)
  (cond ((maybe-object->pathname pathname)
         => (lambda (pathname)
              (%pathname-namestring (file-system-type-option fs-type-option)
                                    pathname)))
        (else
         (apply call-error "invalid pathname argument"
                pathname-namestring pathname fs-type-option))))

(define (enough-namestring pathname default-pathname . fs-type-option)
  (let ((*pathname (maybe-object->pathname pathname))
        (*default-pathname (maybe-object->pathname default-pathname)))
    (if (and *pathname *default-pathname)
        (%enough-namestring (file-system-type-option fs-type-option)
                            *pathname
                            *default-pathname)
        (apply call-error "invalid pathname arguments"
               enough-namestring pathname default-pathname fs-type-option))))

(define (origin-namestring pathname . fs-type-option)
  (cond ((maybe-object->pathname pathname)
         => (lambda (pathname)
              (%origin->namestring (file-system-type-option fs-type-option)
                                   (pathname-origin pathname))))
        (else
         (apply call-error "invalid pathname argument"
                origin-namestring pathname fs-type-option))))

(define (directory-namestring pathname . fs-type-option)
  (cond ((maybe-object->pathname pathname)
         => (lambda (pathname)
              (%directory->namestring (file-system-type-option fs-type-option)
                                      (pathname-directory pathname))))
        (else
         (apply call-error "invalid pathname argument"
                directory-namestring pathname fs-type-option))))

(define (file-namestring pathname . fs-type-option)
  (cond ((maybe-object->pathname pathname)
         => (lambda (pathname)
              (%filename->namestring (file-system-type-option fs-type-option)
                                     (pathname-filename pathname))))
        (else
         (apply call-error "invalid pathname argument"
                file-namestring pathname fs-type-option))))

(define (file-system-type-option fs-type-option)
  (if (pair? fs-type-option)
      (car fs-type-option)
      (host-file-system-type)))

;;;;; Namestring Generics

(define-generic %parse-namestring &parse-namestring (fs-type namestring))

(define-generic %canonicalize-namestring &canonicalize-namestring
  (fs-type namestring))

(define-generic %pathname-namestring &pathname-namestring
  (fs-type pathname))

(define-generic %enough-namestring &enough-namestring
  (fs-type pathname default-pathname))

(define-generic %origin->namestring &origin->namestring (fs-type origin))

(define-generic %directory->namestring &directory->namestring
  (fs-type directory))

(define-generic %filename->namestring &filename->namestring
  (fs-type filename))

;;;; Default methods

(define-method &canonicalize-namestring (fs-type namestring)
  (%pathname-namestring fs-type (%parse-namestring fs-type namestring)))

(define-method &pathname-namestring (fs-type pathname)
  (string-append (%origin->namestring fs-type (pathname-origin pathname))
                 (%directory->namestring fs-type (pathname-directory pathname))
                 (cond ((pathname-filename pathname)
                        => (lambda (filename)
                             (%filename->namestring fs-type filename)))
                       (else ""))))

(define-method &enough-namestring (fs-type pathname default-pathname)
  (%pathname-namestring fs-type (enough-pathname pathname default-pathname)))

;;;; Local Host Initialization

(define (initialize-pathnames! host-type expansion-method expander-method)
  (fluid-cell-set! $host-file-system-type host-type)
  (fluid-cell-set! $host-origin-expansion-method expansion-method)
  (fluid-cell-set! $host-origin-expander-method expander-method))

(define $host-file-system-type (make-fluid (make-cell #f)))
(define (host-file-system-type) (fluid-cell-ref $host-file-system-type))

(define $host-origin-expansion-method (make-fluid (make-cell #f)))
(define (host-origin-expansion origin)
  ((fluid-cell-ref $host-origin-expansion-method) origin))

(define $host-origin-expander-method (make-fluid (make-cell #f)))
(define (host-origin-expander origin-key)
  ((fluid-cell-ref $host-origin-expander-method) origin-key))

(define (with-pathnames-initialized host-type expansion-method expander-method
          thunk)
  (let-fluids $host-file-system-type (make-cell host-type)
              $host-origin-expansion-method (make-cell expansion-method)
              $host-origin-expander-method (make-cell expander-method)
    thunk))
