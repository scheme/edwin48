;;; -*- Mode: Scheme; scheme48-package: pathname -*-

;;;; Pathnames
;;;; Unix File System Type

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define :unix (singleton 'UNIX))

(define-method &origin->namestring ((fs-type :unix) origin)
  (cond ((not origin) ".")
        ((eq? origin 'ROOT) "")
        ((string? origin) (string-append "$" origin))
        ((eq? origin 'HOME) "~")
        ((unix-home-origin origin)
         => (lambda (username)
              (string-append "~" (unix-path-component username))))
        ((eq? origin 'BACK) "..")
        ((let loop ((origin origin))
           (if (pair? origin)
               (and (eq? (car origin) 'BACK)
                    (loop (cdr origin)))
               (null? origin)))
         (decorated-string-append #f "" "/" ""
                                  (map (lambda (x) x "..") origin)))
        (else
         (error "unrecognized Unix pathname origin" origin))))

(define (unix-home-origin origin)
  (and (pair? origin)
       (eq? (car origin) 'HOME)
       (pair? (cdr origin))
       (or (string? (cadr origin))
           (symbol? (cadr origin)))
       (null? (cddr origin))
       (cadr origin)))

(define-method &directory->namestring ((fs-type :unix) directory)
  (decorated-string-append "/" "/" "/" "/"
                           (map unix-filename->namestring directory)))

(define-method &filename->namestring ((fs-type :unix) filename)
  (unix-filename->namestring filename))

(define (unix-filename->namestring filename)
  (string-append
   (unix-path-component (filename-base filename))
   (cond ((filename-types filename)
          => (lambda (types)
               (decorated-string-append "" "." "." ""
                                        (map unix-path-component types))))
         (else ""))
   (let ((version (filename-version filename)))
     (case version
       ((OLDEST) (error "Unix has no representation for oldest version"))
       ((NEWEST #F) "")
       (else (string-append ".~" (number->string version 10) "~"))))))

(define (unix-path-component component)
  (let ((component (if (symbol? component)
                       (string-downcase (symbol->string component))
                       component)))
    (if (let ((length (string-length component)))
          (or (string-index #\/ component 0 length)
              (string-index (ascii->char 0) component 0 length)))
        (error "illegal Unix path component" component)
        component)))

;;;; Unix Namestring Parser

;++ This is really grody.

(define-method &parse-namestring ((fs-type :unix) namestring)
  (or (maybe-parse-unix-namestring namestring)
      (error "malformed Unix namestring" namestring)))

(define (ununix-path-component string start end)
  (let ((component (substring string start end)))
    (let loop ((i 0))
      (cond ((= i (string-length component))
             (string->symbol component))
            ((or (not (char-alphabetic? (string-ref component i)))
                 (char-lower-case? (string-ref component i)))
             (loop (+ i 1)))
            (else
             component)))))

(define (maybe-parse-unix-namestring namestring)
  (let ((length (string-length namestring)))
    (cond ((= length 0) (make-pathname #f '() #f))
          ((= length 1)
           (case (string-ref namestring 0)
             ((#\~) (make-pathname 'HOME '() #f))
             ((#\/) (make-pathname 'ROOT '() #f))
             ((#\$) #f)                 ;No name for the environment variable
             ((#\.) (make-pathname #f '() #f))
             (else (make-pathname #f '() namestring))))
          (else (parse-unix-namestring/origin namestring)))))

(define (parse-unix-namestring/origin namestring)
  (case (string-ref namestring 0)
    ((#\~) (parse-unix-namestring/home namestring))
    ((#\/) (parse-unix-namestring/directory namestring 0 'ROOT))
    ((#\$) (parse-unix-namestring/env namestring))
    ((#\.) (parse-unix-namestring/dot namestring))
    (else (parse-unix-namestring/directory namestring
                                           -1     ;No leading slash
                                           #f))))

(define (parse-unix-namestring/home namestring)
  (let ((length (string-length namestring)))
    (cond ((string-index #\/ namestring 0 length)
           => (lambda (index)
                (parse-unix-namestring/directory
                 namestring
                 index
                 (if (= index 1)
                     'HOME
                     `(HOME ,(ununix-path-component namestring 1 index))))))
          (else
           (make-pathname `(HOME ,(ununix-path-component namestring 1 length))
                          '()
                          #f)))))

(define (parse-unix-namestring/env namestring)
  (let ((length (string-length namestring)))
    (cond ((string-index #\/ namestring 0 length)
           => (lambda (index)
                (and (> index 1)
                     (parse-unix-namestring/directory
                      namestring
                      index
                      (unix-environment-variable-origin
                       (substring namestring 1 index))))))
          (else
           (make-pathname (unix-environment-variable-origin
                           (substring namestring 1 length))
                          '()
                          #f)))))

(define (unix-environment-variable-origin variable)
;++ (if (string=? variable "HOME")
;++     'HOME
;++     variable)
  variable)

(define (parse-unix-namestring/dot namestring)
  (let ((length (string-length namestring)))
    (define (parse-directory index backs)
      (parse-unix-namestring/directory namestring (- index 1) backs))
    (let loop ((backs #f) (index 0))
      (cond ((= index (string-length namestring))
             (make-pathname backs '() #f))
            ((not (char=? #\. (string-ref namestring index)))
             (if (char=? #\/ (string-ref namestring index))
                 (loop backs (+ index 1))
                 (parse-directory index backs)))
            ((= length (+ index 1))
             (make-pathname backs '() "."))
            ((not (char=? #\. (string-ref namestring (+ index 1))))
             (if (char=? #\/ (string-ref namestring (+ index 1)))
                 (loop backs (+ index 2))
                 (parse-directory index backs)))
            (else
             (loop (add-back backs)
                   (+ index 3)))))))

(define (add-back backs)
  (cond ((not backs) 'BACK)
        ((eq? backs 'BACK) '(BACK BACK))
        (else (cons 'BACK backs))))

(define (parse-unix-namestring/directory namestring start origin)
  (define (parse-filename start end)
    (parse-unix-file-namestring namestring start end))
  (let loop ((directory '()) (index start))
    (let ((start (+ index 1)))
      (cond ((string-index #\/ namestring start (string-length namestring))
             => (lambda (index)
                  ;; Ignore double slashes and dots.
                  (loop (if (or (= index start)
                                (and (= index (+ start 1))
                                     (char=? (string-ref namestring start)
                                             #\.)))
                            directory
                            (cons (parse-filename start index) directory))
                        index)))
            (else
             (make-pathname origin
                            (reverse directory)
                            (let ((end (string-length namestring)))
                              (and (< start end)
                                   (parse-filename start end)))))))))

(define (parse-unix-file-namestring string start end)
  (receive (version end)
           (if (parse-unix-file-versions?)
               (parse-unix-file-version string start end)
               (values #f end))
    (if (parse-unix-file-types?)
        (receive (types-start types) (parse-unix-file-types string start end)
          (make-filename (ununix-path-component string start types-start)
                         types
                         version))
        (make-filename (ununix-path-component string start end) '() version))))

(define (parse-unix-file-version string start end)
  (cond ((and (char=? #\~ (string-ref string (- end 1)))
              (string-index-right #\. string start end))
         => (lambda (dot-index)
              (cond ((and (< dot-index (- end 2))
                          (char=? #\~ (string-ref string (+ dot-index 1)))
                          (string->number (substring string
                                                     (+ dot-index 2)
                                                     (- end 1))
                                          10))
                     => (lambda (version)
                          (values version dot-index)))
                    (else (values #f end)))))
        (else (values #f end))))

(define (parse-unix-file-types string start end)
  (let ((component ununix-path-component))
    (cond ((string-index #\. string start end)
           => (lambda (first-dot-index)
                (let loop ((types '()) (start first-dot-index))
                  (let ((start (+ start 1)))
                    (cond ((string-index #\. string start end)
                           => (lambda (dot-index)
                                (loop (cons (component string start dot-index)
                                            types)
                                      dot-index)))
                          (else
                           (values first-dot-index
                                   (reverse (cons (component string start end)
                                                  types)))))))))
          (else (values end '())))))

(define $parse-unix-file-types? (make-fluid (make-cell #f)))
(define $parse-unix-file-versions? (make-fluid (make-cell #f)))

(define (parse-unix-file-types?) (fluid-cell-ref $parse-unix-file-types?))
(define (parse-unix-file-versions?)
  (fluid-cell-ref $parse-unix-file-versions?))

(define (string-index char string start end)
  (let loop ((i start))
    (and (< i end)
         (if (char=? char (string-ref string i))
             i
             (loop (+ i 1))))))

(define (string-index-right char string start end)
  (let loop ((i end))
    (and (> i start)
         (let ((j (- i 1)))
           (if (char=? char (string-ref string j))
               j
               (loop j))))))

(define (string-downcase string)
  (list->string (map char-downcase (string->list string))))

(define (decorated-string-append empty prefix infix suffix strings)
  (if (null? strings)
      empty
      (apply string-append
             prefix
             (let recur ((strings strings))
               (let ((tail (cdr strings)))
                 (if (pair? tail)
                     (cons (car strings) (cons infix (recur (cdr strings))))
                     (list (car strings) suffix)))))))

;++ foo

(define (unix/origin-expansion origin)
  (cond ((string? origin)
         (lambda ()
           (lookup-environment-variable origin)))
        ((eq? origin 'HOME)
         (lambda ()
           (user-info-home-directory-pathname
            (user-id->user-info (get-user-id)))))
        (else #f)))

(define (unix/origin-expander origin-key)
  (case origin-key
    ((HOME)
     (lambda (origin)
       (cond ((and (pair? (cdr origin))
                   (or (string? (cadr origin))
                       (symbol? (cadr origin)))
                   (null? (cddr origin))
                   (name->user-info (unix-path-component (cadr origin))))
              => user-info-home-directory-pathname)
             (else #f))))
    (else #f)))

(define (user-info-home-directory-pathname user-info)
  (pathname-as-directory (user-info-home-directory user-info)))
