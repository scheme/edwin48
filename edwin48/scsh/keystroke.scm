;;; -*- Mode: Scheme; scheme48-package: keystroke -*-

(define (known-key? k)
  (let loop ((keys all-named-keys))
    (cond
     ((null? keys) #f)
     ((and (char? k) (char=? k (caar keys)))
      (cdar keys))
     (else (loop (cdr keys))))))

(define-record-type* key
  (really-make-key
   ;;
   ;; Stores the character value of the key as a string or
   ;; a symbol containing the name of the keystroke (i.e. up, down, left, right)
   ;;
   value
   ;;
   ;; modifiers must be one of the defined key modifiers, listed
   ;; above. The representation is just an enum-set of symbols.
   ;;
   modifiers)
  ())

(define empty-modifiers (make-key-modifier-set '()))
(define (key-modifiers-empty? modifiers) (null? (key-modifier-set->list modifiers)))

(define (key->name k)
  (cond
   ((list? k) (map key->name k))
   (else
    (let ((modifiers (key-modifiers k))
          (name      (key-name k)))
      (if (key-modifiers-empty? modifiers)
          `(kbd ,name)
          `(kbd (,@(map key-modifier-name (key-modifier-set->list modifiers))
                 ,name)))))))

(define (key-name k)
  (let ((value (key-value k)))
    (cond
     ((char? value)   (char->name value))
     ((symbol? value) (symbol->string value))
     (else (error "key is invalid" k)))))

(define (key=? k1 k2)

  (define (modifiers=? m1 m2) (key-modifier-set=? m1 m2))
  (define (value=?     v1 v2)
    (cond ((and (symbol? v1) (symbol? v2))
           (eq? v1 v2))
          ((and (char? v1) (char? v2))
           (char=? v1 v2))
          (else #f) ))

  (if (and (key? k1) (key? k2))
      (and (modifiers=? (key-modifiers k1)
                        (key-modifiers k2))
           (value=?     (key-value     k1)
                        (key-value     k2)))
      #f))

(define* (make-key value
                   (modifiers empty-modifiers)
                   (name      ""))
  (define (strip value)
    (ascii->char (+ -1
                    (char->ascii value)
                    (char->ascii #\a))))
  (define (union modifiers modifier)
    (key-modifier-set-union modifiers modifier))

  (let ((modifiers (if (key-modifier? modifiers)
                       (make-key-modifier-set (list modifiers))
                       modifiers)))
    (cond
     ((number? value) (make-key (ascii->char value) modifiers))
     ((char? value)
      (cond
       ((known-key? value) => (lambda (name) (really-make-key name modifiers)))
       ((char-set-contains? char-set:iso-control value)
        (really-make-key (strip value)
                         (union modifiers (key-modifier-set ctrl))))
       (else (really-make-key value modifiers))))
     ((string? value)
      (if (zero? (string-length value))
          (error "invalid string input" value)
          (cond
           ((symbol? name) (really-make-key name modifiers))
           ((string? name) (really-make-key (string->symbol name) modifiers))
           (else (error "invalid name" name )))))
     (else (error "invalid input" value)))))

(define (add-key-modifiers key modifiers)
  (let ((existing (key-modifiers key))
        (union    key-modifier-set-union))
    (replace-key-modifiers key (union existing modifiers))))

(define (replace-key-modifiers key modifiers)
  (let ((value (key-value key)))
    (make-key value modifiers)))

(define (strip-key-modifiers key)
  (replace-key-modifiers key empty-modifiers))

(define-syntax kbd
  (lambda (form rename compare)
    (let ((%key (rename 'make-key))
          (r    rename)
          (form (cdr form))) ;; discard the first token, 'KBD'
      (define all-modifiers
        (map key-modifier-name (vector->list all-key-modifiers)))
      (define (parse-kbd-form form)
        (cond
         ((char? form)
          `(,%key ,form))
         ((string? form)
          (if (= 1 (string-length form))
              `(,%key (,(r 'string-ref) ,form 0))
              `(,(r 'map) (,(r 'lambda) (c) (,%key c))
                (,(r 'string->list) ,form))))
         ((symbol? form)
          (let loop ((keys all-named-keys))
            (cond
             ((null? keys) (syntax-error "not a valid named key" form))
             ((eq? form (cdar keys)) `(,%key ,(caar keys)))
             (else (loop (cdr keys))))))
         ((list? form)
          (let* ((form      (reverse form))
                 (key       (car form))
                 (modifiers (cdr form)))
            `(,%key ,key (,(r 'key-modifier-set) ,@modifiers))))))
      (if (= 1 (length form))
          (parse-kbd-form (car form))
          (map parse-kbd-form form)))))
