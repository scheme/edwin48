;;; -*- Mode: Scheme; scheme48-package: keystroke -*-

(define-record-type* simple-key
  (%make-simple-key
   ;;
   ;; Stores the character value of the key as a string
   ;;
   value
   ;;
   ;; modifiers must be one of the defined key modifiers, listed
   ;; above. The representation is just a list (set) of symbols, it
   ;; could be optimized to use a bit offsets.
   ;;
   modifiers)
  ())

(define (make-simple-key value modifiers)
  (if (char-set-contains? char-set:iso-control value)
      (%make-simple-key (string (ascii->char (+ -1
                                                (char->ascii value)
                                                (char->ascii #\A))))
                        (enum-set-union modifiers
                                        (key-modifier-set ctrl)))
      (%make-simple-key (string value) modifiers)))

(define (simple-key-name k)
  (if (simple-key? k)
      (let ((value (simple-key-value k)))
        (string-ref value 0))
      (error "not a simple key" k)))

(define-record-type* named-key
  (make-named-key
   ;;
   ;; a string containing a escape sequence
   ;;
   value
   ;;
   ;; see above
   ;;
   modifiers
   ;;
   ;; a symbol containing the name of the keystroke (i.e. up, down, left, right)
   ;;
   name)
  ())

(define empty-modifiers (make-key-modifier-set '()))
(define (key-modifiers-empty? modifiers) (null? (enum-set->list modifiers)))

(define (key->name k)
  (cond
   ((list? k) (map key->name k))
   (else
    (let ((modifiers (key-modifiers k))
          (name      (key-name k)))
      (if (key-modifiers-empty? modifiers)
          `(kbd ,name)
          `(kbd (,@(map key-modifier-name (enum-set->list modifiers))
                 ,name)))))))
 
(define (key? k)
  (or (simple-key? k)
      (named-key?  k)))

(define (key-value k)
  (cond ((simple-key? k) (simple-key-value k))
        ((named-key?  k) (named-key-value  k))
        (else (error "not a key" k))))

(define (key-modifiers k)
  (cond ((simple-key? k) (simple-key-modifiers k))
        ((named-key?  k) (named-key-modifiers  k))
        (else (error "not a key" k))))

(define (key-name k)
  (cond ((simple-key? k) (char->name     (simple-key-name  k)))
        ((named-key?  k) (symbol->string (named-key-name  k)))
        (else (error "not a key" k))))

(define (key=? k1 k2)

  (define (modifiers=? m1 m2) (enum-set=? m1 m2))
  (define (value=?     v1 v2) (string=?   v1 v2))

  (if (and (key? k1) (key? k2))
      (and (modifiers=? (key-modifiers k1)
                        (key-modifiers k2))
           (value=?     (key-value     k1)
                        (key-value     k2)))
      #f))

(define (valid-modifier? m) (key-modifier? m))

(define* (make-key value
                   (modifiers empty-modifiers)
                   (name      #f))
  (cond
   ((number? value) (make-simple-key (ascii->char value) modifiers))
   ((char?   value) (make-simple-key value modifiers))
   ((string? value)
    (if (zero? (string-length value))
        (error "invalid string input" value)
        (cond
         ((symbol? name) (make-named-key value modifiers name)
         ((string? name) (make-named-key value modifiers (string->symbol name)))
         (else (make-simple-key value modifiers))))))
   (else (error "invalid input" value))))

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
         ((list? form)
          (let* ((form      (reverse form))
                 (key       (car form))
                 (modifiers (cdr form)))
            ;; if all the modifiers are valid
            (if (lset<= compare
                        (map rename modifiers)
                        (map rename all-modifiers))
                `(,%key ,key (,(r 'delete-duplicates) ',modifiers))
                (syntax-error "contains invalid modifier" modifiers))))))
      (if (= 1 (length form))
          (parse-kbd-form (car form))
          (map parse-kbd-form form)))))
