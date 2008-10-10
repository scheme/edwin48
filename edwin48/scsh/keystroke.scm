;;; -*- Mode: Scheme; scheme48-package: keystroke -*-

(define (known-key? k)
  (let loop ((keys all-named-keys))
    (cond
     ((null? keys) #f)
     ((char=? k (caar keys))
      (symbol->string (cdar keys)))
     (else (loop (cdr keys))))))

(define-record-type* simple-key
  (%make-simple-key
   ;;
   ;; Stores the character value of the key as a string
   ;;
   value
   ;;
   ;; modifiers must be one of the defined key modifiers, listed
   ;; above. The representation is just an enum-set of symbols.
   ;;
   modifiers)
  ())

(define* (make-simple-key value (modifiers empty-modifiers))
  (define (strip value)
    (ascii->char (+ -1
                    (char->ascii value)
                    (char->ascii #\A))))
  (define (union modifiers modifier)
    (key-modifier-set-union modifiers modifier))
  (if (char-set-contains? char-set:iso-control value)
      (%make-simple-key (string (strip value))
                        (union modifiers (key-modifier-set ctrl)))
      (%make-simple-key (string value) modifiers)))

(define (simple-key-name k)
  (if (simple-key? k)
      (let ((value (simple-key-value k)))
        (string-ref value 0))
      (error "not a simple key" k)))

(define-record-type* named-key
  (make-named-key
   ;;
   ;; a symbol containing the name of the keystroke (i.e. up, down, left, right)
   ;;
   value
   ;;
   ;; see above
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
        ((named-key?  k) (named-key-value k))
        (else (error "not a key" k))))

(define (key=? k1 k2)

  (define (modifiers=? m1 m2) (key-modifier-set=? m1 m2))
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
                   (name      ""))
  (cond
   ((known-key? value) => (lambda (name) (make-named-key name modifiers)))
   ((number? value) (make-simple-key (ascii->char value) modifiers))
   ((char?   value) (make-simple-key value modifiers))
   ((string? value)
    (if (zero? (string-length value))
        (error "invalid string input" value)
        (cond
         ((symbol? name) (make-named-key (symbol->string name) modifiers))
         ((string? name) (make-named-key name modifiers))
         (else (make-simple-key value modifiers)))))
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
