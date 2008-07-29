;;; -*- Mode: Scheme; scheme48-package: keystroke -*-
;;;
;;; also defined for-syntax, see packages.scm
;;;
(define *keystroke-modifiers* '())
(define *keystroke-prefix*    'keystroke-modifier:)

(define-syntax define-keystroke-modifier
  (lambda (form rename compare)
    (let* ((form      (cdr form))
           (name      (car form))
           (offset    (cadr form))
           (full-name (symbol-append *keystroke-prefix* name))
           (%define   (rename 'define)))
      (if (not (assq name *keystroke-modifiers*))
          (set! *keystroke-modifiers* (alist-cons name offset *keystroke-modifiers*)))
      `(,%define ,full-name ,offset))))

(define-keystroke-modifier meta    1)
(define-keystroke-modifier control 2)
(define-keystroke-modifier super   4)
(define-keystroke-modifier hyper   8)

(define-record-type* simple-key
  (make-simple-key
   ;;
   ;; Stores the character value of the key
   ;;
   value
   ;;
   ;; modifiers must be one of the defined key modifiers, listed
   ;; above. The representation is just a list (set) of symbols, it
   ;; could be optimized to use a bit offsets.
   ;;
   modifiers)
  ())

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
  (cond ((simple-key? k) (char->name   (simple-key-value  k)))
        ((named-key?  k) (symbol->string (named-key-name  k)))
        (else (error "not a key" k))))

(define (key=? k1 k2)
  (if (and (key? k1) (key? k2))
      ;; check modifiers
      (and (list=  (key-modifiers k1)
                   (key-modifiers k2))
           ;; use the appropriate equality tester for simple/named
           ((cond
             ((and (simple-key? k1)
                   (simple-key? k2))
              char=?)
             ((and (named-key? k1)
                   (named-key? k2))
              string=?)
             (else (error "keys are not compatible" k1 k2)))
            (key-value k1)
            (key-value k2)))
      #f))

(define* (make-key value (modifiers '()) (name #f))
  (cond
   ((number? value) (make-simple-key (ascii->char value) modifiers))
   ((char? value)   (make-simple-key value modifiers))
   ((string? value)
    (cond
     ((or  (symbol? name) (string? name)) (make-named-key  value modifiers name))
     ((not (zero? (string-length value))) (make-simple-key value modifiers))
     (else "invalid string input" value)))
   (else (error "invalid input" value))))

(define-syntax kbd
  (lambda (form rename compare)
    (let ((%key (rename 'make-key))
          (r    rename)
          (form (cdr form))) ;; discard the first token, 'KBD'
      (define (parse-kbd-form form)
        (cond
         ((char? form)
          `(,%key (,(r 'string) ,form) '()))
         ((string? form)
          (if (= 1 (string-length form))
              `(,%key (,(r 'string-ref) ,form 0 ) '())
              `(,(r 'map) (,(r 'lambda) (c) (,%key c '()))
                (,(r 'string->list) ,form))))
         ((list? form)
          (let* ((form      (reverse form))
                 (key       (car form))
                 (modifiers (cdr form)))
            ;; if all the modifiers are valid
            (if (lset<= compare
                        (map rename modifiers)
                        (map (lambda (x) (rename (car x))) *keystroke-modifiers*))
                `(,%key ,key (,(r 'delete-duplicates) ',modifiers))
                (syntax-error "contains invalid modifier" modifiers))))))
      (if (= 1 (length form))
          (parse-kbd-form (car form))
          (map parse-kbd-form form)))))
