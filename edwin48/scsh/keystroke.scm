;;; -*- Mode: Scheme; scheme48-package: keystroke -*-

;; Char    Decimal Hex     Octal   Binary       [Meaning]
;;  m-a    225     0xe1    0341    11100001
;;  m-a    225     0xe1    0341    11100001
;;  m-a    225     0xe1    0341    11100001
;;  m-n    238     0xee    0356    11101110
;;    a     97     0x61    0141    01100001
;;   ^A      1     0x01    0001    00000001     (SOH)
;;    A     65     0x41    0101    01000001
;;   ^[     27     0x1b    0033    00011011     Escape (ESC)
;;   ^A      1     0x01    0001    00000001     (SOH)
;;  m-a    225     0xe1    0341    11100001
;;    a     97     0x61    0141    01100001
;;  m-a    225     0xe1    0341    11100001
;;  m-a    225     0xe1    0341    11100001
;;   ^A      1     0x01    0001    00000001     (SOH)
;;   ^[     27     0x1b    0033    00011011     Escape (ESC)
;;    [     91     0x5b    0133    01011011
;;    A     65     0x41    0101    01000001
;;  m-h    232     0xe8    0350    11101000
;;    b     98     0x62    0142    01100010
;;   ^B      2     0x02    0002    00000010     (STX)
;;  m-b    226     0xe2    0342    11100010
;; m-^A    129     0x81    0201    10000001     (SOH)
;; m-^B    130     0x82    0202    10000010     (STX)


(define (known-key? k)
  (let loop ((keys all-named-keys))
    (cond
     ((null? keys) #f)
     ((and (char? k) (char=? k (caar keys)))
      (cdar keys))
     (else (loop (cdr keys))))))

(define (simple-key? k)
  (key-modifiers-empty? (key-modifiers k)))

(define (special-key? k) (not (simple-key? k)))

(define (key-char-value k)
  (if (simple-key? k)
      (string-ref (key-value k) 0)))

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

(define (%char-code char)
  (fix:and (char->integer char) #x1FFFFF))

;;; Right shift the bits over by 7
;;;
;;; %char-bits:  char -> integer
(define (%char-bits char)
  (fix:lsh (char->integer char) -7))

(define (ascii-controlified? char)
    (fix:< char #x20))

;; Check to see if the key is ascii-controlified?
;;  If so then =>
;;    See if the meta bit is set=>
;;     If so then => make a key META-CONTROL- (then unset the meta and control bit)

(define (remove-meta-ctrl int) (- int 32))
(define (remove-ctrl int) (+ 96 int))
(define (remove-meta int) (- int 97))

;;; TODO: Checking
(define (char-code char)
  (%char-code char))

(define (keystroke-bits-set? bits char)
  (fix:= bits (fix:and (%char-bits char) bits)))

(define (keystroke-bits-clear? bits char)
  (fix:= 0 (fix:and (%char-bits char) bits)))

(define (8-bit-char? object)
  (and (char? object)
       (fix:< (char->integer object) 256)))

(define (key->name k)
  (cond
   ((char? k)   `(kbd ,k))
   ((string? k) `(kbd ,k))
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
     ((string? value) value)
     (else (error "key is invalid" k)))))

(define (key=? k1 k2)
  (define (both pred? x y) (and (pred? x) (pred? y)))
  (define (modifiers=? m1 m2) (key-modifier-set=? m1 m2))
  (define (value=?     v1 v2)
    (cond ((both symbol? v1 v2) (eq?      v1 v2))
          ((both char?   v1 v2) (char=?   v1 v2))
          ((both string? v1 v2) (string=? v1 v2))
          (else #f)))

  (cond ((both key? k1 k2)
         (and (modifiers=? (key-modifiers k1)
                           (key-modifiers k2))
              (value=?     (key-value     k1)
                           (key-value     k2))))
        ((both list? k1 k2)
         (every (lambda (true?) true?) (map key=? k1 k2)))
        (else #f)))

;;; Returns a pretty printed string of a kbd record
;;;
;;; Example: (key->string (kbd (ctrl #\s))) => "(kbd (ctrl s))"
;;;
;;; key->string?  keystroke -> string
;;;
(define (key->string keystroke)
    (let* ((name (key-name keystroke))
           (modifiers (key-modifier-set->list (key-modifiers keystroke)))
           (mod-list (map (lambda (km) (symbol->string (key-modifier-name km))) modifiers)))
      (format #f "(kbd ~A)" (append mod-list (list name)))))

(define (key-hash key)
  (define (modifiers-hash modifiers)
    (apply + (map (lambda (m) (* 1000 (expt 2 (key-modifier-index m))))
                  (key-modifier-set->list modifiers))))
  (if (not (key? key))
      (error "Not a key" key)
      (let ((kval (key-value key)))
        (+ (modifiers-hash (key-modifiers key))
           (cond ((symbol? kval) (string-hash (symbol->string kval)))
                 ((string? kval) (string-hash kval))
                 (else (char->integer kval))))))) ;; (else (key-hash (char->name key))))))))
      ;; Have to do something special for keys that are symbols
      ;; (key-value key) returns a string, so make it a list of chars
      ;; then add them together to make the key

;;; TODO: Figure out what NAME is
(define* (make-key value
                   (modifiers empty-modifiers)
                   (name      ""))
  (define (strip value)
    (integer->char (+ -1
                    (char->integer value)
                    (char->integer #\a))))
  (define (union modifiers modifier)
    (key-modifier-set-union modifiers modifier))
  (let ((modifiers (if (key-modifier? modifiers)
                       (make-key-modifier-set (list modifiers))
                       modifiers)))
    (cond
     ((key? value)
      (make-key (key-value value)
                (union (key-modifiers value) modifiers)
                name))
     ((number? value) (make-key (integer->char value) modifiers))
     ((symbol? value) (really-make-key value modifiers))
     ((char? value)
      (cond
       ((known-key? value) => (lambda (name) (really-make-key name modifiers)))
       ((char-set-contains? char-set:iso-control value)
        (really-make-key (strip value) ; Possibly just value and not stripping it
                         (union modifiers (key-modifier-set ctrl))))
       (else (really-make-key (string value) modifiers))))
     ((string? value)
      (if (zero? (string-length value))
          (error "invalid string input" value)
          (cond
           ((symbol? name) (really-make-key name modifiers))
           ((string? name) (really-make-key value modifiers))
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
          (%key-value (rename 'key-value))
          (r    rename)
          (form (cdr form))) ;; discard the first token, 'KBD'

      (define all-modifiers
        (map key-modifier-name (vector->list all-key-modifiers)))

      (define (list->key form)
        (let ((form      (reverse form))
              (value     (car form))
              (modifiers (cdr form))
              (%key-modifier-set (r 'key-modifier-set)))
          `(,%key (,%key-value ,(->key value))
                  (,%key-modifier-set ,@modifiers))))

      (define (string->key form)
        (let ((%lambda       (r 'lambda))
              (%map          (r 'map))
              (%string->list (r 'string->list))
              (%string-ref   (r 'string-ref)))
          (if (= 1 (string-length form))
              `(,%key (,%string-ref ,form 0))
              `(,%map (,%lambda (c) (,%key c))
                      (,%string->list ,form)))))

      (define (symbol->key form)
        (let loop ((keys all-named-keys))
          (cond
           ((null? keys)
            (let ((keys (symbol->string form)))
              (if (string-every char-set:printing keys)
                  (string->key keys)
                  (syntax-error "not a valid key" form))))
           ((eq? form (cdar keys)) `(,%key ,(caar keys)))
           (else (loop (cdr keys))))))

      (define (->key form)
        (cond
         ((char? form)  `(,%key      ,form))
         ((string? form) (string->key form))
         ((symbol? form) (symbol->key form))
         ;; reverse the form so the key-value is the first element
         ((list? form)   (list->key   (reverse form)))))

      (if (= 1 (length form))
          (->key (car form))
          `(,(r 'list) ,@(map ->key form))))))
