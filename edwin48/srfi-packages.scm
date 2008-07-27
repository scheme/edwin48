;;; -*- mode: scheme; scheme48-package: (config) -*-

;;; SRFI 43: Vector library

(define-interface srfi-43-interface
  (export make-vector vector vector-unfold vector-unfold-right
	  vector-copy vector-reverse-copy vector-append vector-concatenate
	  vector? vector-empty? vector= vector-ref vector-length
	  vector-fold vector-fold-right vector-map vector-map!
	  vector-for-each vector-count vector-index vector-skip
	  vector-index-right vector-skip-right
	  vector-binary-search vector-any vector-every
	  vector-set! vector-swap! vector-fill! vector-reverse!
	  vector-copy! vector-reverse-copy! vector-reverse!
	  vector->list reverse-vector->list list->vector reverse-list->vector))

(define-structure srfi-43 srfi-43-interface
  (open (modify scheme
                (rename (vector-fill! %vector-fill!))
                (rename (vector->list %vector->list))
                (rename (list->vector %list->vector)))
	(modify util (rename (unspecific unspecified-value)))
	(subset srfi-8 (receive))
	(subset signals (error)))
  (files srfi-43))

;;; SRFI 66: Octet Vectors

(define-interface srfi-66-interface
  (export make-u8vector
          u8vector?
          list->u8vector u8vector->list
          u8vector
          u8vector-length
          u8vector-ref u8vector-set!
          u8vector-copy! u8vector-copy
          u8vector=?
          u8vector-compare))

(define-structure srfi-66 srfi-66-interface
  (open scheme
        byte-vectors
        (subset primitives (copy-bytes!)))
  (files srfi-66))


;;; SRFI 69: Basic hash tables
;;;
;;; Implement this using the tables module

(define-interface srfi-69-interface
  (export make-hash-table hash-table? alist->hash-table
          ;; hash-table-equivalence-function, hash-table-hash-function
          hash-table-ref hash-table-ref/default
          hash-table-set! hash-table-delete! hash-table-exists?
          ;; hash-table-update! hash-table-update!/default
          hash-table-size hash-table-keys hash-table-values
          hash-table-walk ;; hash-table-fold
          hash-table->alist hash-table-copy ;; hash-table-merge!
          hash string-hash ;; string-ci-hash hash-by-identity
          ))

(define-structure srfi-69 srfi-69-interface
  (open scheme
        (subset signals (error))
        (modify tables (rename (default-hash-function hash)))
        srfi-89)
  (files srfi-69))

;;
;; SRFI-89
;;
(define-structure srfi-89
    (export (define* :syntax))
  (open scheme srfi-1 let-opt)
  (for-syntax (open scheme let-opt (subset signals (syntax-error)) srfi-1))
  (files srfi-89))