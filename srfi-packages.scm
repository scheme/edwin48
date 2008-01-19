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
