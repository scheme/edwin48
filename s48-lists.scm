;;; -*- Mode: Scheme; scheme48-package: lists -*-
;;;
;;; s48-lists.scm -
;;;
;;; Implement some MIT Scheme-specific list functions using SRFI-1
;;;

(define (find-matching-item items predicate)
  (find predicate items))

(define (keep-matching-items items predicate)
  (filter predicate items))

(define (delete-matching-items items predicate)
  (remove predicate items))

(define (delq! element items)
  (remove! (lambda (x) (eq? element x)) items))

(define (list-deletor! predicate)
  (lambda (items) (remove! predicate items)))

(define (for-all? items predicate)
  (every predicate items))

(define (there-exists? items predicate)
  (any predicate items))
