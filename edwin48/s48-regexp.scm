;;; -*- mode: scheme; scheme48-package: s48-regexp -*-
;;;
;;; s48-regexp.scm - MIT Scheme Regular Expression compatibility library
;;;
;;; ,open scheme posix-regexps regexps

(define (re-compile-pattern pattern case-fold?)
  (if case-fold?
      (make-regexp pattern (regexp-option ignore-case))
      (make-regexp pattern)))

(define (re-string-match pattern string . case-fold?)
  (let ((regexp (if (regexp? pattern) pattern
                    (re-compile-pattern pattern case-fold?))))
    (regexp-match regexp string 0 #t #t #t)))

(define (re-substring-match pattern string start end . case-fold?)
  (re-string-match pattern (substring string start end) case-fold?))

(define (re-string-search-forward pattern string . case-fold?)
  #f)

(define (re-match-start-index n matches)
  (match-start (list-ref matches n)))

(define (re-match-end-index n matches)
  (match-end (list-ref matches n)))

(define (re-match-extract string regs i)
  (substring string
             (re-match-start-index i regs)
             (re-match-end-index i regs)))

(define (regexp-group . alternatives)
  (apply sequence (map (lambda (i) (text i)) alternatives)))