;;; -*- Mode: Scheme; scheme48-package: strings -*-
;;;
;;; s48-strings.scm -
;;;
;;; Implement some MIT Scheme-specific string functions using SRFI-13
;;;

(define (%match-char-ci char)
  (lambda (c) (char-ci=? c char)))

(define (string-capitalize s) (string-titlecase s))

(define (string-find-next-char s char)
  (string-index s char))

(define (substring-find-next-char s start end char)
  (string-index s char start end))

(define (string-find-next-char-ci s char)
  (string-index s (%match-char-ci char)))

(define (substring-find-next-char-ci s start end char)
  (string-index s (%match-char-ci char) start end))

(define (string-find-next-char-in-set s charset)
  (string-index s charset))

(define (substring-find-next-char-in-set s start end charset)
  (string-index s charset start end))

(define (string-find-previous-char s char)
  (string-index-right s char))

(define (substring-find-previous-char s start end char)
  (string-index-right s char start end))

(define (string-find-previous-char-ci s char)
  (string-index-right s (%match-char-ci char)))

(define (substring-find-previous-char-ci s start end char)
  (string-index-right s (%match-char-ci char) start end))

(define (string-find-previous-char-in-set s charset)
  (string-index-right s charset))

(define (substring-find-previous-char-in-set s start end charset)
  (string-index-right s charset start end))

(define (substring-prefix-ci? s1 start1 end1 s2 start2 end2)
  (string-prefix-ci? s1 s2 start1 end1 start2 end2))

(define (string-match-forward s1 s2)
  (string-prefix-length s1 s2))

(define (substring-match-forward-ci s1 start1 end1 s2 start2 end2)
  (string-prefix-length-ci s1 s2 start1 end1 start2 end2))

(define (substring-move-left! string1 start1 end1 string2 start2)
  (string-copy! string2 start2 string1 start1 end1))

(define (substring-move-right! string1 start1 end1 string2 start2)
  (string-copy! string2 start2 string1 end1 start1))

(define (substring-fill! s start end char)
  (string-fill! s char start end))
