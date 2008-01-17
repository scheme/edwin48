;;; -*- Mode: Scheme; scheme48-package: strings -*-
;;;
;;; s48-strings.scm -
;;;
;;; Implement some MIT Scheme-specific string functions using SRFI-13
;;;

(define (substring-move-left! string1 start1 end1 string2 start2)
  (string-copy! string2 start2 string1 start1 end1))

(define (substring-move-right! string1 start1 end1 string2 start2)
  (string-copy! string2 start2 string1 end1 start1))

(define (string-head string end)
  (substring string 0 end))

(define (string-tail string start)
  (substring string start (string-length string)))