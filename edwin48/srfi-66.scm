; Copyright (c) 1993-2007 by Richard Kelsey and Jonathan Rees. See file COPYING.

; SRFI 66: Octet vectors

(define (make-u8vector k fill)
  (make-byte-vector k fill))

(define u8vector? byte-vector?)

(define (list->u8vector octets)
  (let* ((size (length octets))
	 (v (make-byte-vector size 0)))
    (do ((i 0 (+ 1 i))
	 (l octets (cdr l)))
	((>= i size))
      (byte-vector-set! v i (car l)))
    v))

(define (u8vector->list octets)
  (let loop ((n (byte-vector-length octets)) (r '()))
    (if (zero? n)
	r
	(loop (- n 1) (cons (byte-vector-ref octets (- n 1)) r)))))
  
(define u8vector byte-vector)

(define u8vector-length byte-vector-length)

(define u8vector-ref byte-vector-ref)

(define u8vector-set! byte-vector-set!)

(define (u8vector-copy! source source-start target target-start count)
  (copy-bytes! source source-start target target-start count))

(define (u8vector-copy u8vector)
  (let* ((size (byte-vector-length u8vector))
	 (copy (make-byte-vector size 0)))
    (u8vector-copy! u8vector 0 copy 0 size)
    copy))

(define (u8vector=? u8vector-1 u8vector-2)
  (let ((size (byte-vector-length u8vector-1)))
    (and (= size (byte-vector-length u8vector-2))
	 (let loop ((i 0))
	   (or (>= i size)
	       (and (= (byte-vector-ref u8vector-1 i)
		       (byte-vector-ref u8vector-2 i))
		    (loop (+ 1 i))))))))

(define (u8vector-compare u8vector-1 u8vector-2)
  (let ((length-1 (u8vector-length u8vector-1))
        (length-2 (u8vector-length u8vector-2)))
    (cond
     ((< length-1 length-2) -1)
     ((> length-1 length-2)  1)
     (else
      (let loop ((i 0))
	(if (= i length-1)
	    0
	    (let ((elt-1 (u8vector-ref u8vector-1 i))
		  (elt-2 (u8vector-ref u8vector-2 i)))
	      (cond ((< elt-1 elt-2) -1)
		    ((> elt-1 elt-2)  1)
		    (else (loop (+ i 1)))))))))))
