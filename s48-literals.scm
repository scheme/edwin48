;;; -*- Mode: Scheme; scheme48-package: literals -*-
;;;
;;; Provide additional literals available in MIT Scheme to Scheme48's reader
;;;
;;; ,opens scheme ascii reading srfi-13
;;;

(define *char-name-table*
  '((nul  #x00)
    (null #x00)
    (soh #x01)
    (stx #x02)
    (etx #x03)
    (eot #x04)
    (enq #x05)
    (ack #x06)
    (alarm #x07)
    (bel   #x07)
    (backspace #x08)
    (bs  #x08)
    (tab #x09)
    (vt  #x09)
    (linefeed #x0A)
    (newline  #x0A)
    (lfd  #x0A)
    (lf   #x0A)
    (vtab #x0B)
    (vt   #x0B)
    (page #x0C)
    (ff   #x0C)
    (np   #x0C)
    (return #x0D)
    (ret #x0D)
    (cr  #x0D)
    (so  #x0E)
    (si  #x0F)
    (dle #x10)
    (dc1 #x11)
    (dc2 #x12)
    (dc3 #x13)
    (dc4 #x14)
    (nak #x15)
    (syn #x16)
    (etb #x17)
    (can #x18)
    (em #x19)
    (sub #x1A)
    (call #x1A)
    (esc #x1B)
    (escape #x1B)
    (altmode #x1B)
    (fs #x1C)
    (gs #x1D)
    (rs #x1E)
    (us #x1F)
    (backnext #x1F)
    (space #x20)
    (spc #x20)
    (sp #x20)
    (delete #x7F)
    (del #x7F)
    (rubout #x7F)
    (nbsp #xA0)
    (bom #xFEFF)
    ))

;;;
;;; this originally comes from read.scm
;;;
(define-sharp-macro #\\
  (lambda (c port)
    (read-char port)
    (let ((c (peek-char port)))
      (cond ((eof-object? c)
	     (reading-error port "end of file after #\\"))
	    ((char-alphabetic? c)
	     (let ((name (read port)))
	       (cond ((= (string-length (symbol->string name)) 1)
		      c)
		     ((assq name *char-name-table*) => (lambda (i) (ascii->char (cadr i))))
		     (else
		      (reading-error port "unknown #\\ name" name)))))
	    (else
	     (read-char port)
	     c)))))

;;; From scsh's modified read.scm
(define (multi-line-comment-skip c port)
  (read-char port)
  (let lp ((state 0) (nested? #f))
    (let* ((advance-one-of-two
	    (lambda (look-for1 state1 look-for2 state2 nested?)
			  (let ((c (read-char port)))
			    (if (eof-object? c)
				(reading-error port "EOF inside block comment -- #| missing a closing |#")
				(lp (cond ((char=? c look-for1) state1)
					  ((char=? c look-for2) state2)
					  (else 0)) nested?)))))
	   (advance-if (lambda (look-for state nested?)
			 (advance-one-of-two look-for state
					     look-for state
					     nested?))))
      (case state
	((0) (advance-one-of-two #\| 1 #\# 5 nested?))
	((1) (advance-if #\# 2 nested?))
	((2) (if nested? #f (read port)))
	((5) (advance-if #\| 6 nested?))
	((6) (lp 0 #t) (lp 0 nested?))))))

(define-sharp-macro #\| multi-line-comment-skip)