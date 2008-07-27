;;; -*- Mode: Scheme; scheme48-package: char-support -*-
;;;
;;; Copies of some procedures on characters from MIT Scheme
;;;

(define char-set/atom-delimiters
  (char-set-union char-set:whitespace
                  ;; Note that #\, may break older code.
                  (string->char-set "()[]{}\";'`,")
                  ;; (char-set #\U+00AB #\U+00BB)
                  ))

(define (->name entries n)
  (let ((entry (assv n entries)))
    (and entry
         (cadr entry))))

(define* (char->name char (slashify? #f))
  (let ((code (char->ascii char)))
    (string-append
     (let ((base-char (if (fix:= 0 bits) char (ascii->char code))))
       (cond ((->name named-codes code))
	     ((and slashify?
		   (not (fix:= 0 bits))
		   (or (char=? base-char #\\)
		       (char-set-member? char-set/atom-delimiters base-char)))
	      (string-append "\\" (string base-char)))
	     ((char-graphic? base-char)
	      (string base-char))
	     (else
	      (string-append "U+"
			     (let ((s (number->string code 16)))
			       (string-pad s
                                           (let ((l (string-length s)))
                                             (let loop ((n 2))
                                               (if (fix:<= l n)
                                                   n
                                                   (loop (fix:* 2 n)))))
                                           #\0)))))))))

(define named-codes
  '((#x00 #f "null" "nul")
    (#x01 #f "soh")
    (#x02 #f "stx")
    (#x03 #f "etx")
    (#x04 #f "eot")
    (#x05 #f "enq")
    (#x06 #f "ack")
    (#x07 #f "bel")
    (#x08 "backspace" "bs")
    (#x09 "tab" "ht")
    (#x0A "newline" "linefeed" "lfd" "lf")
    (#x0B #f "vt")
    (#x0C "page" "ff" "np")
    (#x0D "return" "ret" "cr")
    (#x0E #f "so")
    (#x0F #f "si")
    (#x10 #f "dle")
    (#x11 #f "dc1")
    (#x12 #f "dc2")
    (#x13 #f "dc3")
    (#x14 #f "dc4")
    (#x15 #f "nak")
    (#x16 #f "syn")
    (#x17 #f "etb")
    (#x18 #f "can")
    (#x19 #f "em")
    (#x1A #f "sub" "call")
    (#x1B "escape" "esc" "altmode")
    (#x1C #f "fs")
    (#x1D #f "gs")
    (#x1E #f "rs")
    (#x1F #f "us" "backnext")
    (#x20 "space" "spc" "sp")
    (#x7F "delete" "del" "rubout")
    (#xA0 "nbsp")
    (#xFEFF "bom")))
