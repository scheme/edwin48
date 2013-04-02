;;; -*- Mode: Scheme; scheme48-package: cosmacs -*-
;;;
;;; cosmacs.scm: Dumb Terminal Editor For Edwin
;;;
;;; Copyright © 2010 Jeff Dlouhy <jeff.dlouhy@gmail.com>
;;;

;;; Swiped From Linedit's terminal-mode.scm
(define (input-terminal-cooked-mode)
  (make-tty-info 25862 5 1215 35387 'extb 'extb 1 0))

(define (input-terminal-raw-mode)
  (let* ((info   (input-terminal-cooked-mode))
         (ispeed (tty-info:input-speed info))
         (ospeed (tty-info:output-speed info))
         (min    1)
         (time   0))
    (make-tty-info
     ;; c_iflag &= ~(IGNBRK|BRKINT|PARMRK|ISTRIP|INLCR|IGNCR|ICRNL|IXON);
     (bitwise-and
      (tty-info:input-flags info)
      (bitwise-not
       (bitwise-ior ttyin/ignore-break
                    ttyin/interrupt-on-break
                    ttyin/mark-parity-errors
                    ttyin/7bits ttyin/nl->cr
                    ttyin/ignore-cr ttyin/cr->nl
                    ttyin/output-flow-ctl)))

     (tty-info:output-flags info)

     ;; c_cflag &= ~(CSIZE|PARENB);
     (bitwise-and
      (tty-info:control-flags info)
      (bitwise-not
       (bitwise-ior ttyc/char-size
                    ttyc/enable-parity)))

     ;; c_lflag &= ~(ECHO|ECHONL|ICANON|ISIG|IEXTEN);
     (bitwise-and
      (tty-info:local-flags info)
      (bitwise-not
       (bitwise-ior ttyl/echo ttyl/echo-nl
                    ttyl/canonical
                    ttyl/enable-signals
                    ttyl/extended)))

     ispeed ospeed min time)))

(define (make-cosmacs-port tty-port)
  (let ((oport (dup->outport tty-port)))
    (set-tty-info/now tty-port (input-terminal-raw-mode))
;    (set-port-buffering oport bufpol/none)
    tty-port))

(define (wait-for-key . optionals)
  (let* ((tty-port (current-input-port))
         (old (tty-info))
         (copy (copy-tty-info old)))
    (set-tty-info:local-flags
     copy
     (bitwise-and (tty-info:local-flags copy)
                  (bitwise-not ttyl/canonical)))
    (set-tty-info:min copy 1)
    (set-tty-info:time copy 0)
    (set-tty-info/now tty-port copy)
    (let ((c (read-char tty-port)))
      (set-tty-info/now tty-port old)
      c)))

;;; Is this a ASCII control char?
;;;
;;; ascii-controlified?  char -> boolean
;;;
(define (ascii-controlified? char)
    (< (char->integer char) 32)) ;was fix:<

;;; Read from standard in and return a kbd
;;;
;;; %read-char  input-port -> keystroke
;;;
(define (%read-char port-data)
  (let lp ()
    (let* ((c (read-char port-data))
           (key (make-key c)))
      (set! last-command-key c)
      key)))

;;; Given a key, evaluate the procedure if
;;; there is a command with that keystroke
(define (dispatch-on-key comtab keystroke)
  (let* ((command   (comtab-entry comtab keystroke))
         (name (command-name command))
         (procedure (command-procedure command)))
;    (format #t "INTEGER: ~A KEY: ~A KEYSTROKE: ~A~%COMMAND: ~A" (char->integer last-command-key) last-command-key (key->string keystroke) name)
    (procedure)
))

(define (start)
  (write-string "Welcome To Cosmetic EMACS")
  (let ((command-port (make-cosmacs-port (current-input-port))))
    (let lp ()
      (write-string "\r\n? ")
      (force-output)
      (let ((val (%read-char command-port)))
        (cond ((not (eq? val 'quit))
               (newline)
               (dispatch-on-key 'fundamental val)
               (lp)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Modes

(define-command fundamental-mode
  "Make the current mode be Fundamental Mode.
All normal editing modes are defined relative to this mode."
  ()
  (lambda ()
    (set-current-major-mode! (ref-mode-object fundamental))))

(define-major-mode fundamental #f "Fundamental"
  "Major mode not specialized for anything in particular.
Most other major modes are defined by comparison to this one.")

(define initial-buffer-name
  "*scheme*")

;; The extra range allows international keyboards to insert 8-bit characters
(define char-set:self-insert-keys
  (char-set-union char-set:digit (ucs-range->char-set 128 255)))

(define last-command-key (kbd space))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Commands

(define-command self-insert-command
  "Insert the character you type.
Whichever character you type to run this command is inserted."
  "P"
  (lambda (argument)
    (let ((char (key-value last-command-key)))
      (if (not (char? char))
          (error "self-insert-command only works on character keys." char))
      (write char))))


(define-command delete-backward-char
  "Delete character before point.
With argument, kills several characters (saving them).
Negative args kill characters forward."
  "P"
  (lambda (argument)
    (write-string "RUBOUT....\r")))


(define-command forward-char
  "Move forward one character.
With argument, move that many characters forward.
Negative args move backward."
  "p"
  (lambda (argument)
    (write-string "move forward char")))

(define-key 'fundamental char-set:self-insert-keys 'self-insert-command)
(define-key 'fundamental char-set:letter 'self-insert-command)
;(define-key 'fundamental char-set:digit 'auto-digit-argument)
;(define-key 'fundamental (kbd #\-) 'auto-negative-argument)
(define-key 'fundamental (kbd #\rubout) 'delete-backward-char)
(define-key 'fundamental (kbd backspace) 'delete-backward-char)
(define-key 'fundamental (kbd delete) 'delete-backward-char)
;(define-key 'fundamental (kbd #\s) 'delete-backward-char)
(define-key 'fundamental (kbd (ctrl #\f)) 'forward-char)

(define-command do-something "Does something" () (lambda () (write-string "Did something\n")))
;(define fundamental (make-comtab))
(define-key 'fundamental (kbd #\s) 'do-something)


