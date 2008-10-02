;;; -*- Mode: Scheme; scheme48-package: terminal-mode -*-
;;;
;;; terminal-mode.scm: Terminal mode (based on the MIT Scheme API)
;;;
;;; Copyright © 2007 Duncan Mak <duncan@ccs.neu.edu>
;;;

(define-enumeration terminal-mode (cooked raw))

(define (input-terminal-mode . args)
  (let-optionals args ((port (current-input-port)))
    (let* ((info      (tty-info port))
           (canonical (bitwise-and (tty-info:local-flags info)
                                   ttyl/canonical)))
      (if (zero? canonical) 'raw 'cooked))))

(define (set-input-terminal-mode mode . args)
  (let-optionals args ((port (current-input-port)))
    (if (tty-info? mode)
        (set-tty-info/now port mode)
        (case mode
          ((cooked) (set-tty-info/now port (input-terminal-cooked-mode)))
          ((raw)    (set-tty-info/now port (input-terminal-raw-mode)))
          (else     (format #t "~A is not a valid mode" mode))))))

(define (call-with-input-terminal-mode mode thunk . args)
  (let-optionals args ((port (current-input-port)))
    (let ((orig (tty-info port)))
      (dynamic-wind
        (lambda () (set-input-terminal-mode mode port))
        thunk
        (lambda () (set-input-terminal-mode orig port))))))

(define-syntax with-input-terminal-mode
  (syntax-rules ()
    ((_ port mode body ...)
     (call-with-input-terminal-mode mode
         (lambda () body ...) port))))

(define-syntax with-current-input-terminal-mode
  (syntax-rules ()
    ((_ mode body ...)
     (call-with-input-terminal-mode mode
         (lambda () body ...)))))

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

;; (define (output-terminal-mode . args)
;;   (let-optionals args ((port (current-output-port)))
;;     (let* ((info      (tty-info port))
;;            (canonical (bitwise-and (tty-info:output-flags info)
;;                                    ttyout/enable)))
;;       (if (zero? canonical) 'raw 'cooked))))

;; (define (set-output-terminal-mode mode . args)
;;   (let-optionals args ((port (current-output-port)))
;;     (case mode
;;       ((cooked) (set-tty-info/now port (output-terminal-cooked-mode)))
;;       ((raw)    (set-tty-info/now port (output-terminal-raw-mode)))
;;       (else     (error "This is not a valid terminal mode: " mode)))))

;; (define (call-with-output-terminal-mode mode thunk . args)
;;   (let-optionals args ((port (current-output-port)))
;;     (let ((orig (tty-info port)))
;;       (set-output-terminal-mode mode port)
;;       (thunk)
;;       (set-tty-info/flush port orig))))

;; (define-syntax with-output-terminal-mode
;;   (syntax-rules ()
;;     ((_ port mode body...)
;;      (call-with-output-terminal-mode mode
;;          (lambda () body ...) port))))

;; (define-syntax with-current-output-terminal-mode
;;   (syntax-rules ()
;;     ((_ mode body ...)
;;      (call-with-output-terminal-mode mode
;;          (lambda () body ...)))))

;; (define $output-terminal-cooked-mode (make-fluid (tty-info (current-output-port))))

;; (define (output-terminal-cooked-mode)
;;   (fluid $output-terminal-cooked-mode))

;; (define (output-terminal-raw-mode)
;;   (let* ((info   (output-terminal-cooked-mode))
;;          (ispeed (tty-info:input-speed info))
;;          (ospeed (tty-info:output-speed info))
;;          (min    1)
;;          (time   0))
;;     (make-tty-info
;;      )))