;;; -*- Mode: Scheme; scheme48-package: tests:comtab -*-
;;;
;;; Comtab tests
;;;

; Running in Scheme48:
; Run in edwin48/tests/
; ,exec ,load load.scm
; ,config ,open edwin:command-table

;;; On the first day he created comtabs
(define ass-mode (make-comtab))
(check (comtab? ass-mode) => #t)

(define fundamental (make-comtab))
(check (comtab? fundamental) => #t)
;;; and it was good ...


;;; On the second day he created commands
(define-command do-something "Does something" () (lambda () (display "Did something\n")))
(check (command? (name->command 'do-something)) => #t)

(define-command do-nothing "Does nada" () (lambda () (display "Did nothing\n")))
(check (command? (name->command 'do-nothing)) => #t)

(define-command do-me "Does Moi" () (lambda () (display "Did Moi\n")))
(check (command? (name->command 'do-me)) => #t)

(define-command do-be "Or Not To Be" () (lambda () (display "To Be\n")))
(check (command? (name->command 'do-be)) => #t)

(define-command prefix-key "Loops back for more keys" () (lambda () (display "Keyboard HUNGRY!\n")))
(check (command? (name->command 'prefix-key)) => #t)
;;; and it was good ...


;;; On the third day he created keys
(define-key fundamental (kbd #\s) 'do-something)
(check (comtab-get fundamental (kbd #\s)) => (name->command 'do-something))

(define-key ass-mode (kbd (ctrl #\s)) 'do-something)
(check (comtab-get ass-mode (kbd (ctrl #\s))) => (name->command 'do-something))

(define-key fundamental (kbd #\n) 'do-nothing)
(check (comtab-get fundamental (kbd #\n)) => (name->command 'do-nothing))

(define-key fundamental (kbd (ctrl #\n)) 'do-nothing)
(check (comtab-get fundamental (kbd (ctrl #\n))) => (name->command 'do-nothing))

(define-key ass-mode (kbd (ctrl #\q)) 'do-nothing)
(check (comtab-get ass-mode (kbd (ctrl #\q))) => (name->command 'do-nothing))

(define-key fundamental (kbd #\m) 'do-me)
(check (comtab-get fundamental (kbd #\m)) => (name->command 'do-me))

(define-key ass-mode (kbd #\n) 'do-be)
(check (comtab-get ass-mode (kbd #\n)) => (name->command 'do-be))
;;; and it was good ...


;;; On the forth day he created prefix keys
(define-prefix-key ass-mode (kbd (ctrl #\x)))
(check (comtab? (hash-table-ref (comtab-table ass-mode) (kbd (ctrl #\x)))) => #t)

(define-command find-file "Find file" () (lambda () (display "find\n")))
(check (command? (name->command 'find-file)) => #t)

(define-prefix-key ass-mode (kbd (ctrl #\x) #\v))
(check (comtab? (comtab-get ass-mode (kbd (ctrl #\x) #\v))) => #t)

(define-command vc-dir "Version Control Dir" () (lambda () (display "VC Dir\n")))
(check (command? (name->command 'vc-dir)) => #t)

(define-key ass-mode (kbd (ctrl #\x) #\v #\v) 'vc-dir)
(check (comtab-get ass-mode (kbd (ctrl #\x) #\v #\v)) => (name->command 'vc-dir))
;;; and it was good ...


;;; On the fifth day he asked for help
(check (comtab-key-bindings (list ass-mode fundamental) (name->command 'do-nothing))
       (=> key=?)
       (list (kbd (ctrl #\n)) (kbd (ctrl #\q))))
;;; The order of the comtabs determines the shadowing. So =>
;;; C-q ass-mode
;;; C-n fundamental ('do-be in ass-mode shadows (kbd n) in fundamental)

(check (comtab-key-bindings (list fundamental ass-mode) (name->command 'do-nothing))
       (=> key=?)
       (list (kbd #\n) (kbd (ctrl #\n)) (kbd (ctrl #\q))))
;;; Flipping them gets us different results =>
;;; n fundamental
;;; C-n fundamental
;;; C-q ass-mode


(check-report)