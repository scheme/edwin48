#| -*-Scheme-*-

$Id: tterm.scm,v 1.46 2008/01/30 20:02:06 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Terminfo(3) Screen Implementation


(define (make-console-screen)
  (let ((description (setup-terminal)))
    (cond ((not (sufficiently-powerful? description))
           (error "terminal type not powerful enough"
                  (car (terminal:names description))))
          ((not (no-undesirable-characteristics? description))
           (error "terminal type has undesirable characteristics"
                  (car (terminal:names description)))))
    (let ((baud-rate (terminal:baud-rate description))
          (x-size    (terminal:x-size    description))
          (y-size    (terminal:y-size    description)))
      (make-screen (call-with-values
                       (lambda ()
                         (compute-scrolling-costs description
                                                  baud-rate
                                                  x-size
                                                  y-size))
                     (lambda (insert-line-cost
                              insert-line-next-cost
                              delete-line-cost
                              delete-line-next-cost
                              scroll-region-cost)
                       (make-terminal-state description
                                            baud-rate
                                            insert-line-cost
                                            insert-line-next-cost
                                            delete-line-cost
                                            delete-line-next-cost
                                            scroll-region-cost
                                            (make-key-table description))))
                   console-beep
                   console-clear-line!
                   console-clear-rectangle!
                   console-clear-screen!
                   console-discard!
                   console-enter!
                   console-exit!
                   console-flush!
                   console-modeline-event!
                   console-scroll-lines-down!
                   console-scroll-lines-up!
                   console-wrap-update!
                   console-write-char!
                   console-write-cursor!
                   console-write-substring!
                   (fix:1+ (fix:quotient baud-rate 2400))
                   x-size
                   y-size))))


(define (console-available?)
  (let ((description (setup-terminal)))
    (and (sufficiently-powerful? description)
         (no-undesirable-characteristics? description))))

(define (sufficiently-powerful? description)
  (let ((x-size (terminal:x-size description))
        (y-size (terminal:y-size description)))
    (and (and x-size (> x-size 0))
         (and y-size (> y-size 0))
         (capability-available? cursor-address description))))

(define (no-undesirable-characteristics? description)
  (not (or (tilde-glitch description)
           (dest-tabs-magic-smso description)
           (transparent-underline description))))

;;; Returns an alist from escape sequences (string)
;;; to keystroke records.
(define (make-key-table description)
  (let ((x description))
    (map (lambda (name+key-seq)
           (cons (cdr name+key-seq)
                 (make-key (cdr name+key-seq)
                           '()
                           (car name+key-seq))))
         `((up    . ,(key-up    x))
           (down  . ,(key-down  x))
           (left  . ,(key-left  x))
           (right . ,(key-right x))
           (f1    . ,(key-f1    x))
           (f2    . ,(key-f2    x))
           (f3    . ,(key-f3    x))
           (f4    . ,(key-f4    x))
           (f5    . ,(key-f5    x))
           (f6    . ,(key-f6    x))
           (f7    . ,(key-f7    x))
           (f8    . ,(key-f8    x))
           (f9    . ,(key-f9    x))
           (f10   . ,(key-f10   x))
           (f11   . ,(key-f11   x))
           (f12   . ,(key-f12   x))))))

(define (get-console-input-operations terminal-state)
  (let ((port   console-input-port)
        (string (make-string (* 3 input-buffer-size)))
        (start  0)
        (end    0)
        (incomplete-pending #F)
        (timeout-interval 1000)         ; 1s. Should be f(baud rate) etc
        (len     0))                    ; length of event in input characters
    ;; When the input is a prefix of the character sequence sent by some key
    ;; we are prepared to wait a little while to see if the rest of
    ;; the sequence arrives.  INCOMPLETE-PENDING is either #F, the
    ;; real time at which we timeout for waiting for the sequence to
    ;; complete, or #T if a timeout occured.
    (letrec
        ((parse-key                     ; -> #F or a keystroke
          (lambda ()
            (and (fix:< start end)
                 terminal-state
                 (let ((n-chars  (fix:- end start)))
                   (let find
                       ((key-pairs (terminal-state-key-table terminal-state))
                        (possible-pending? #F))
                     (if (null? key-pairs)
                         (begin
                           (if (number? incomplete-pending)
                               (if (or (not possible-pending?)
                                       (> (real-time-clock)
                                          incomplete-pending))
                                   (set! incomplete-pending #T)))
                           (if (number? incomplete-pending)
                               #F
                               (begin
                                 (set! len 1)
                                 ;; We must explicitly map the 8th bit
                                 ;; of an incoming character to the
                                 ;; meta bit.
                                 (let ((code (vector-8b-ref string start)))
                                   (if (fix:< code #x80)
                                       (make-key code)
                                       (make-key (fix:and code #x7F)
                                                 (key-modifier meta)))))))
                         (let* ((key-seq  (caar key-pairs))
                                (n-seq    (string-length key-seq)))
                           (cond ((and (fix:<= n-seq n-chars)
                                       (string= string key-seq
                                                start (fix:+ start n-seq)
                                                0 n-seq))
                                  (set! len n-seq)
                                  (cdar key-pairs))
                                 ((and (fix:> n-seq n-chars)
                                       (string= string key-seq
                                                start (fix:+ start n-chars)
                                                0 n-chars))
                                  (if (not incomplete-pending)
                                      (set! incomplete-pending
                                            (+ (real-time-clock)
                                               timeout-interval)))
                                  (find (cdr key-pairs) #t))
                                 (else
                                  (find (cdr key-pairs)
                                        possible-pending?))))))))))
         (read-more?                    ; -> #F or #T is some chars were read
          (lambda ()
            (let ((n (read-string!/partial string port end input-buffer-size)))
              (cond ((not n)  #F)
                    ((fix:> n 0)
                     (set! end (fix:+ end n))
                     #T)
                    ((fix:= n 0)
                     ;;(error "Reached EOF in keyboard input.")
                     #F)
                    (else
                     (error "Illegal return value:" n))))))
         (read-char
          (lambda ()
            (if (read-more?)
                (parse-key)
                #F)))
         (read-event
          (lambda (block?)
            (or (read-char)
                (let loop ()
                  (cond ;; REPL-related
                        ;; (inferior-thread-changes? event:interrupt)
                        ;; TODO, see process.scm
                        ;; ((process-output-available?) event:process-output) 
                        ((input-available-on-port? port block?)
                         (read-event block?))
                        (else #f))))))
         (guarantee-result
          (lambda ()
            (let ((event (read-event #t)))
              (cond ((char? event) event)
                    ((special-key? event) event)
                    ((process-change-event event)
                     => (lambda (flag)
                          (make-input-event
                           (if (eq? flag 'FORCE-RETURN) 'RETURN 'UPDATE)
                           update-screens! #f)))
                    (else (guarantee-result))))))
         (consume!
          (lambda (bytes)
            (set! start (fix:+ start bytes))
            (cond ((fix:>= start end)   ; all consumed
                   (set! end 0)
                   (set! start 0))
                  ((fix:>= start input-buffer-size)
                   (string-copy! string 0 string start end)
                   (set! end (fix:- end start))
                   (set! start 0)))
            (set! incomplete-pending #F)
            unspecific)))
      (values
       (lambda ()                       ;halt-update?
         (or (fix:< start end)
             (read-char)))
       (lambda ()                       ;peek-no-hang
         (or (parse-key)
             (let ((event (read-event #f)))
               (if (fix:fixnum? event)
                   (begin
                     (process-change-event event)
                     #f)
                   event))))
       (lambda ()                       ;peek
         (or (parse-key)
             (guarantee-result)))
       (lambda ()                       ;read
         (let ((event (or (parse-key) (guarantee-result))))
           (consume! len)
           event))))))


(define input-buffer-size 16)
(define event:process-output -2)
(define event:process-status -3)
(define event:interrupt -4)

(define (process-change-event event)
  (cond ((fix:= event event:process-output) (accept-process-output))
        ((fix:= event event:process-status) (handle-process-status-changes))
        ((fix:= event event:interrupt) (accept-thread-output))
        (else (error "Illegal change event:" event))))

(define (signal-interrupt!)
  (signal-thread-event editor-thread
    (lambda ()
      ;; (editor-beep)                  ; kbd beeps by itself
      (temporary-message "Quit")
      (^G-signal))))

(define (with-console-interrupts-enabled thunk)
  (with-console-interrupt-state #t thunk))

(define (with-console-interrupts-disabled thunk)
  (with-console-interrupt-state #f thunk))

(define (with-console-interrupt-state inside thunk)
  (let ((outside unspecific))
    (dynamic-wind (lambda ()
                    (set! outside (terminal-get-interrupt-char))
                    (terminal-set-interrupt-char! inside))
                  thunk
                  (lambda ()
                    (set! inside (terminal-get-interrupt-char))
                    (terminal-set-interrupt-char! outside)))))

(define console-display-type
  (make-display-type 'CONSOLE
                     #f
                     console-available?
                     make-console-screen
                     (lambda (screen)
                       (get-console-input-operations
                        (screen-state screen)))
                     with-console-grabbed
                     with-console-interrupts-enabled
                     with-console-interrupts-disabled))

(define console-description 'UNKNOWN)


;;;
;;; This procedure first sets the console to raw mode, and sets up a
;;; DYNAMIC-WIND wrapper around 'receiver' so that its argument,
;;; 'thunk', is always executed in the original console state (before
;;; setting it to raw mode).
;;;
(define (with-console-grabbed receiver)
  (bind-console-state #f
    (lambda (get-outside-state)
      (terminal-operation terminal-raw-input
                          console-input-port)
      (terminal-operation terminal-raw-output
                          console-output-port)
      (terminal-set-interrupt-char! #t)
      (receiver
       (lambda (thunk)
         (bind-console-state (get-outside-state)
           (lambda (get-inside-state)
             get-inside-state
             (thunk))))
       `((INTERRUPT/ABORT-TOP-LEVEL ,signal-interrupt!))))))

;;;
;;; This sets up a barrier using DYNAMIC-WIND to ensure that the state
;;; of the console will be properly restored if a continuation
;;; escapes out.
;;;
(define (bind-console-state inside-state receiver)
  (let ((outside-state unspecific))
    (dynamic-wind (lambda ()
                    (set! outside-state (console-state))
                    (if inside-state
                        (set-console-state! inside-state)))
                  (lambda ()
                    (receiver (lambda () outside-state)))
                  (lambda ()
                    (set! inside-state (console-state))
                    (set-console-state! outside-state)))))

(define (console-state)
  (vector (port-state console-input-port)
          (port-state console-output-port)
          (terminal-get-interrupt-char)))

(define (set-console-state! state)
  (set-port-state! console-input-port
                   (vector-ref state 0))
  (set-port-state! console-output-port
                   (vector-ref state 1))
  (terminal-set-interrupt-char! (vector-ref state 2)))

(define (port-state port)
  (and port
       (tty? port)
       (tty-info port)))

(define (set-port-state! port state)
  (if (and port
           (tty? port)
           state)
      (set-tty-info/flush port state)))

(define (terminal-operation operation port)
  (if (and port
           (tty? port))
      (operation port)))

;;;; Terminal State

(define-record-type* terminal-state
  (make-terminal-state description
                       baud-rate
                       insert-line-cost
                       insert-line-next-cost
                       delete-line-cost
                       delete-line-next-cost
                       scroll-region-cost
                       (key-table))
  (cursor-x
   cursor-y
   standout-mode?
   insert-mode?
   delete-mode?
   scroll-region))

(define (accessor procedure)
  (lambda (screen)
    (procedure (screen-state screen))))

(define screen-description            (accessor terminal-state-description))
(define screen-baud-rate              (accessor terminal-state-baud-rate))
(define screen-insert-line-cost       (accessor terminal-state-insert-line-cost))
(define screen-insert-line-next-cost) (accessor terminal-state-insert-line-next-cost)
(define screen-delete-line-cost       (accessor terminal-state-delete-line-cost))
(define screen-delete-line-next-cost  (accessor terminal-state-delete-line-next-cost))
(define screen-scroll-region-cost     (accessor terminal-state-scroll-region-cost))
(define screen-cursor-x               (accessor terminal-state-cursor-x))
(define set-screen-cursor-x!          (accessor set-terminal-state-cursor-x!))
(define screen-cursor-y               (accessor terminal-state-cursor-y))
(define set-screen-cursor-y!          (accessor set-terminal-state-cursor-y!))
(define screen-standout-mode?         (accessor terminal-state-standout-mode?))
(define set-screen-standout-mode?!    (accessor set-terminal-state-standout-mode?!))
(define screen-insert-mode?           (accessor terminal-state-insert-mode?))
(define set-screen-insert-mode?!      (accessor set-terminal-state-insert-mode?!))
(define screen-delete-mode?           (accessor terminal-state-delete-mode?))
(define set-screen-delete-mode?!      (accessor set-terminal-state-delete-mode?!))
(define screen-scroll-region          (accessor terminal-state-scroll-region))
(define set-screen-scroll-region!     (accessor set-terminal-state-scroll-region!))

(define (insert/delete-line-ok? description)
  (or (and (or (capability-available? insert-line          description)
               (capability-available? parm-insert-line     description))
           (or (capability-available? delete-line          description)
               (capability-available? parm-delete-line     description)))
      (and (or (capability-available? change-scroll-region description)
               (capability-available? set-window           description))
           (or (capability-available? scroll-forward       description)
               (capability-available? parm-index           description))
           (or (capability-available? scroll-reverse       description)
               (capability-available? parm-rindex          description)))))

(define (scroll-region-ok? description)
  (or (capability-available? change-scroll-region description)
      (capability-available? set-window           description)))


;;;; Console Screen Operations

(define (console-discard! screen)
  screen
  (set! console-description 'UNKNOWN)
  unspecific)

(define (console-enter! screen)
  (add-event-receiver! event:console-resize resize-screen)
  (maybe-output screen (enter-ca-mode (screen-description screen)))
  (set-screen-cursor-x! screen #f)
  (set-screen-cursor-y! screen #f))

(define (console-exit! screen)
  (remove-event-receiver! event:console-resize resize-screen)
  (let ((description (screen-description screen)))
    (move-cursor screen 0 (fix:-1+ (screen-y-size screen)))
    (do-exit-standout-mode screen)
    (do-exit-insert-mode screen)
    (maybe-output screen (exit-ca-mode description)))
  (flush-tty/output console-output-port))

(define (console-modeline-event! screen window type)
  screen window type
  unspecific)

(define (console-wrap-update! screen thunk)
  screen
  (let ((finished? (thunk)))
    (flush-tty/output console-output-port)
    finished?))

(define (console-beep screen)
  (output-1 screen (bell (screen-description screen))))

(define (console-flush! screen)
  screen
  (flush-tty/output console-output-port))

(define (console-write-cursor! screen x y)
  (move-cursor screen x y))

(define (console-write-char! screen x y char highlight)
  (if (let ((description (screen-description screen)))
        (not (and (auto-right-margin description)
                  (fix:= x (fix:-1+ (screen-x-size screen)))
                  (fix:= y (fix:-1+ (screen-y-size screen))))))
      (begin
        (do-exit-insert-mode screen)
        (move-cursor screen x y)
        (highlight-if-desired screen highlight)
        (write-char char console-output-port)
        (record-cursor-after-output screen (fix:1+ x)))))

(define (console-write-substring! screen x y string start end highlight)
  (if (fix:< start end)
      (begin
        (do-exit-insert-mode screen)
        (move-cursor screen x y)
        (highlight-if-desired screen highlight)
        (let ((end
               (if (let ((description (screen-description screen)))
                     (and (auto-right-margin description)
                          (fix:= y (fix:-1+ (screen-y-size screen)))
                          (fix:= (fix:+ x (fix:- end start))
                                 (screen-x-size screen))))
                   (fix:-1+ end)
                   end)))
          (write-string (substring string start end) console-output-port)
          (record-cursor-after-output screen (fix:+ x (fix:- end start)))))))

(define (console-clear-line! screen x y first-unused-x)
  (move-cursor screen x y)
  (clear-line screen first-unused-x))

(define (console-clear-screen! screen)
  (clear-screen screen))

(define (console-clear-rectangle! screen xl xu yl yu highlight)
  highlight
  (let ((x-size (screen-x-size screen))
        (y-size (screen-y-size screen)))
    (cond ((not (fix:= xu x-size))
           (let ((n (fix:- xu xl)))
             (do ((y yl (fix:1+ y)))
                 ((fix:= y yu))
               (move-cursor screen xl y)
               (clear-multi-char screen n))))
          ((fix:= yl (fix:1+ yu))
           (move-cursor screen xl yl)
           (clear-line screen x-size))
          ((and (fix:= xl 0) (fix:= yu y-size))
           (if (fix:= yl 0)
               (clear-screen screen)
               (begin
                 (move-cursor screen 0 yl)
                 (clear-to-bottom screen))))
          (else
           (do ((y yl (fix:1+ y)))
               ((fix:= y yu))
             (move-cursor screen xl y)
             (clear-line screen x-size))))))

(define (console-scroll-lines-down! screen xl xu yl yu amount)
  (let ((description (screen-description screen)))
    (and (insert/delete-line-ok? description)
         (fix:= xl 0)
         (fix:= xu (screen-x-size screen))
         (let ((y-size (screen-y-size screen))
               (yu* (fix:- yu amount)))
           (let ((draw-cost (scroll-draw-cost screen yl yu*)))
             (if (or (fix:= yu y-size)
                     (scroll-region-ok? description))
                 (and (fix:< (insert-lines-cost screen yl yu amount) draw-cost)
                      (begin
                        (insert-lines screen yl yu amount)
                        'CLEARED))
                 (and (fix:<
                       (fix:+ (delete-lines-cost screen yu* y-size amount)
                              (insert-lines-cost screen yl y-size amount))
                       draw-cost)
                      (begin
                        (delete-lines screen yu* y-size amount)
                        (insert-lines screen yl y-size amount)
                        'CLEARED))))))))

(define (console-scroll-lines-up! screen xl xu yl yu amount)
  (let ((description (screen-description screen)))
    (and (insert/delete-line-ok? description)
         (fix:= xl 0)
         (fix:= xu (screen-x-size screen))
         (let ((y-size (screen-y-size screen))
               (draw-cost (scroll-draw-cost screen (fix:+ yl amount) yu)))
           (if (or (fix:= yu y-size)
                   (scroll-region-ok? description))
               (and (fix:< (delete-lines-cost screen yl yu amount) draw-cost)
                    (begin
                      (delete-lines screen yl yu amount)
                      'CLEARED))
               (let ((yu* (fix:- yu amount)))
                 (and (fix:<
                       (fix:+ (delete-lines-cost screen yl y-size amount)
                              (insert-lines-cost screen yu* y-size amount))
                       draw-cost)
                      (begin
                        (delete-lines screen yl y-size amount)
                        (insert-lines screen yu* y-size amount)
                        'CLEARED))))))))

(define (scroll-draw-cost screen yl yu)
  (do ((yl yl (fix:+ yl 1))
       (cost 0 (fix:+ cost (screen-line-draw-cost screen yl))))
      ((fix:= yl yu) cost)))

;;;; Terminfo Commands

(define (clear-screen screen)
  (let ((description (screen-description screen)))
    (let ((ts-clear-screen (capability-available? clear-screen description)))
      (if ts-clear-screen
          (begin
            (do-exit-standout-mode screen)
            (output-n screen (clear-screen description) (screen-y-size screen))
            (set-screen-cursor-x! screen 0)
            (set-screen-cursor-y! screen 0))
          (begin
            (move-cursor screen 0 0)
            (clear-to-bottom screen))))))

(define (clear-to-bottom screen)
  (let ((description (screen-description screen)))
    (let ((ts-clear-to-bottom (capability-available? clr-eos description)))
      (if ts-clear-to-bottom
          (begin
            (do-exit-standout-mode screen)
            (output screen (clr-eos description)))
          (let ((x-size (screen-x-size screen))
                (y-size (screen-y-size screen)))
            (do ((y (screen-cursor-y screen) (fix:1+ y)))
                ((fix:= y y-size))
              (move-cursor screen 0 y)
              (clear-line screen x-size)))))))

(define (clear-line screen first-unused-x)
  (do-exit-standout-mode screen)
  (let* ((description (screen-description screen))
         (output-port (terminal:port description)))
    (if (capability-available? clr-eol description)
        (output-1 screen (clr-eol))
        (begin
          (do-exit-insert-mode screen)
          (let ((first-unused-x
                 (if (and (auto-right-margin description)
                          (fix:= first-unused-x (screen-x-size screen))
                          (fix:= (screen-cursor-y screen)
                                 (fix:-1+ (screen-y-size screen))))
                     (fix:-1+ first-unused-x)
                     first-unused-x)))
            (do ((x (screen-cursor-x screen) (fix:1+ x)))
                ((fix:= x first-unused-x))
              (write-char #\space output-port))
            (record-cursor-after-output screen first-unused-x))))))

(define (clear-multi-char screen n)
  (do-exit-standout-mode screen)
  (let ((description (screen-description screen)))
    (let ((ts-clear-multi-char (capability-available? erase-chars description)))
      (if ts-clear-multi-char
          (output-1 screen (erase-chars n description))
          (begin
            (do-exit-insert-mode screen)
            (let ((cursor-x (screen-cursor-x screen)))
              (let ((x-end
                     (let ((x-end (fix:+ cursor-x n))
                           (x-size (screen-x-size screen)))
                       (if (fix:> x-end x-size)
                           (error "can't clear past end of line"))
                       (if (and (fix:= x-end x-size)
                                (auto-right-margin description)
                                (fix:= (screen-cursor-y screen)
                                       (fix:-1+ (screen-y-size screen))))
                           (fix:-1+ x-size)
                           x-end))))
                (do ((x cursor-x (fix:1+ x)))
                    ((fix:= x x-end))
                  (write-char #\space console-output-port))
                (record-cursor-after-output screen x-end))))))))

(define (insert-lines screen yl yu n)
  (let ((y-size (screen-y-size screen))
        (description (screen-description screen))
        (n-lines (fix:- yu yl)))
    (cond ((capability-available? insert-line description)
           =>
           (lambda (ts-insert-line)
             (if (not (fix:= yu y-size))
                 (set-scroll-region screen yl yu))
             (move-cursor screen 0 yl)
             (do-exit-standout-mode screen)
             (let ((ts-insert-multi-line (capability-available? parm-insert-line description)))
               (if (and (fix:> n 1) ts-insert-multi-line)
                   (output-n screen (parm-insert-line n description) n-lines)
                   (do ((i 0 (fix:1+ i)))
                       ((fix:= i n))
                     (output-n screen (insert-line description) n-lines))))
             (clear-scroll-region screen)))
          ((capability-available? scroll-reverse description)
           =>
           (lambda (ts-reverse-scroll)
             (set-scroll-region screen yl yu)
             (move-cursor screen 0 yl)
             (do-exit-standout-mode screen)
             (do ((i 0 (fix:1+ i)))
                 ((fix:= i n))
               (output-n screen (scroll-reverse description) n-lines))
             (clear-scroll-region screen)
             (if (and (capability-available? memory-above description)
                      (fix:= yl 0)
                      (fix:= yu y-size))
                 (let ((x-size (screen-x-size screen)))
                   (do ((y 0 (fix:1+ y)))
                       ((fix:= y n))
                     (move-cursor screen 0 y)
                     (clear-line screen x-size))))))
          (else
           (error "can't insert lines" screen)))))

(define (insert-lines-cost screen yl yu n)
  (if (and (capability-available? insert-line (screen-description screen))
           (fix:= yu (screen-y-size screen)))
      (fix:+ (vector-ref (screen-insert-line-cost screen) yl)
             (fix:* (vector-ref (screen-insert-line-next-cost screen) yl)
                    (fix:- n 1)))
      (fix:+ (screen-scroll-region-cost screen)
             (let ((yl (fix:+ yl (fix:- (screen-y-size screen) yu))))
               (fix:+ (vector-ref (screen-insert-line-cost screen) yl)
                      (fix:* (vector-ref (screen-insert-line-next-cost screen)
                                         yl)
                             (fix:- n 1)))))))

(define (delete-lines screen yl yu n)
  (let ((y-size (screen-y-size screen))
        (description (screen-description screen))
        (n-lines (fix:- yu yl)))
    (cond ((capability-available? delete-line description)
           =>
           (lambda (ts-delete-line)
             (if (not (fix:= yu y-size))
                 (set-scroll-region screen yl yu))
             (move-cursor screen 0 yl)
             (do-exit-standout-mode screen)
             (let ((ts-delete-multi-line (capability-available? parm-delete-line description)))
               (if (and (fix:> n 1) ts-delete-multi-line)
                   (output-n screen
                             (parm-delete-line n description)
                             n-lines)
                   (do ((i 0 (fix:1+ i)))
                       ((fix:= i n))
                     (output-n screen (delete-line description) n-lines))))))
          ((scroll-forward description)
           =>
           (lambda (ts-forward-scroll)
             (set-scroll-region screen yl yu)
             (move-cursor screen 0 (fix:-1+ yu))
             (do-exit-standout-mode screen)
             (do ((i 0 (fix:1+ i)))
                 ((fix:= i n))
               (output-n screen (scroll-forward description) n-lines))))
          (else
           (error "can't delete lines" screen)))
    (if (and (memory-below description)
             (not (screen-scroll-region screen))
             (fix:> n 0))
        (begin
          (move-cursor screen 0 (fix:- y-size n))
          (clear-to-bottom screen)))
    (clear-scroll-region screen)))

(define (delete-lines-cost screen yl yu n)
  (if (and (capability-available? delete-line (screen-description screen))
           (fix:= yu (screen-y-size screen)))
      (fix:+ (vector-ref (screen-delete-line-cost screen) yl)
             (fix:* (vector-ref (screen-delete-line-next-cost screen) yl)
                    (fix:- n 1)))
      (fix:+ (screen-scroll-region-cost screen)
             (let ((yl (fix:+ yl (fix:- (screen-y-size screen) yu))))
               (fix:+ (vector-ref (screen-delete-line-cost screen) yl)
                      (fix:* (vector-ref (screen-delete-line-next-cost screen)
                                         yl)
                             (fix:- n 1)))))))

(define (set-scroll-region screen yl yu)
  (let ((y-size (terminal:x-size (screen-description screen))))
    (if (and (fix:= yl 0) (fix:= yu y-size))
        (clear-scroll-region screen)
        (if (let ((scroll-region (screen-scroll-region screen)))
              (not (and scroll-region
                        (fix:= yl (car scroll-region))
                        (fix:= yu (cdr scroll-region)))))
            (begin
              (%set-scroll-region screen yl yu)
              (set-screen-scroll-region! screen (cons yl yu)))))))

(define (clear-scroll-region screen)
  (let ((scroll-region (screen-scroll-region screen)))
    (if scroll-region
        (begin
          (%set-scroll-region screen 0 (terminal:y-size (screen-description screen)))
          (set-screen-scroll-region! screen #f)))))

(define (%set-scroll-region screen yl yu)
  (output-1 screen
            (let ((s
                   (%set-scroll-region-string (screen-description screen)
                                              (screen-x-size screen)
                                              (screen-y-size screen)
                                              yl
                                              yu)))
              (if (not s)
                  (error "can't set scroll region" screen))
              s))
  (set-screen-cursor-x! screen #f)
  (set-screen-cursor-y! screen #f))

(define (%set-scroll-region-string description x-size y-size yl yu)
  (cond ((capability-available? change-scroll-region description)
         =>
         (lambda (ts-set-scroll-region)
           (change-scroll-region yl (fix:-1+ yu))))
        ((capability-available? set-window description)
         =>
         (lambda (ts-set-window)
           (set-window yl (fix:-1+ yu) 0 (fix:-1+ x-size))))
        (else #f)))

(define (highlight-if-desired screen highlight)
  (if highlight
      (do-enter-standout-mode screen)
      (do-exit-standout-mode screen)))

(define (do-enter-standout-mode screen)
  ;; If the terminal uses standout markers, don't use standout.
  ;; It's too complicated to bother with.
  (if (and (not (screen-standout-mode? screen))
           (not (magic-cookie-glitch (screen-description screen))))
      (begin
        (set-screen-standout-mode?! screen #t)
        (maybe-output-1
         screen
         (enter-standout-mode (screen-description screen))))))

(define (do-exit-standout-mode screen)
  (if (screen-standout-mode? screen)
      (begin
        (set-screen-standout-mode?! screen #f)
        (maybe-output-1 screen
                        (exit-standout-mode (screen-description screen))))))

(define (do-enter-insert-mode screen)
  (if (not (screen-insert-mode? screen))
      (begin
        (set-screen-insert-mode?! screen #t)
        (maybe-output-1 screen
                        (enter-insert-mode (screen-description screen))))))

(define (do-exit-insert-mode screen)
  (if (screen-insert-mode? screen)
      (begin
        (set-screen-insert-mode?! screen #f)
        (maybe-output-1 screen
                        (exit-insert-mode (screen-description screen))))))

(define (do-enter-delete-mode screen)
  (if (not (screen-delete-mode? screen))
      (begin
        (set-screen-delete-mode?! screen #t)
        (maybe-output-1 screen
                        (enter-delete-mode (screen-description screen))))))

(define (do-exit-delete-mode screen)
  (if (screen-delete-mode? screen)
      (begin
        (set-screen-delete-mode?! screen #f)
        (maybe-output-1 screen
                        (exit-delete-mode (screen-description screen))))))

(define (move-cursor screen x y)
  (if (not (and (screen-cursor-x screen)
                (fix:= x (screen-cursor-x screen))
                (fix:= y (screen-cursor-y screen))))
      (%move-cursor screen x y)))

(define (%move-cursor screen x y)
  (let ((description (screen-description screen))
        (cursor-x (screen-cursor-x screen))
        (cursor-y (screen-cursor-y screen))
        (y-size (screen-y-size screen)))
    (let ((general-case
           (lambda () (output-1 screen (cursor-address y x description)))))
      (if (not (move-standout-mode description))
          (exit-standout-mode screen))
      (if (not (move-insert-mode description))
          (exit-insert-mode screen))
      (cond ((and (fix:= x 0)
                  (fix:= y 0)
                  (capability-available? cursor-home description))
             (cursor-home description))
            ((and (fix:= x 0)
                  (fix:= y (fix:-1+ y-size))
                  (capability-available? cursor-to-ll description))
             (cursor-to-ll description))
            ((not cursor-x)
             (general-case))
            ((fix:= y cursor-y)
             (cond ((and (fix:= x (fix:-1+ cursor-x))
                         (capability-available? cursor-left description))
                    (cursor-left description))
                   ((and (fix:= x (fix:1+ cursor-x))
                         (capability-available? cursor-right description))
                    (cursor-right description))
                   ((and (fix:= x 0)
                         (capability-available? carriage-return description))
                    (carriage-return description))
                   ((capability-available? column-address description)
                    (output-1 screen (column-address x description)))
                   (else
                    (general-case))))
            ((fix:= x cursor-x)
             (cond ((and (fix:= y (fix:-1+ cursor-y))
                         (capability-available? cursor-up description))
                    (cursor-up description))
                   ((and (fix:= y (fix:1+ cursor-y))
                         (capability-available? cursor-down description))
                    (cursor-down description))
                   (else
                    (general-case))))
            (else
             (general-case)))))
  (set-screen-cursor-x! screen x)
  (set-screen-cursor-y! screen y))

(define (record-cursor-after-output screen cursor-x)
  (let ((description (screen-description screen)))
    (let ((x-size (screen-x-size screen)))
      (cond ((fix:< cursor-x x-size)
             (set-screen-cursor-x! screen cursor-x))
            ((fix:> cursor-x x-size)
             (error "wrote past end of line" cursor-x x-size))
            ((eat-newline-glitch description)
             (set-screen-cursor-x! screen #f)
             (set-screen-cursor-y! screen #f))
            ((auto-right-margin description)
             (set-screen-cursor-x! screen 0)
             (set-screen-cursor-y! screen (fix:1+ (screen-cursor-y screen))))
            (else
             (set-screen-cursor-x! screen (fix:-1+ x-size)))))))

(define (output screen command)
  (output-n screen
            command
            (fix:- (let ((scroll-region (screen-scroll-region screen)))
                     (if scroll-region
                         (cdr scroll-region)
                         (terminal:x-size (screen-description screen))))
                   (screen-cursor-y screen))))

(define (output-1 screen command)
  (output-n screen command 1))

(define (output-n screen command n-lines)
  (let ((description (screen-description screen)))
    (tputs command n-lines (terminal:port description))))

(define (maybe-output screen command)
  (if command
      (output screen command)))

(define (maybe-output-1 screen command)
  (maybe-output-n screen command 1))

(define (maybe-output-n screen command n-lines)
  (if command
      (output-n screen command n-lines)))

(define (compute-scrolling-costs description baud-rate x-size y-size)
  (call-with-values
      (lambda ()
        (i/d-line-cost-vectors description
                               baud-rate
                               y-size
                               (capability-available? parm-insert-line description)
                               (or (capability-available? insert-line description)
                                   (capability-available? scroll-reverse description))))
    (lambda (insert-line-cost insert-line-next-cost)
      (call-with-values
          (lambda ()
            (i/d-line-cost-vectors description
                                   baud-rate
                                   y-size
                                   (capability-available? parm-delete-line description)
                                   (or (capability-available? delete-line description)
                                       (capability-available? scroll-forward description))))
        (lambda (delete-line-cost delete-line-next-cost)
          (values insert-line-cost
                  insert-line-next-cost
                  delete-line-cost
                  delete-line-next-cost
                  (let ((string
                         (%set-scroll-region-string description
                                                    x-size
                                                    y-size
                                                    0
                                                    y-size)))
                    (if string
                        (fix:* 2 (string-cost description baud-rate string 0))
                        0))))))))

(define (i/d-line-cost-vectors description baud-rate y-size
                               multi-line one-line)
  (let ((extra
         ;; Discourage long scrolls slightly on fast lines.  This
         ;; says that scrolling nearly the full length of the screen
         ;; is not worth it if reprinting takes less than 1/4
         ;; second.
         (fix:quotient baud-rate (fix:* 40 y-size)))
        (string-cost
         (lambda (string n)
           (string-cost description baud-rate string n))))
    (cond (multi-line
           (let ((c (string-cost multi-line 0)))
             (scrolling-vectors y-size
                                c
                                (fix:- (string-cost multi-line 10) c)
                                extra
                                0)))
          (one-line
           (let ((c (string-cost one-line 0)))
             (scrolling-vectors y-size
                                0
                                0
                                (fix:+ c extra)
                                (fix:- (string-cost one-line 10) c))))
          (else
           (values #f #f)))))

(define (string-cost description baud-rate string n-lines)
  (let* ((string-port   (open-output-string))
         (padded-string (tputs string n-lines string-port))
         (output-string (get-output-string string-port)))
    (string-length output-string)))

#| Calculate the insert and delete line costs.

We keep the ID costs in a precomputed array based on the position at
which the I or D is performed.  Also, there are two kinds of ID costs:
the "once-only" and the "repeated".  This is to handle both those
terminals that are able to insert N lines at a time (once-only) and
those that must repeatedly insert one line.

The cost to insert N lines at line L (0-origin indexing) is

        (+ (+ IL-OV1 (* IL-PF1 (- Y-SIZE L)))
           (* N (+ IL-OVN (* IL-PFN (- Y-SIZE L)))))

IL-OV1 represents the basic insert line overhead.  IL-PF1 is the
padding required to allow the terminal time to move a line: insertion
at line L changes (- Y-SIZE L) lines.

The first subexpression above is the overhead; the second is the
multiply factor.  Both are dependent only on the position at which the
insert is performed.  We store the overhead in INSERT-LINE-COST and
the multiply factor in INSERT-LINE-NEXT-COST.  Note however that any
insertion must include at least one multiply factor.  Rather than
compute this as INSERT-LINE-COST[line]+INSERT-LINE-NEXT-COST[line], we
add INSERT-LINE-NEXT-COST into INSERT-LINE-COST.  This is reasonable
because of the particular algorithm used.

Deletion is essentially the same as insertion. 

Note that the multiply factors are in tenths of characters.  |#

(define (scrolling-vectors y-size overhead-1 factor-1 overhead-n factor-n)
  (let ((overhead (make-vector y-size))
        (factor (make-vector y-size)))
    (let loop
        ((y 0)
         (o (fix:+ (fix:* overhead-1 10) (fix:* factor-1 y-size)))
         (n (fix:+ (fix:* overhead-n 10) (fix:* factor-n y-size))))
      (if (fix:< y y-size)
          (begin
            (vector-set! factor y (fix:quotient n 10))
            (let ((n (fix:- n factor-n)))
              (vector-set! overhead y (fix:quotient (fix:+ o n) 10))
              (loop (fix:1+ y) (fix:- o factor-1) n)))))
    (values overhead factor)))

(define (resize-screen)
  (let* ((screen (selected-screen))
         (state (screen-state screen)))
    (if (not (terminal-state? state))
        (editor-error "Not a terminal screen")
        (let ((port console-output-port)
              (desc (terminal-state-description state)))
          (let ((x-size (terminal:x-size desc))
                (y-size (terminal:y-size desc)))
            (if (or (not (= x-size (screen-x-size screen)))
                    (not (= y-size (screen-y-size screen))))
                (begin
                  (without-interrupts
                   (lambda ()
                     (set-terminal-x-size! desc x-size)
                     (set-terminal-y-size! desc y-size)
                     (set-screen-size! screen x-size y-size)))
                  (update-screen! screen #t))))))))