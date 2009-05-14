#| -*-Scheme-*-

$Id: paredit.scm,v 1.10 2007/02/23 11:38:32 riastradh Exp $

This code is written by Taylor R. Campbell and placed in the Public
Domain.  All warranties are disclaimed.

|#

;;;; Paredit: Parenthesis-Editing Minor Mode (based on paredit.el)


(define-command paredit-mode
  "Toggle pseudo-structural editing of Lisp code.
With a prefix argument, enable paredit mode if the argument is
  positive, and disable paredit mode if not."
  "P"
  (lambda (argument)
    (let ((mode (ref-mode-object paredit)))
      (if (if argument
              (positive? (command-argument-value argument))
              (not (current-minor-mode? mode)))
          (enable-current-minor-mode! mode)
          (disable-current-minor-mode! mode)))))

(define-minor-mode paredit "Paredit"
  "Minor mode for pseudo-structurally editing Lisp code.

\\{paredit}")

(for-each (lambda (key)
            (define-key 'paredit (car key) (cadr key)))
          '(
            ;; Insertion commands
            ((kbd #\()      paredit-open-list)
            ((kbd #\))      paredit-close-list-and-newline)
            ((kbd (meta #\)))   paredit-close-list)
            ((kbd (meta #\"))   paredit-close-string-and-newline)
            ((kbd #\")      paredit-doublequote)
            ((kbd #\\)      paredit-backslash)
            ((kbd return) paredit-newline)  ; This defies the convention,
            ((kbd (ctrl #\j))    newline)          ; but I prefer it, and you can
                                        ; customize it yourself anyway.
            ;; Killing & deleting
            ((kbd (ctrl #\d))    paredit-forward-delete)
            ((kbd rubout) paredit-backward-delete)
            ((kbd (ctrl #\k))    paredit-kill)

            ;; Movement & navigation
            ((kbd ctrl meta #\f) paredit-forward)
            ((kbd ctrl meta #\b) paredit-backward)
;;;         ((kbd ctrl meta #\u) backward-up-list) ; These two are built-in.
;;;         ((kbd ctrl meta #\d) down-list)
            ((kbd ctrl meta #\p) backward-down-list)
            ((kbd ctrl meta #\n) up-list)
            ((kbd (ctrl #\c) (kbd ctrl meta #\l)) paredit-recentre-on-sexp)

            ;; Depth-changing commands
            ((kbd (meta #\()) paredit-wrap-sexp)
            ((kbd (meta #\r))  paredit-raise-sexp)
            ((kbd (meta #\s))  paredit-splice-sexp)   ;++ This conflicts with M-s
                                           ;++ for STEP-DEFUN.  Hmmmm.

            ;; Splitting and Joining
            ((kbd (meta #\S)) paredit-split-sexp)
            ((kbd (meta #\J)) paredit-join-sexps)
            ))

;;;; Basic Editing Commands

(define-command paredit-open-list
  "Insert a balanced round bracket parenthesis pair.
With a prefix argument N, put the closing round bracket after N
  S-expressions forward.
If in string or comment, inserts a single opening round bracket.
If in a character literal, does nothing.  This prevents accidentally
  changing what was in the character literal to a meaningful delimiter
  unintentionally."
  "P"
  (let ((open-list
         (lambda (argument)
           (insert-sexp-pair #\( #\)
                             (or (command-argument-value argument)
                                 0)))))
    (lambda (argument)
      (if (group-start? (current-point))
          (open-list #f)
          (let ((state (current-parse-state)))
            (cond ((or (parse-state-in-string? state)
                       (parse-state-in-comment? state))
                   (insert-char #\( ))
                  ((not (mark-right-char-quoted? (current-point)))
                   (open-list argument))))))))

(define-command paredit-close-list
  "Move past the closing delimiter of the list the point is on.
Delete all extraneous space before the closing delimiter, but do not
  move it past comments between it and the point.
If in a string or comment, insert a single closing round bracket.
If in a character literal, do nothing.  This prevents accidentally
  changing what was in the character literal to a meaningful delimiter
  unintentionally."
  ()
  (lambda ()
    (let ((point (current-point)))
      (if (group-start? point)
          (editor-failure "No list to close at buffer start.")
          (let ((state (current-parse-state)))
            (cond ((or (parse-state-in-string? state)
                       (parse-state-in-comment? state))
                   (insert-char #\) ))
                  ((not (mark-right-char-quoted? point))
                   (paredit-move-past-close-and-reindent point state)
                   (flash-sexp-match))))))))

(define-command paredit-close-list-and-newline
  "Move past close of the current list, insert a newline, & indent.
If in a string or comment, insert a single closing round bracket.
If in a character literal, do nothing.  This prevents accidentally
  changing what was in the character literal to a meaningful delimiter
  unintentionally."
  ()
  (lambda ()
    (let ((point (current-point)))
      (if (group-start? point)
          (editor-failure "No list to close at buffer start.")
          (let ((state (current-parse-state)))
            (cond ((or (parse-state-in-string? state)
                       (parse-state-in-comment? state))
                   (insert-char #\) ))
                  (else
                   (paredit-move-past-close-and-reindent
                    (if (mark-right-char-quoted? point)
                        (mark1+ point)
                        point)
                    state)
                   (insert-newline-preserving-comment)
                   (lisp-indent-line-and-sexp)
                   (flash-sexp-match #t))))))))

(define (paredit-move-past-close-and-reindent mark state)
  (cond ((forward-up-one-list mark)
         => (lambda (after-close)
              (undo-record-point!)
              (set-current-point! after-close)
              (let loop ((before-close (mark-1+ after-close)))
                (if (mark= (horizontal-space-end
                            (line-start before-close 0))
                           before-close)
                    ;; The closing delimiter is the first thing on the
                    ;; line.  If the previous line ends in a comment,
                    ;; we stop here; otherwise, we go on.
                    (let ((end-of-prev (line-end before-close -1))
                          (location (parse-state-location state)))
                      (cond ((and (not (mark<= end-of-prev location))
                                  (parse-state-in-comment?
                                   (parse-partial-sexp location
                                                       end-of-prev
                                                       #f #f
                                                       state)))
                             ;; Nothing more to be done, so just
                             ;; indent the line we're on (which has
                             ;; the closing delimiter).
                             (lisp-indent-line #f))
                            (else
                             ;; More to delete.
                             (delete-string end-of-prev before-close)
                             (loop end-of-prev))))
                    ;; We've reached our goal, though there might be
                    ;; some indentation between the closing delimiter
                    ;; and where we want it to be.  We must take care,
                    ;; though, to preserve whitespace characters.
                    (let* ((mark
                            (horizontal-space-start before-close))
                           (escaped
                            (and (mark-right-char-quoted? mark)
                                 (mark-right-char mark))))
                      (delete-horizontal-space before-close)
                      (if escaped
                          (insert-char escaped mark)))))))
        (else
         (editor-error "No closing delimiter to move over."))))

(define-command paredit-close-string-and-newline
  "Move to the end of the string, insert a newline, and indent.
If not in a string, act as `paredit-doublequote'."
  ()
  (lambda ()
    (let ((state (current-parse-state)))
      (if (not (parse-state-in-string? state))
          ((ref-command paredit-doublequote))
          (let ((after-string (parse-state-end-of-sexp state)))
            (set-current-point! after-string)
            (insert-newline)
            (lisp-indent-line-and-sexp)
            (flash-sexp-match #f after-string))))))

(define-command paredit-doublequote
  "Insert a pair of double-quotes.
Inside a comment, insert a literal double-quote.
At the end of a string, move past the closing double-quote.
In the middle of a string, insert a backslash-escaped double-quote.
If in a character literal, do nothing.  This prevents accidentally
  changing what was in the character literal to a meaningful delimiter
  unintentionally."
  ()
  (lambda ()
    (let ((state (current-parse-state)))
      (cond ((parse-state-in-string? state)
             (if (mark= (mark-1+ (parse-state-end-of-sexp state))
                        (current-point))
                 ;; On the closing quote -- move past it & flash.
                 (begin (set-current-point! (mark1+ (current-point)))
                        (flash-sexp-match))
                 ;; Elsewhere in a string: insert escaped.
                 (begin (insert-char #\\ )
                        (insert-char #\"))))
            ((parse-state-in-comment? state)
             (insert-char #\" ))
            ((not (mark-right-char-quoted? (current-point)))
             (insert-sexp-pair #\" #\" 0))))))

(define-command paredit-backslash
  "Insert a backslash followed by a character to escape."
  ()
  (lambda ()
    (let ((state (current-parse-state)))
      (insert-char #\\ )
      (if (not (parse-state-in-comment? state))
          (let ((char #f))
            (dynamic-wind               ;++ What happens if this gets
              (lambda () unspecific)    ;++ used in a recursive edit?
              (lambda ()
                (set! char (prompt-for-char "Character to escape")))
              (lambda ()
                (if (and char (not (char=? char #\rubout)))
                    (insert-char char)
                    (delete-left-char)))))))))

(define-command paredit-newline
  "Insert a newline and indent.
This is like `newline-and-indent', but it not only indents the line
  that the point is on but also the S-expression following the point,
  if there is one.
Move forward one character first if on an escaped character.
If in a string, just insert a literal newline."
  ()
  (lambda ()
    (let ((state (current-parse-state)))
      (cond ((parse-state-in-string? state)
             (insert-newline))
            (else
             (let ((point (current-point)))
               (if (and (not (parse-state-in-string? state))
                        (mark-right-char-quoted? point))
                   (set-current-point! (mark1+ point))))
             (delete-horizontal-space)
             (insert-newline)
             (lisp-indent-line-and-sexp))))))

(define-command paredit-forward-delete
  "Delete a character forward or move forward over a delimiter.
If on an opening S-expression delimiter, move forward into the
  S-expression.
If on a closing S-expression delimiter, refuse to delete unless the
  S-expression is empty, in which case delete the whole S-expression.
With a prefix argument, simply delete a character forward, without
  regard for delimiter balancing.  This is useful when the buffer has
  entered a structurally inconsistent state which paredit is unable to
  cope with."
  "P"
  (lambda (argument)
    (let ((point (current-point)))
      (if (or (command-argument-value argument)
              (group-end? point))
          ((ref-command delete-char) #f)
          (let ((state (current-parse-state))
                (right (mark-right-char point)))
            (cond ((parse-state-in-string? state)
                   (paredit-forward-delete-in-string point state))
                  ((parse-state-in-comment? state)
                   (delete-right-char point))
                  ((mark-right-char-quoted? point)
                   ;; Escape -- delete both characters.
                   (delete-string (mark-1+ point)
                                  (mark1+ point)))
                  ((char=? right #\\ )
                   ;; Ditto.
                   (delete-string (mark+ point 2) point))
                  ((let ((syn (char-syntax right)))
                     (or (char=? syn #\( )
                         (char=? syn #\" )))
                   ;; Enter into an S-expression forward.
                   (set-current-point! (mark1+ point)))
                  ((and (not (group-start? point))
			(not (mark-right-char-quoted?
                              (mark-1+ point)))
                        (char=? (char-syntax right)
                                #\) )
                        (char=? (mark-left-char point)
                                (char-matching-paren right)))
                   ;; Empty list -- delete both delimiters.
                   (delete-string (mark-1+ point)
                                  (mark1+ point)))
                  ;; Just delete a single character, if it's not a
                  ;; closing parenthesis.
                  ((not (char=? (char-syntax right) #\) ))
                   (delete-right-char point))))))))

(define (paredit-forward-delete-in-string point state)
  (let ((before (mark-1+ point))
        (after (mark1+ point)))
    (cond ((not (mark= after (parse-state-end-of-sexp state)))
           ;; If it's not the close-quote, it's safe to delete.  But
           ;; first handle the case that we're in a string escape.
           (cond ((mark-within-string-escape? point)
                  ;; We're right after the backslash, so delete one
                  ;; character backward (the backslash) and one
                  ;; character forward (the escaped character).
                  (delete-string before after))
                 ((mark-within-string-escape? after)
                  ;; A string escape starts here, so delete both
                  ;; characters forward.
                  (delete-string point (mark1+ after)))
                 (else
                  ;; Otherwise, just delete a single character.
                  (delete-right-char point))))
          ((mark= before (parse-state-start-of-sexp state))
           ;; If it is the close-quote, delete only if we're also
           ;; right past the open-quote (i.e. it's empty), and then
           ;; delete both quotes.  Otherwise refuse to delete it.
           (delete-string before after)))))

(define-command paredit-backward-delete
  "Delete a character backward or move backward over a delimiter.
If on a closing S-expression delimiter, move backward into the
  S-expression.
If on an opening S-expression delimiter, refuse to delete unless the
  S-expression is empty, in which case delete the whole S-expression.
With a prefix argument, simply delete a character backward, without
  regard for delimiter balancing, and possibly untabify.  This is
  useful when the buffer has entered a structurally inconsistent state
  which paredit is unable to cope with."
  "P"
  (lambda (argument)
    (let ((point (current-point)))
      (if (or (command-argument-value argument)
              (group-start? point))
          ((ref-command backward-delete-char-untabify) #f)
          (let ((state (current-parse-state))
                (left (mark-left-char point)))
            (cond ((parse-state-in-string? state)
                   (paredit-backward-delete-in-string point state))
                  ((parse-state-in-comment? state)
                   ((ref-command backward-delete-char-untabify) #f))
                  ((mark-right-char-quoted? point)
                   ;; Escape -- delete both characters.
                   (delete-string (mark-1+ point)
                                  (mark1+ point)))
                  ((mark-left-char-quoted? point)
                   ;; Ditto.
                   (delete-string (mark- point 2) point))
                  ((let ((syn (char-syntax left)))
                     (or (char=? syn #\) )
                         (char=? syn #\" )))
                   ;; Enter into an S-expression backward.
                   (set-current-point! (mark-1+ point)))
                  ((and (char=? (char-syntax left) #\( )
                        (char=? (mark-right-char point)
                                (char-matching-paren left)))
                   ;; Empty list -- delete both delimiters.
                   (delete-string (mark-1+ point)
                                  (mark1+ point)))
                  ;; Delete it only on the condition that it's not an
                  ;; opening parenthesis.
                  ((not (char=? (char-syntax left) #\( ))
                   ((ref-command backward-delete-char-untabify) #f))))))))

(define (paredit-backward-delete-in-string point state)
  (let ((before (mark-1+ point))
        (after (mark1+ point)))
    (cond ((not (mark= before (parse-state-start-of-sexp state)))
           ;; If it's not the open-quote, it's safe to delete, but we
           ;; still must be careful with escapes.
           (cond ((mark-within-string-escape? point)
                  (delete-string before after))
                 ((mark-within-string-escape? before)
                  (delete-string (mark-1+ before) point))
                 (else
                  (delete-left-char point))))
          ((mark= after (parse-state-end-of-sexp state))
           ;; If it is the open-quote, delete only if we're also right
           ;; past the close-quote (i.e. it's empty), and then delete
           ;; both quotes.  Otherwise we refuse to delete it.
           (delete-string before after)))))

(define-command paredit-kill
  "Kill a line as if with `kill-line', but respect delimiters.
In a string, act exactly as `kill-line' but do not kill past the
  closing string delimiter.
On a line with no S-expressions on it starting after the point or
  within a comment, act exactly as `kill-line'.
Otherwise, kill all S-expressions that start on the line after the
  point."
  "P"
  (lambda (argument)
    (if (command-argument-value argument)
        ((ref-command kill-line) #f)
        (let ((state (current-parse-state))
              (point (current-point)))
          (cond ((parse-state-in-string? state)
                 (paredit-kill-line-in-string point))
                ((or (parse-state-in-comment? state)
                     (let* ((eol (line-end point 0))
                            (next
                             (skip-whitespace-forward point eol)))
                       (or (mark= next eol)
                           (char=? (mark-right-char next)
                                   #\; ))))
                 ((ref-command kill-line) #f))
                (else
                 (paredit-kill-sexps-on-line point)))))))

(define (paredit-kill-line-in-string point)
  (let ((eol (line-end point 0)))
    (cond ((mark= (skip-whitespace-forward point eol)
                  eol)
           ((ref-command kill-line) #f))
          (else
           (let ((beginning (if (mark-within-string-escape? point)
                                (mark-1+ point)
                                point)))
             (let loop ((mark beginning))
               (if (or (mark= mark eol)
                       (char=? (mark-right-char mark)
                               #\" ))
                   (kill-string beginning mark)
                   (loop (mark+ mark
                                (if (char=? (mark-left-char mark)
                                            #\\ )
                                    2
                                    1))))))))))

(define (paredit-kill-sexps-on-line point)
  (let* ((beginning (if (mark-right-char-quoted? point)
                        (mark1+ point)  ; Don't break a line in a
                        point))         ; character literal.
         (eol (line-end beginning 0))
         (kill-to (lambda (end)
                    (kill-string beginning end))))
    (let loop ((mark beginning))
      (cond ((or (group-end? mark)
                 (not (mark= (line-end mark 0) eol)))
             (kill-to mark))
            ((forward-one-sexp mark)
             => (lambda (sexp-end-mark)
                  (cond ((backward-one-sexp sexp-end-mark)
                         => (lambda (sexp-start-mark)
                              ;; Only if it starts on the same line
                              ;; will we include it in what we kill.
                              (if (mark= (line-end sexp-start-mark 0)
                                         eol)
                                  (loop sexp-end-mark)
                                  (kill-to mark))))
                        (else (kill-to mark)))))
            ((forward-up-one-list mark)
             => (lambda (after-close)
                  (kill-to (if (mark= (line-end after-close 0)
                                      eol)
                               (mark-1+ after-close)
                               eol))))
            (else
             (kill-to mark))))))

;;;; Cursor and Screen Movement Commands on S-expressions

(define (paredit-movement-command move-sexp move-char move-up)
  (lambda ()
    (set-current-point!
     (let ((point (current-point)))
       (cond ((move-sexp point))
             ((parse-state-in-string? (current-parse-state))
              (move-char point))
             ((move-up point))
             (else
              (editor-error "Unable to move.")))))))

(define-command paredit-forward
  "Move forward an S-expression, or up an S-expression forward.
If there are no more S-expressions in this one before the closing
  delimiter, move past that closing delimiter; otherwise, move forward
  over the S-expression following the point."
  ()
  (paredit-movement-command forward-one-sexp
                            mark1+
                            forward-up-one-list))

(define-command paredit-backward
  "Move backward an S-expression, or up an S-expression backward.
If there are no more S-expressions in this one after the opening
  delimiter, move past that opening delimiter; otherwise, move
  backward over the S-expression preceding the point."
  ()
  (paredit-movement-command backward-one-sexp
                            mark-1+
                            backward-up-one-list))

(define-command paredit-recentre-on-sexp
  "Recentre the screen on the S-expression following the point.
With a prefix argument N, encompass all N S-expressions forward."
  "p"
  (lambda (n)
    (let* ((end-mark (forward-sexp (current-point) n 'ERROR))
           (start-mark (backward-sexp end-mark n 'ERROR))
           (centre-offset (quotient (count-lines start-mark end-mark)
                                    2)))
      (set-current-point! (line-start start-mark centre-offset))
      ((ref-command recenter) #f))))

;;;; Wrappage, splicage, & raisage

(define-command paredit-wrap-sexp
  "Wrap the following S-expression in a list.
If a prefix argument N is given, wrap N S-expressions.
Automatically indent the newly wrapped S-expression.
As a special case, if the point is at the end of a list, simply insert
  a pair of parentheses."
  "p"
  (lambda (n)
    (insert-sexp-pair #\( #\)
                      (if (forward-sexp (current-point) n #f)
                          n
                          0))
    (lisp-indent-sexp
     (or (backward-up-one-list (current-point))
         (error "Wrappage bogosity.  Please inform TRC.")))))

(define-command paredit-raise-sexp
  "Raise the following S-expression in a tree, deleting its siblings.
With a prefix argument N, raise the following N S-expressions.  If N
  is negative, raise the preceding N S-expressions."
  "p"
  (lambda (n)
    ;; I have very carefully selected where to use {FOR,BACK}WARD-SEXP
    ;; with arguments 1 & ERROR and {FOR,BACKWARD}-ONE-SEXP here, so
    ;; that the error is signalled initially and then not checked
    ;; redundantly later.
    ;++ This should be verified.
    (let* ((point (current-point))
           (mark (forward-sexp (current-point) n 'ERROR))
           (sexps (if (negative? n)
                      (extract-string mark
                                      (forward-one-sexp
                                       (backward-one-sexp point)))
                      (extract-string (backward-one-sexp
                                       (forward-one-sexp point))
                                      mark)))
           (before-encloser (mark-temporary-copy
                             (backward-up-list point 1 'ERROR))))
      (delete-string before-encloser
                     (forward-sexp before-encloser 1 'ERROR))
      (insert-string sexps before-encloser)
      (let loop ((n n) (mark before-encloser))
        (if (positive? n)
            (let ((after (forward-one-sexp mark)))
              (set-current-point! (backward-one-sexp after))
              (lisp-indent-line #f)
              (lisp-indent-sexp (current-point))
              (loop (- n 1) after))))
      (set-current-point! before-encloser))))

(define-command paredit-splice-sexp
  "Splice the list that the point is on by removing its delimiters.
With a prefix argument as in `C-u', kill all S-expressions backward in
  the current list before splicing all S-expressions forward into the
  enclosing list.
With two prefix arguments as in `C-u C-u', kill all S-expressions
  forward in the current list before splicing all S-expressions
  backward into the enclosing list.
With a numerical prefix argument N, kill N S-expressions backward in
  the current list before splicing the remaining S-expressions into the
  enclosing list.  If N is negative, kill forward."
  "P"
  (lambda (argument)
    (undo-record-point!)
    (if argument (paredit-kill-surrounding-sexps-for-splice argument))
    (let* ((before-open (backward-up-list (current-point) 1 'ERROR))
           (before-close
            (mark-1+ (forward-sexp before-open 1 'ERROR)))) 
      (delete-right-char before-close)
      (delete-right-char before-open)
      (with-current-point before-open
        (lambda ()
          (paredit-reindent-splicage argument))))))

(define (paredit-kill-surrounding-sexps-for-splice argument)
  (cond ((command-argument-multiplier-only? argument)
         (let ((loop (lambda (mark-end? advance-one-sexp)
                       (let ((point-a (current-point)))
                         (let loop ((point-b point-a))
                           (define (win) (kill-string point-a point-b))
                           (cond ((mark-end? point-b) (win))
                                 ((advance-one-sexp point-b) => loop)
                                 (else (win)))))))
               (value (command-argument-numeric-value argument)))
           (if (= value 4)              ;One C-u
               (loop group-start? backward-one-sexp)
               (loop group-end? forward-one-sexp))))
        ((exact-integer? argument)
         (let* ((point (current-point))
                (mark (backward-sexp point argument 'ERROR)))
           (kill-string point mark)))
        (else
         (error "Bizarre prefix argument to PAREDIT-SPLICE:"
                argument))))

(define (paredit-reindent-splicage argument)
  (cond ((backward-up-list (current-point) 1 #f)
         => lisp-indent-sexp)
        ((not (exact-integer? argument))
         unspecific)
        ((positive? argument)
         (lisp-indent-line #f)
         (lisp-indent-sexp (current-point))
         (if (> argument 1)
             (save-excursion
              (lambda ()
                (let loop ((n argument))
                  (lisp-indent-line #f)
                  (modify-current-point!
                   (lambda (point)
                     (lisp-indent-sexp point)
                     (forward-one-sexp point)))
                  (let ((m (- n 1)))
                    (if (positive? m)
                        (loop m))))))))
        ((negative? argument)
         (save-excursion
          (lambda ()
            (let loop ((n argument))
              (cond ((not (zero? n))
                     (modify-current-point! backward-one-sexp)
                     (lisp-indent-line #f)
                     (lisp-indent-sexp (current-point))
                     (loop (+ n 1))))))))))

;;;; Splitting and Joining

(define-command paredit-split-sexp
  "Split the list or string the point is on in two."
  ()
  (lambda ()
    (let ((state (current-parse-state)))
      (cond ((parse-state-in-string? state)
             (insert-char #\")
             (save-excursion
              (lambda ()
                (insert-char #\space)
                (insert-char #\"))))
            ((or (parse-state-in-comment? state)
                 (mark-right-char-quoted? (current-point)))
             (editor-error
              "Invalid context for S-expression splitting."))
            ((let ((point (current-point)))
               (and (memv (char-syntax (mark-left-char point))
                          '(#\w #\_))
                    (memv (char-syntax (mark-right-char point))
                          '(#\w #\_))))
             (save-excursion (lambda ()
                               (insert-char #\space))))
            (else
             (undo-record-point!)
             (split-sexp-at-point))))))

(define (split-sexp-at-point)
  (let ((open (backward-up-list (current-point) 1 'ERROR))
        (close (forward-up-list (current-point) 1 'ERROR)))
    (let ((open-char (mark-right-char open))
          (close-char (mark-left-char close)))
      (let ((new-close (cond ((backward-one-sexp (current-point))
                              => forward-one-sexp)
                             (else (mark1+ open))))
            (new-open (cond ((forward-one-sexp (current-point))
                             => backward-one-sexp)
                            (else (mark-1+ close)))))
        (if (mark< new-open new-close)  ;Can't actually happen...
            (editor-error               ;I guess Democritus was right!
             "Splitting atom!  RUN, before critical mass!!"))
        (let ((new-close (mark-left-inserting-copy new-close))
              (new-open (mark-left-inserting-copy new-open)))
          (insert-char close-char new-close)
          (mark-temporary! new-close)
          (save-excursion
           (lambda ()
             (if (not (char=? (char-syntax (mark-left-char new-open))
                              #\space))
                 (insert-char #\space new-open))
             (mark-temporary! new-open)
             (insert-char open-char new-open)
             (if (mark/= (line-start (current-point) 0)
                         (line-start new-open 0))
                 (with-current-point new-open
                   lisp-indent-line-and-sexp)
                 (lisp-indent-sexp new-open)))))))))

(define-command paredit-join-sexps
  "Join the S-expressions adjacent on either side of the point.
Both must be lists, strings, or atoms; error if there is mismatch."
  ()
  (lambda ()
    (let ((state (current-parse-state)))
      (if (or (parse-state-in-comment? state)
              (parse-state-in-string? state) ;foo
              (mark-right-char-quoted? (current-point)))
          (editor-error "Invalid context for S-expression joining.")
          (let ((left-point (end-of-sexp-backward (current-point)))
                (right-point (start-of-sexp-forward (current-point))))
            (cond ((mark< right-point left-point)
                   (editor-error "Joining single S-expression."))
                  ((intervening-text? left-point right-point)
                   (editor-error
                    "S-expressions to join have intervenining text."))
                  (else
                   (save-excursion
                    (lambda ()
                      (join-sexps left-point right-point))))))))))

(define (join-sexps left-point right-point)
  (let ((left-syntax (char-syntax (mark-left-char left-point)))
        (right-syntax (char-syntax (mark-right-char right-point))))
    (cond ((and (char=? left-syntax #\))
                (char=? right-syntax #\())
           (let ((right-point
                  (if (mark/= left-point right-point)
                      right-point
                      (begin (insert-char #\space right-point)
                             (mark1+ right-point)))))
             (delete-right-char right-point)
             (delete-left-char left-point))
           (lisp-indent-sexp
            (backward-up-list (current-point) 1 'ERROR)))
          ((and (char=? left-syntax #\")
                (char=? right-syntax #\"))
           (delete-string (mark-1+ left-point)
                          (mark1+ right-point)))
          ((or (and (memq left-syntax  '(#\w #\_))
                    (memq right-syntax '(#\w #\_))))
           ;; Word or symbol
           (delete-string left-point right-point))
          (else
           (editor-error
            "Mismatched S-expressions to join.")))))

;;;; Miscellaneous Utilities

(define (current-parse-state #!optional point)
  (let ((point (if (default-object? point)
                   (current-point)
                   point)))
    (parse-partial-sexp (or (this-definition-start point)
                            (buffer-start (current-buffer)))
                        point)))

(define (insert-sexp-pair open close sexps #!optional mark)

  (define (insert-space end? mark)
    (if (and (not (if end?
                      (group-end? mark)
                      (group-start? mark)))
             (memv (char-syntax (if end?
                                    (mark-right-char mark)
                                    (mark-left-char mark)))
                   (cons (if end? #\( #\) )
                         '(#\\          ; escape
                           #\w          ; word constituent
                           #\_          ; symbol constituent
                           #\"))))      ; string quote
        (begin (insert-char #\space mark)
               (mark1+ mark))
        mark))

  (let* ((start (mark-temporary-copy (if (default-object? mark)
                                         (current-point)
                                         mark)))
         (before (insert-space #f start)))
    (insert-char open before)
    (let ((point (mark1+ before)))
      (let ((after (forward-sexp point sexps 'ERROR)))
        (insert-char close after)
        (insert-space #t (mark1+ after)))
      (set-current-point! point))))

(define (insert-newline-preserving-comment #!optional mark)
  (let ((mark (if (default-object? mark) (current-point) mark)))
    (cond ((line-margin-comment-region mark)
           => (lambda (region)
                (mark-permanent! mark)
                (let* ((before-semi (region-start region))
                       (bol (line-start before-semi 0))
                       (column (region-count-chars
                                (make-region bol before-semi)))
                       (comment (extract-and-delete-string
                                 before-semi
                                 (region-end region))))
                  (delete-horizontal-space before-semi)
                  (let ((copy (mark-temporary-copy mark)))
                    (insert-newline mark)
                    (indent-to column 0 copy)
                    (insert-string comment (line-end copy 0))))))
          (else
           (insert-newline mark)))))

;;; This assumes that POINT is before the comment on the line, if there
;;; is a comment.  This assumption may be flawed for general use, but
;;; it is guaranteed by paredit's use of this procedure.

(define (line-margin-comment-region #!optional point)
  (let* ((point (if (default-object? point)
                    (current-point)
                    point))
         (eol (line-end point 0)))
    (let loop ((point point)
               (state (current-parse-state point)))
      (cond ((char-search-forward #\; point eol)
             => (lambda (after-semi)
                  (let ((state* (parse-partial-sexp point after-semi
                                                    #f #f
                                                    state)))
                    (if (or (mark-left-char-quoted? after-semi)
                            (parse-state-in-string? state*))
                        (loop after-semi state*)
                        (make-region (mark-1+ after-semi)
                                     eol)))))
            (else #f)))))

(define (start-of-sexp-forward mark)
  (backward-sexp (forward-sexp mark 1 'ERROR) 1))

(define (end-of-sexp-backward mark)
  (forward-sexp (backward-sexp mark 1 'ERROR) 1))

(define (intervening-text? start end)
  (mark/= (skip-whitespace-forward start end)
          end))

(define (lisp-indent-line-and-sexp)
  (lisp-indent-line #f)
  (let ((point (current-point)))
    (if (cond ((forward-one-sexp point)
               => (lambda (end)
                    (mark= (line-start (backward-one-sexp end) 0)
                           (line-start point 0))))
              (else #f))
        (lisp-indent-sexp point))))

;;; In paredit.el, the ABSOLUTELY? argument determined whether or not
;;; to override the BLINK-MATCHING-PAREN variable, because in some
;;; contexts SHOW-PAREN-MODE suffices for the purpose; however, Edwin
;;; has no such variable or SHOW-PAREN-MODE, but I'd like to make it
;;; easy to support them later on.

(define (flash-sexp-match #!optional absolutely? point)
  absolutely?
  (mark-flash (backward-one-sexp (if (default-object? point)
                                     (current-point)
                                     point))
              'RIGHT))

(define (char-matching-paren char)
  ;++ This is a hideous kludge.  Why is it necessary?  There must be
  ;++ something built-in that does this.
  (string-ref (char-syntax->string
               (get-char-syntax (ref-variable syntax-table)
                                char))
              1))

;;; This assumes that MARK is already in a string.

(define (mark-within-string-escape? mark)
  (let loop ((flag #f) (mark mark))
    (if (char=? (mark-left-char mark)
                #\\)
        (loop (not flag) (mark-1+ mark))
        flag)))

(define (skip-whitespace-forward #!optional start end)
  (skip-chars-forward (char-set->string char-set:whitespace)
                      start
                      end))

(define (char-set->string char-set)
  (list->string (char-set-members char-set)))

(define (undo-record-point! #!optional buffer)
  (let ((group (buffer-group (if (default-object? buffer)
                                 (current-buffer)
                                 buffer))))
    (set-group-undo-data! group
                          (cons (mark-index (group-point group))
                                (group-undo-data group)))))

(define (modify-current-point! modifier)
  (set-current-point! (modifier (current-point))))

;;; Edwin Variables:
;;; outline-pattern: "^\n;;;;+"
;;; End:
