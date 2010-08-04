#| -*-Scheme-*-

$Id: modefs.scm,v 1.170 2008/01/30 20:02:03 cph Exp $

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

;;;; Fundamental Mode


(define-command fundamental-mode
  "Make the current mode be Fundamental Mode.
All normal editing modes are defined relative to this mode."
  ()
  (lambda ()
    (set-current-major-mode! (ref-mode-object fundamental))))

(define-major-mode fundamental #f "Fundamental"
  "Major mode not specialized for anything in particular.
Most other major modes are defined by comparison to this one.")

(define-variable editor-default-mode
  "The default major mode for new buffers."
  (ref-mode-object fundamental))

(define initial-buffer-name
  "*scheme*")

;; The extra range allows international keyboards to insert 8-bit characters
(define char-set:self-insert-keys
  (char-set-union char-set:digit (ucs-range->char-set 128 255)))

(define-key 'fundamental char-set:self-insert-keys 'self-insert-command)
(define-key 'fundamental char-set:digit 'auto-digit-argument)
(define-key 'fundamental (kbd #\-) 'auto-negative-argument)
(define-key 'fundamental (kbd rubout) 'delete-backward-char)

(define-major-mode read-only fundamental "Read-only"
  "Major mode for read-only buffers.
Like Fundamental mode, but no self-inserting characters.
Digits and - are bound to prefix argument commands.")

(define-key 'read-only char-set:self-insert-keys 'undefined)
(define-key 'read-only char-set:digit 'digit-argument)
(define-key 'read-only (kbd #\-) 'negative-argument)
(define-prefix-key 'read-only (kbd (ctrl #\x)))
(define-key 'read-only (kbd (ctrl #\x) (ctrl #\q)) 'no-toggle-read-only)

(define-major-mode read-only-noarg fundamental "Read-only-noarg"
  "Major mode for read-only buffers.
Like Fundamental mode, but no self-inserting characters.")

(define-key 'read-only-noarg char-set:self-insert-keys 'undefined)
(define-prefix-key 'read-only-noarg (kbd (ctrl #\x)))
(define-key 'read-only-noarg (kbd (ctrl #\x) (ctrl #\q)) 'no-toggle-read-only)

(define global-modes
  (list (ref-mode-object fundamental)
	(ref-mode-object read-only)
	(ref-mode-object read-only-noarg)))

(define-key 'fundamental (kbd (ctrl space)) 'set-mark-command)
(define-key 'fundamental (kbd (ctrl #\%)) 'replace-string)
(define-key 'fundamental (kbd (ctrl #\-)) 'negative-argument)
(define-key 'fundamental (kbd (ctrl #\0)) 'digit-argument)
(define-key 'fundamental (kbd (ctrl #\1)) 'digit-argument)
(define-key 'fundamental (kbd (ctrl #\2)) 'digit-argument)
(define-key 'fundamental (kbd (ctrl #\3)) 'digit-argument)
(define-key 'fundamental (kbd (ctrl #\4)) 'digit-argument)
(define-key 'fundamental (kbd (ctrl #\5)) 'digit-argument)
(define-key 'fundamental (kbd (ctrl #\6)) 'digit-argument)
(define-key 'fundamental (kbd (ctrl #\7)) 'digit-argument)
(define-key 'fundamental (kbd (ctrl #\8)) 'digit-argument)
(define-key 'fundamental (kbd (ctrl #\9)) 'digit-argument)
(define-key 'fundamental (kbd (ctrl #\;)) 'indent-for-comment)
(define-key 'fundamental (kbd (ctrl #\<)) 'mark-beginning-of-buffer)
(define-key 'fundamental (kbd (ctrl #\=)) 'what-cursor-position)
(define-key 'fundamental (kbd (ctrl #\>)) 'mark-end-of-buffer)
(define-key 'fundamental (kbd (ctrl #\@)) 'set-mark-command)
(define-key 'fundamental (kbd (ctrl #\a)) 'beginning-of-line)
(define-key 'fundamental (kbd (ctrl #\b)) 'backward-char)
(define-prefix-key 'fundamental (kbd (ctrl #\c)))
(define-key 'fundamental (kbd (ctrl #\d)) 'delete-char)
(define-key 'fundamental (kbd (ctrl #\e)) 'end-of-line)
(define-key 'fundamental (kbd (ctrl #\f)) 'forward-char)
(define-key 'fundamental (kbd (ctrl #\g)) 'keyboard-quit)
(define-prefix-key 'fundamental (kbd (ctrl #\h)) 'help-prefix)
(define-key 'fundamental (kbd (ctrl #\i)) 'indent-for-tab-command)
(define-key 'fundamental (kbd (ctrl #\j)) 'newline-and-indent)
(define-key 'fundamental (kbd (ctrl #\k)) 'kill-line)
(define-key 'fundamental (kbd (ctrl #\l)) 'recenter)
(define-key 'fundamental (kbd (ctrl #\m)) 'newline)
(define-key 'fundamental (kbd (ctrl #\n)) 'next-line)
(define-key 'fundamental (kbd (ctrl #\o)) 'open-line)
(define-key 'fundamental (kbd (ctrl #\p)) 'previous-line)
(define-key 'fundamental (kbd (ctrl #\q)) 'quoted-insert)
(define-key 'fundamental (kbd (ctrl #\r)) 'isearch-backward)
(define-key 'fundamental (kbd (ctrl #\s)) 'isearch-forward)
(define-key 'fundamental (kbd (ctrl #\t)) 'transpose-chars)
(define-key 'fundamental (kbd (ctrl #\u)) 'universal-argument)
(define-key 'fundamental (kbd (ctrl #\v)) 'scroll-up)
(define-key 'fundamental (kbd (ctrl #\w)) 'kill-region)
(define-prefix-key 'fundamental (kbd (ctrl #\x)))
(define-key 'fundamental (kbd (ctrl #\y)) 'yank)
(define-key 'fundamental (kbd (ctrl #\z)) 'control-meta-prefix)
(define-key 'fundamental (kbd (ctrl #\[)) 'meta-prefix)
(define-key 'fundamental (kbd (ctrl #\])) 'abort-recursive-edit)
(define-key 'fundamental (kbd (ctrl #\^)) 'control-prefix)
(define-key 'fundamental (kbd (ctrl #\_)) 'undo)
(define-key 'fundamental (kbd (ctrl rubout)) 'backward-delete-char-untabify)

(define-key 'fundamental (kbd (meta space)) 'just-one-space)
(define-key 'fundamental (kbd (meta #\!)) 'shell-command)
(define-key 'fundamental (kbd (meta #\%)) 'query-replace)
(define-key 'fundamental (kbd (meta #\')) 'abbrev-prefix-mark)
(define-key 'fundamental (kbd (meta #\,)) 'tags-loop-continue)
(define-key 'fundamental (kbd (meta #\-)) 'auto-argument)
(define-key 'fundamental (kbd (meta #\.)) 'find-tag)
(define-key 'fundamental (kbd (meta #\/)) 'dabbrev-expand)
(define-key 'fundamental (kbd (meta #\0)) 'auto-argument)
(define-key 'fundamental (kbd (meta #\1)) 'auto-argument)
(define-key 'fundamental (kbd (meta #\2)) 'auto-argument)
(define-key 'fundamental (kbd (meta #\3)) 'auto-argument)
(define-key 'fundamental (kbd (meta #\4)) 'auto-argument)
(define-key 'fundamental (kbd (meta #\5)) 'auto-argument)
(define-key 'fundamental (kbd (meta #\6)) 'auto-argument)
(define-key 'fundamental (kbd (meta #\7)) 'auto-argument)
(define-key 'fundamental (kbd (meta #\8)) 'auto-argument)
(define-key 'fundamental (kbd (meta #\9)) 'auto-argument)
(define-key 'fundamental (kbd (meta #\:)) 'eval-expression)
(define-key 'fundamental (kbd (meta #\;)) 'indent-for-comment)
(define-key 'fundamental (kbd (meta #\<)) 'beginning-of-buffer)
(define-key 'fundamental (kbd (meta #\=)) 'count-lines-region)
(define-key 'fundamental (kbd (meta #\>)) 'end-of-buffer)
(define-key 'fundamental (kbd (meta #\@)) 'mark-word)
(define-key 'fundamental (kbd (meta #\[)) 'backward-paragraph)
(define-key 'fundamental (kbd (meta #\\)) 'delete-horizontal-space)
(define-key 'fundamental (kbd (meta #\])) 'forward-paragraph)
(define-key 'fundamental (kbd (meta #\()) 'insert-parentheses)
(define-key 'fundamental (kbd (meta #\))) 'move-past-close-and-reindent)
(define-key 'fundamental (kbd (meta #\^)) 'delete-indentation)
(define-key 'fundamental (kbd (meta #\a)) 'backward-sentence)
(define-key 'fundamental (kbd (meta #\b)) 'backward-word)
(define-key 'fundamental (kbd (meta #\c)) 'capitalize-word)
(define-key 'fundamental (kbd (meta #\d)) 'kill-word)
(define-key 'fundamental (kbd (meta #\e)) 'forward-sentence)
(define-key 'fundamental (kbd (meta #\f)) 'forward-word)
(define-key 'fundamental (kbd (meta #\g)) 'fill-region)
(define-key 'fundamental (kbd (meta #\h)) 'mark-paragraph)
(define-key 'fundamental (kbd (meta #\i)) 'tab-to-tab-stop)
(define-key 'fundamental (kbd (meta #\j)) 'indent-new-comment-line)
(define-key 'fundamental (kbd (meta #\k)) 'kill-sentence)
(define-key 'fundamental (kbd (meta #\l)) 'downcase-word)
(define-key 'fundamental (kbd (meta #\m)) 'back-to-indentation)
(define-key 'fundamental (kbd (meta #\q)) 'fill-paragraph)
(define-key 'fundamental (kbd (meta #\r)) 'move-to-window-line)
;; This should only be bound in NT/Windows, and only when running with
;; I/O through the scheme window as a terminal (rather than a proper screen).
(define-key 'fundamental (kbd (meta #\S)) 'resize-screen)
(define-key 'fundamental (kbd (meta #\t)) 'transpose-words)
(define-key 'fundamental (kbd (meta #\u)) 'upcase-word)
(define-key 'fundamental (kbd (meta #\v)) 'scroll-down)
(define-key 'fundamental (kbd (meta #\w)) 'copy-region-as-kill)
(define-key 'fundamental (kbd (meta #\x)) 'execute-extended-command)
(define-key 'fundamental (kbd (meta #\y)) 'yank-pop)
(define-key 'fundamental (kbd (meta #\z)) 'zap-to-char)
(define-key 'fundamental (kbd (meta #\|)) 'shell-command-on-region)
(define-key 'fundamental (kbd (meta #\~)) 'not-modified)
(define-key 'fundamental (kbd (meta rubout)) 'backward-kill-word)

(define-key 'fundamental (kbd (ctrl meta #\space)) 'mark-sexp)
(define-key 'fundamental (kbd (ctrl meta #\0)) 'digit-argument)
(define-key 'fundamental (kbd (ctrl meta #\1)) 'digit-argument)
(define-key 'fundamental (kbd (ctrl meta #\2)) 'digit-argument)
(define-key 'fundamental (kbd (ctrl meta #\3)) 'digit-argument)
(define-key 'fundamental (kbd (ctrl meta #\4)) 'digit-argument)
(define-key 'fundamental (kbd (ctrl meta #\5)) 'digit-argument)
(define-key 'fundamental (kbd (ctrl meta #\6)) 'digit-argument)
(define-key 'fundamental (kbd (ctrl meta #\7)) 'digit-argument)
(define-key 'fundamental (kbd (ctrl meta #\8)) 'digit-argument)
(define-key 'fundamental (kbd (ctrl meta #\9)) 'digit-argument)
(define-key 'fundamental (kbd (ctrl meta #\-)) 'negative-argument)
(define-key 'fundamental (kbd (ctrl meta #\\)) 'indent-region)
(define-key 'fundamental (kbd (ctrl meta #\^)) 'delete-indentation)
(define-key 'fundamental (kbd (ctrl meta #\()) 'backward-up-list)
(define-key 'fundamental (kbd (ctrl meta #\))) 'up-list)
(define-key 'fundamental (kbd (ctrl meta #\@)) 'mark-sexp)
(define-key 'fundamental (kbd (ctrl meta #\;)) 'kill-comment)
(define-key 'fundamental (kbd (ctrl meta #\[)) 'eval-expression)
(define-key 'fundamental (kbd (ctrl meta #\a)) 'beginning-of-defun)
(define-key 'fundamental (kbd (ctrl meta #\b)) 'backward-sexp)
(define-key 'fundamental (kbd (ctrl meta #\c)) 'exit-recursive-edit)
(define-key 'fundamental (kbd (ctrl meta #\d)) 'down-list)
(define-key 'fundamental (kbd (ctrl meta #\e)) 'end-of-defun)
(define-key 'fundamental (kbd (ctrl meta #\f)) 'forward-sexp)
(define-key 'fundamental (kbd (ctrl meta #\h)) 'mark-defun)
(define-key 'fundamental (kbd (ctrl meta #\j)) 'indent-new-comment-line)
(define-key 'fundamental (kbd (ctrl meta #\k)) 'kill-sexp)
(define-key 'fundamental (kbd (ctrl meta #\l)) 'twiddle-buffers)
(define-key 'fundamental (kbd (ctrl meta #\n)) 'forward-list)
(define-key 'fundamental (kbd (ctrl meta #\o)) 'split-line)
(define-key 'fundamental (kbd (ctrl meta #\p)) 'backward-list)
(define-key 'fundamental (kbd (ctrl meta #\r)) 'align-defun)
(define-key 'fundamental (kbd (ctrl meta #\s)) 'isearch-forward-regexp)
(define-key 'fundamental (kbd (ctrl meta #\t)) 'transpose-sexps)
(define-key 'fundamental (kbd (ctrl meta #\u)) 'backward-up-list)
(define-key 'fundamental (kbd (ctrl meta #\v)) 'scroll-other-window)
(define-key 'fundamental (kbd (ctrl meta #\w)) 'append-next-kill)
(define-key 'fundamental (kbd (ctrl meta #\rubout)) 'backward-kill-sexp)

(define-key 'fundamental (kbd (ctrl #\c) (ctrl #\i)) 'insert-filename)
(define-key 'fundamental (kbd (ctrl #\c) (ctrl #\s)) 'repl)

(define-key 'fundamental (kbd (ctrl #\h) #\a) 'command-apropos)
(define-key 'fundamental (kbd (ctrl #\h) #\b) 'describe-bindings)
(define-key 'fundamental (kbd (ctrl #\h) #\c) 'describe-key-briefly)
(define-key 'fundamental (kbd (ctrl #\h) #\f) 'describe-function)
(define-key 'fundamental (kbd (ctrl #\h) #\i) 'info)
(define-key 'fundamental (kbd (ctrl #\h) #\k) 'describe-key)
(define-key 'fundamental (kbd (ctrl #\h) #\l) 'view-lossage)
(define-key 'fundamental (kbd (ctrl #\h) #\m) 'describe-mode)
(define-key 'fundamental (kbd (ctrl #\h) #\s) 'describe-syntax)
(define-key 'fundamental (kbd (ctrl #\h) #\t) 'help-with-tutorial)
(define-key 'fundamental (kbd (ctrl #\h) #\v) 'describe-variable)
(define-key 'fundamental (kbd (ctrl #\h) #\w) 'where-is)

(define-key 'fundamental (kbd (ctrl #\x) #\') 'expand-abbrev)
(define-key 'fundamental (kbd (ctrl #\x) (ctrl #\[)) 'repeat-complex-command)
(define-key 'fundamental (kbd (ctrl #\x) (ctrl #\b)) 'list-buffers)
(define-key 'fundamental (kbd (ctrl #\x) (ctrl #\c)) 'save-buffers-kill-scheme)
(define-key 'fundamental (kbd (ctrl #\x) (ctrl #\d)) 'list-directory)
(define-key 'fundamental (kbd (ctrl #\x) (ctrl #\e)) 'eval-last-sexp)
(define-key 'fundamental (kbd (ctrl #\x) (ctrl #\f)) 'find-file)
(define-key 'fundamental (kbd (ctrl #\x) (ctrl #\i)) 'indent-rigidly)
(define-key 'fundamental (kbd (ctrl #\x) (ctrl #\l)) 'downcase-region)
(define-key 'fundamental (kbd (ctrl #\x) (ctrl #\n)) 'set-goal-column)
(define-key 'fundamental (kbd (ctrl #\x) (ctrl #\o)) 'delete-blank-lines)
(define-key 'fundamental (kbd (ctrl #\x) (ctrl #\p)) 'mark-page)
(define-key 'fundamental (kbd (ctrl #\x) (ctrl #\q)) 'toggle-read-only)
(define-key 'fundamental (kbd (ctrl #\x) (ctrl #\s)) 'save-buffer)
(define-key 'fundamental (kbd (ctrl #\x) (ctrl #\t)) 'transpose-lines)
(define-key 'fundamental (kbd (ctrl #\x) (ctrl #\u)) 'upcase-region)
(define-key 'fundamental (kbd (ctrl #\x) (ctrl #\v)) 'find-alternate-file)
(define-key 'fundamental (kbd (ctrl #\x) (ctrl #\w)) 'write-file)
(define-key 'fundamental (kbd (ctrl #\x) (ctrl #\x)) 'exchange-point-and-mark)
(define-key 'fundamental (kbd (ctrl #\x) (ctrl #\z)) 'suspend-scheme)
(define-key 'fundamental (kbd (ctrl #\x) #\() 'start-kbd-macro)
(define-key 'fundamental (kbd (ctrl #\x) #\)) 'end-kbd-macro)
(define-key 'fundamental (kbd (ctrl #\x) #\-) 'shrink-window-if-larger-than-buffer)
(define-key 'fundamental (kbd (ctrl #\x) #\.) 'set-fill-prefix)
(define-key 'fundamental (kbd (ctrl #\x) #\/) 'point-to-register)
(define-key 'fundamental (kbd (ctrl #\x) #\0) 'delete-window)
(define-key 'fundamental (kbd (ctrl #\x) #\1) 'delete-other-windows)
(define-key 'fundamental (kbd (ctrl #\x) #\2) 'split-window-vertically)
(define-key 'fundamental (kbd (ctrl #\x) #\3) 'split-window-horizontally)
(define-prefix-key 'fundamental (kbd (ctrl #\x) #\4))
(define-key 'fundamental (kbd (ctrl #\x) #\4 #\0) 'kill-buffer-and-window)
(define-key 'fundamental (kbd (ctrl #\x) #\4 (ctrl #\f)) 'find-file-other-window)
(define-key 'fundamental (kbd (ctrl #\x) #\4 #\.) 'find-tag-other-window)
(define-key 'fundamental (kbd (ctrl #\x) #\4 #\b) 'switch-to-buffer-other-window)
(define-key 'fundamental (kbd (ctrl #\x) #\4 #\d) 'dired-other-window)
(define-key 'fundamental (kbd (ctrl #\x) #\4 #\f) 'find-file-other-window)
(define-key 'fundamental (kbd (ctrl #\x) #\4 #\m) 'mail-other-window)
(define-prefix-key 'fundamental (kbd (ctrl #\x) #\5))
(define-key 'fundamental (kbd (ctrl #\x) #\5 (ctrl #\f)) 'find-file-other-frame)
(define-key 'fundamental (kbd (ctrl #\x) #\5 #\.) 'find-tag-other-frame)
(define-key 'fundamental (kbd (ctrl #\x) #\5 #\0) 'delete-frame)
(define-key 'fundamental (kbd (ctrl #\x) #\5 #\2) 'make-frame)
(define-key 'fundamental (kbd (ctrl #\x) #\5 #\b) 'switch-to-buffer-other-frame)
(define-key 'fundamental (kbd (ctrl #\x) #\5 #\d) 'dired-other-frame)
(define-key 'fundamental (kbd (ctrl #\x) #\5 #\f) 'find-file-other-frame)
(define-key 'fundamental (kbd (ctrl #\x) #\5 #\m) 'mail-other-frame)
(define-key 'fundamental (kbd (ctrl #\x) #\5 #\o) 'other-frame)

(define-key 'fundamental (kbd (ctrl #\x) #\;) 'set-comment-column)
(define-key 'fundamental (kbd (ctrl #\x) #\=) 'what-cursor-position)
(define-key 'fundamental (kbd (ctrl #\x) #\[) 'backward-page)
(define-key 'fundamental (kbd (ctrl #\x) #\]) 'forward-page)
(define-key 'fundamental (kbd (ctrl #\x) #\^) 'enlarge-window)
(define-prefix-key 'fundamental (kbd (ctrl #\x) #\a))
(define-key 'fundamental (kbd (ctrl #\x) #\a #\') 'expand-abbrev)
(define-key 'fundamental (kbd (ctrl #\x) #\a #\+) 'add-mode-abbrev)
(define-key 'fundamental (kbd (ctrl #\x) #\a #\-) 'inverse-add-global-abbrev)
(define-key 'fundamental (kbd (ctrl #\x) #\a (ctrl a)) 'add-mode-abbrev)
(define-key 'fundamental (kbd (ctrl #\x) #\a #\e) 'expand-abbrev)
(define-key 'fundamental (kbd (ctrl #\x) #\a #\g) 'add-global-abbrev)
(define-prefix-key 'fundamental (kbd (ctrl #\x) #\a #\i))
(define-key 'fundamental (kbd (ctrl #\x) #\a #\i #\g) 'inverse-add-global-abbrev)
(define-key 'fundamental (kbd (ctrl #\x) #\a #\i #\l) 'inverse-add-mode-abbrev)
(define-key 'fundamental (kbd (ctrl #\x) #\a #\l) 'add-mode-abbrev)
(define-key 'fundamental (kbd (ctrl #\x) #\b) 'switch-to-buffer)
(define-key 'fundamental (kbd (ctrl #\x) #\c) 'save-buffers-kill-edwin)
(define-key 'fundamental (kbd (ctrl #\x) #\d) 'dired)
(define-key 'fundamental (kbd (ctrl #\x) #\e) 'call-last-kbd-macro)
(define-key 'fundamental (kbd (ctrl #\x) #\f) 'set-fill-column)
(define-key 'fundamental (kbd (ctrl #\x) #\g) 'insert-register)
(define-key 'fundamental (kbd (ctrl #\x) #\h) 'mark-whole-buffer)
(define-key 'fundamental (kbd (ctrl #\x) #\i) 'insert-file)
(define-key 'fundamental (kbd (ctrl #\x) #\j) 'register-to-point)
(define-key 'fundamental (kbd (ctrl #\x) #\k) 'kill-buffer)
(define-key 'fundamental (kbd (ctrl #\x) #\l) 'count-lines-page)
(define-key 'fundamental (kbd (ctrl #\x) #\m) 'mail)
(define-key 'fundamental (kbd (ctrl #\x) #\n) 'narrow-to-region)
(define-key 'fundamental (kbd (ctrl #\x) #\o) 'other-window)
(define-key 'fundamental (kbd (ctrl #\x) #\p) 'narrow-to-page)
(define-key 'fundamental (kbd (ctrl #\x) #\q) 'kbd-macro-query)
(define-key 'fundamental (kbd (ctrl #\x) #\r) 'copy-rectangle-to-register)
(define-key 'fundamental (kbd (ctrl #\x) #\s) 'save-some-buffers)
(define-key 'fundamental (kbd (ctrl #\x) #\u) 'undo)
(define-key 'fundamental (kbd (ctrl #\x) #\w) 'widen)
(define-key 'fundamental (kbd (ctrl #\x) #\x) 'copy-to-register)
(define-key 'fundamental (kbd (ctrl #\x) #\z) 'suspend-edwin)
(define-key 'fundamental (kbd (ctrl #\x) #\{) 'shrink-window-horizontally)
(define-key 'fundamental (kbd (ctrl #\x) #\}) 'enlarge-window-horizontally)
(define-key 'fundamental (kbd (ctrl #\x) rubout) 'backward-kill-sentence)

;;; Additional bindings to `standard' special keys:

(define-key 'fundamental (make-special-key 'down 0) 'next-line)
(define-key 'fundamental (make-special-key 'up 0) 'previous-line)
(define-key 'fundamental (make-special-key 'left 0) 'backward-char)
(define-key 'fundamental (make-special-key 'right 0) 'forward-char)
(define-key 'fundamental (make-special-key 'left 1) 'backward-word)
(define-key 'fundamental (make-special-key 'right 1) 'forward-word)

;; PC bindings:
(define-key 'fundamental (make-special-key 'home 0) 'beginning-of-line)
(define-key 'fundamental (make-special-key 'end 0) 'end-of-line)
(define-key 'fundamental (make-special-key 'delete 0) 'delete-char)
(define-key 'fundamental (make-special-key 'page-up 0) 'scroll-down)
(define-key 'fundamental (make-special-key 'page-down 0) 'scroll-up)
(define-key 'fundamental (make-special-key 'page-up 1) 'scroll-other-window)
(define-key 'fundamental (make-special-key 'page-down 1) 'scroll-other-window-down)

;; HP bindings:
(define-key 'fundamental (make-special-key 'deletechar 0) 'delete-char)
(define-key 'fundamental (make-special-key 'deleteline 0) 'kill-line)
(define-key 'fundamental (make-special-key 'insertline 0) 'open-line)
(define-key 'fundamental (make-special-key 'next 0) 'scroll-up)
(define-key 'fundamental (make-special-key 'prior 0) 'scroll-down)
(define-key 'fundamental (make-special-key 'next 1) 'scroll-other-window)
(define-key 'fundamental (make-special-key 'prior 1) 'scroll-other-window-down)

;;; Jokes:

(define-key 'fundamental (kbd h #\space) 'hyper-space)
(define-key 'fundamental (make-special-key 'malesymbol 4) 'super-man)
(define-key 'fundamental (make-special-key 'menu 4) 'super-menu)

;;; Mouse buttons:

(define-key 'fundamental button1-down 'mouse-set-point)
;; Next two are for wheel mouse under X.
(define-key 'fundamental button4-down 'mouse-scroll-down)
(define-key 'fundamental button5-down 'mouse-scroll-up)
(define-key 'fundamental button1-up 'mouse-ignore)
(define-key 'fundamental button2-up 'mouse-ignore)
(define-key 'fundamental button3-up 'mouse-ignore)
(define-key 'fundamental button4-up 'mouse-ignore)
(define-key 'fundamental button5-up 'mouse-ignore)

;; Bind VC keys only if VC is loaded.
(if (name->command 'vc-toggle-read-only #f)
    (begin
      (define-key 'fundamental (kbd (ctrl x) (ctrl q)) 'vc-toggle-read-only)
      (define-prefix-key 'fundamental (kbd (ctrl x) #\v))
      ;;(define-key 'fundamental (kbd (ctrl x) #\v #\a) 'vc-update-change-log)
      ;;(define-key 'fundamental (kbd (ctrl x) #\v #\c) 'vc-cancel-version)
      (define-key 'fundamental (kbd (ctrl x) #\v #\d) 'vc-directory)
      ;;(define-key 'fundamental (kbd (ctrl x) #\v #\g) 'vc-annotate)
      (define-key 'fundamental (kbd (ctrl x) #\v #\h) 'vc-insert-headers)
      (define-key 'fundamental (kbd (ctrl x) #\v #\i) 'vc-register)
      (define-key 'fundamental (kbd (ctrl x) #\v #\l) 'vc-print-log)
      ;;(define-key 'fundamental (kbd (ctrl x) #\v #\m) 'vc-merge)
      ;;(define-key 'fundamental (kbd (ctrl x) #\v #\r) 'vc-retrieve-snapshot)
      ;;(define-key 'fundamental (kbd (ctrl x) #\v #\s) 'vc-create-snapshot)
      (define-key 'fundamental (kbd (ctrl x) #\v #\u) 'vc-revert-buffer)
      (define-key 'fundamental (kbd (ctrl x) #\v #\v) 'vc-next-action)
      (define-key 'fundamental (kbd (ctrl x) #\v #\=) 'vc-diff)
      (define-key 'fundamental (kbd (ctrl x) #\v #\~) 'vc-version-other-window)
      ))