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
  (char-set-union char-set:graphic (ascii-range->char-set 128 255)))

(define-key 'fundamental char-set:self-insert-keys 'self-insert-command)
(define-key 'fundamental char-set:numeric 'auto-digit-argument)
(define-key 'fundamental #\- 'auto-negative-argument)
(define-key 'fundamental #\rubout 'delete-backward-char)

(define-major-mode read-only fundamental "Read-only"
  "Major mode for read-only buffers.
Like Fundamental mode, but no self-inserting characters.
Digits and - are bound to prefix argument commands.")

(define-key 'read-only char-set:self-insert-keys 'undefined)
(define-key 'read-only char-set:numeric 'digit-argument)
(define-key 'read-only #\- 'negative-argument)
(define-key 'read-only '(#\c-x #\c-q) 'no-toggle-read-only)

(define-major-mode read-only-noarg fundamental "Read-only-noarg"
  "Major mode for read-only buffers.
Like Fundamental mode, but no self-inserting characters.")

(define-key 'read-only-noarg char-set:self-insert-keys 'undefined)
(define-key 'read-only-noarg '(#\c-x #\c-q) 'no-toggle-read-only)

(define global-modes
  (list (ref-mode-object fundamental)
	(ref-mode-object read-only)
	(ref-mode-object read-only-noarg)))

(define-key 'fundamental #\c-space 'set-mark-command)
(define-key 'fundamental #\c-% 'replace-string)
(define-key 'fundamental #\c-- 'negative-argument)
(define-key 'fundamental #\c-0 'digit-argument)
(define-key 'fundamental #\c-1 'digit-argument)
(define-key 'fundamental #\c-2 'digit-argument)
(define-key 'fundamental #\c-3 'digit-argument)
(define-key 'fundamental #\c-4 'digit-argument)
(define-key 'fundamental #\c-5 'digit-argument)
(define-key 'fundamental #\c-6 'digit-argument)
(define-key 'fundamental #\c-7 'digit-argument)
(define-key 'fundamental #\c-8 'digit-argument)
(define-key 'fundamental #\c-9 'digit-argument)
(define-key 'fundamental #\c-\; 'indent-for-comment)
(define-key 'fundamental #\c-< 'mark-beginning-of-buffer)
(define-key 'fundamental #\c-= 'what-cursor-position)
(define-key 'fundamental #\c-> 'mark-end-of-buffer)
(define-key 'fundamental #\c-@ 'set-mark-command)
(define-key 'fundamental #\c-a 'beginning-of-line)
(define-key 'fundamental #\c-b 'backward-char)
(define-prefix-key 'fundamental #\c-c)
(define-key 'fundamental #\c-d 'delete-char)
(define-key 'fundamental #\c-e 'end-of-line)
(define-key 'fundamental #\c-f 'forward-char)
(define-key 'fundamental #\c-g 'keyboard-quit)
(define-prefix-key 'fundamental #\c-h 'help-prefix)
(define-key 'fundamental #\c-i 'indent-for-tab-command)
(define-key 'fundamental #\c-j 'newline-and-indent)
(define-key 'fundamental #\c-k 'kill-line)
(define-key 'fundamental #\c-l 'recenter)
(define-key 'fundamental #\c-m 'newline)
(define-key 'fundamental #\c-n 'next-line)
(define-key 'fundamental #\c-o 'open-line)
(define-key 'fundamental #\c-p 'previous-line)
(define-key 'fundamental #\c-q 'quoted-insert)
(define-key 'fundamental #\c-r 'isearch-backward)
(define-key 'fundamental #\c-s 'isearch-forward)
(define-key 'fundamental #\c-t 'transpose-chars)
(define-key 'fundamental #\c-u 'universal-argument)
(define-key 'fundamental #\c-v 'scroll-up)
(define-key 'fundamental #\c-w 'kill-region)
(define-prefix-key 'fundamental #\c-x)
(define-key 'fundamental #\c-y 'yank)
(define-key 'fundamental #\c-z 'control-meta-prefix)
(define-key 'fundamental #\c-\[ 'meta-prefix)
(define-key 'fundamental #\c-\] 'abort-recursive-edit)
(define-key 'fundamental #\c-^ 'control-prefix)
(define-key 'fundamental #\c-_ 'undo)
(define-key 'fundamental #\c-rubout 'backward-delete-char-untabify)

(define-key 'fundamental #\m-space 'just-one-space)
(define-key 'fundamental #\m-! 'shell-command)
(define-key 'fundamental #\m-% 'query-replace)
(define-key 'fundamental #\m-\' 'abbrev-prefix-mark)
(define-key 'fundamental #\m-\, 'tags-loop-continue)
(define-key 'fundamental #\m-- 'auto-argument)
(define-key 'fundamental #\m-. 'find-tag)
(define-key 'fundamental #\m-/ 'dabbrev-expand)
(define-key 'fundamental #\m-0 'auto-argument)
(define-key 'fundamental #\m-1 'auto-argument)
(define-key 'fundamental #\m-2 'auto-argument)
(define-key 'fundamental #\m-3 'auto-argument)
(define-key 'fundamental #\m-4 'auto-argument)
(define-key 'fundamental #\m-5 'auto-argument)
(define-key 'fundamental #\m-6 'auto-argument)
(define-key 'fundamental #\m-7 'auto-argument)
(define-key 'fundamental #\m-8 'auto-argument)
(define-key 'fundamental #\m-9 'auto-argument)
(define-key 'fundamental #\m-: 'eval-expression)
(define-key 'fundamental #\m-\; 'indent-for-comment)
(define-key 'fundamental #\m-< 'beginning-of-buffer)
(define-key 'fundamental #\m-= 'count-lines-region)
(define-key 'fundamental #\m-> 'end-of-buffer)
(define-key 'fundamental #\m-@ 'mark-word)
(define-key 'fundamental #\m-\[ 'backward-paragraph)
(define-key 'fundamental #\m-\\ 'delete-horizontal-space)
(define-key 'fundamental #\m-\] 'forward-paragraph)
(define-key 'fundamental #\m-\( 'insert-parentheses)
(define-key 'fundamental #\m-\) 'move-past-close-and-reindent)
(define-key 'fundamental #\m-^ 'delete-indentation)
(define-key 'fundamental #\m-a 'backward-sentence)
(define-key 'fundamental #\m-b 'backward-word)
(define-key 'fundamental #\m-c 'capitalize-word)
(define-key 'fundamental #\m-d 'kill-word)
(define-key 'fundamental #\m-e 'forward-sentence)
(define-key 'fundamental #\m-f 'forward-word)
(define-key 'fundamental #\m-g 'fill-region)
(define-key 'fundamental #\m-h 'mark-paragraph)
(define-key 'fundamental #\m-i 'tab-to-tab-stop)
(define-key 'fundamental #\m-j 'indent-new-comment-line)
(define-key 'fundamental #\m-k 'kill-sentence)
(define-key 'fundamental #\m-l 'downcase-word)
(define-key 'fundamental #\m-m 'back-to-indentation)
(define-key 'fundamental #\m-q 'fill-paragraph)
(define-key 'fundamental #\m-r 'move-to-window-line)
;; This should only be bound in NT/Windows, and only when running with
;; I/O through the scheme window as a terminal (rather than a proper screen).
(define-key 'fundamental #\m-S 'resize-screen)
(define-key 'fundamental #\m-t 'transpose-words)
(define-key 'fundamental #\m-u 'upcase-word)
(define-key 'fundamental #\m-v 'scroll-down)
(define-key 'fundamental #\m-w 'copy-region-as-kill)
(define-key 'fundamental #\m-x 'execute-extended-command)
(define-key 'fundamental #\m-y 'yank-pop)
(define-key 'fundamental #\m-z 'zap-to-char)
(define-key 'fundamental #\m-\| 'shell-command-on-region)
(define-key 'fundamental #\m-~ 'not-modified)
(define-key 'fundamental #\m-rubout 'backward-kill-word)

(define-key 'fundamental #\c-m-space 'mark-sexp)
(define-key 'fundamental #\c-m-0 'digit-argument)
(define-key 'fundamental #\c-m-1 'digit-argument)
(define-key 'fundamental #\c-m-2 'digit-argument)
(define-key 'fundamental #\c-m-3 'digit-argument)
(define-key 'fundamental #\c-m-4 'digit-argument)
(define-key 'fundamental #\c-m-5 'digit-argument)
(define-key 'fundamental #\c-m-6 'digit-argument)
(define-key 'fundamental #\c-m-7 'digit-argument)
(define-key 'fundamental #\c-m-8 'digit-argument)
(define-key 'fundamental #\c-m-9 'digit-argument)
(define-key 'fundamental #\c-m-- 'negative-argument)
(define-key 'fundamental #\c-m-\\ 'indent-region)
(define-key 'fundamental #\c-m-^ 'delete-indentation)
(define-key 'fundamental #\c-m-\( 'backward-up-list)
(define-key 'fundamental #\c-m-\) 'up-list)
(define-key 'fundamental #\c-m-@ 'mark-sexp)
(define-key 'fundamental #\c-m-\; 'kill-comment)
(define-key 'fundamental #\c-m-\[ 'eval-expression)
(define-key 'fundamental #\c-m-a 'beginning-of-defun)
(define-key 'fundamental #\c-m-b 'backward-sexp)
(define-key 'fundamental #\c-m-c 'exit-recursive-edit)
(define-key 'fundamental #\c-m-d 'down-list)
(define-key 'fundamental #\c-m-e 'end-of-defun)
(define-key 'fundamental #\c-m-f 'forward-sexp)
(define-key 'fundamental #\c-m-h 'mark-defun)
(define-key 'fundamental #\c-m-j 'indent-new-comment-line)
(define-key 'fundamental #\c-m-k 'kill-sexp)
(define-key 'fundamental #\c-m-l 'twiddle-buffers)
(define-key 'fundamental #\c-m-n 'forward-list)
(define-key 'fundamental #\c-m-o 'split-line)
(define-key 'fundamental #\c-m-p 'backward-list)
(define-key 'fundamental #\c-m-r 'align-defun)
(define-key 'fundamental #\c-m-s 'isearch-forward-regexp)
(define-key 'fundamental #\c-m-t 'transpose-sexps)
(define-key 'fundamental #\c-m-u 'backward-up-list)
(define-key 'fundamental #\c-m-v 'scroll-other-window)
(define-key 'fundamental #\c-m-w 'append-next-kill)
(define-key 'fundamental #\c-m-rubout 'backward-kill-sexp)

(define-key 'fundamental '(#\c-c #\c-i) 'insert-filename)
(define-key 'fundamental '(#\c-c #\c-s) 'repl)

(define-key 'fundamental '(#\c-h #\a) 'command-apropos)
(define-key 'fundamental '(#\c-h #\b) 'describe-bindings)
(define-key 'fundamental '(#\c-h #\c) 'describe-key-briefly)
(define-key 'fundamental '(#\c-h #\f) 'describe-function)
(define-key 'fundamental '(#\c-h #\i) 'info)
(define-key 'fundamental '(#\c-h #\k) 'describe-key)
(define-key 'fundamental '(#\c-h #\l) 'view-lossage)
(define-key 'fundamental '(#\c-h #\m) 'describe-mode)
(define-key 'fundamental '(#\c-h #\s) 'describe-syntax)
(define-key 'fundamental '(#\c-h #\t) 'help-with-tutorial)
(define-key 'fundamental '(#\c-h #\v) 'describe-variable)
(define-key 'fundamental '(#\c-h #\w) 'where-is)

(define-key 'fundamental '(#\c-x #\') 'expand-abbrev)
(define-key 'fundamental '(#\c-x #\c-\[) 'repeat-complex-command)
(define-key 'fundamental '(#\c-x #\c-b) 'list-buffers)
(define-key 'fundamental '(#\c-x #\c-c) 'save-buffers-kill-scheme)
(define-key 'fundamental '(#\c-x #\c-d) 'list-directory)
(define-key 'fundamental '(#\c-x #\c-e) 'eval-last-sexp)
(define-key 'fundamental '(#\c-x #\c-f) 'find-file)
(define-key 'fundamental '(#\c-x #\c-i) 'indent-rigidly)
(define-key 'fundamental '(#\c-x #\c-l) 'downcase-region)
(define-key 'fundamental '(#\c-x #\c-n) 'set-goal-column)
(define-key 'fundamental '(#\c-x #\c-o) 'delete-blank-lines)
(define-key 'fundamental '(#\c-x #\c-p) 'mark-page)
(define-key 'fundamental '(#\c-x #\c-q) 'toggle-read-only)
(define-key 'fundamental '(#\c-x #\c-s) 'save-buffer)
(define-key 'fundamental '(#\c-x #\c-t) 'transpose-lines)
(define-key 'fundamental '(#\c-x #\c-u) 'upcase-region)
(define-key 'fundamental '(#\c-x #\c-v) 'find-alternate-file)
(define-key 'fundamental '(#\c-x #\c-w) 'write-file)
(define-key 'fundamental '(#\c-x #\c-x) 'exchange-point-and-mark)
(define-key 'fundamental '(#\c-x #\c-z) 'suspend-scheme)
(define-key 'fundamental '(#\c-x #\() 'start-kbd-macro)
(define-key 'fundamental '(#\c-x #\)) 'end-kbd-macro)
(define-key 'fundamental '(#\c-x #\-) 'shrink-window-if-larger-than-buffer)
(define-key 'fundamental '(#\c-x #\.) 'set-fill-prefix)
(define-key 'fundamental '(#\c-x #\/) 'point-to-register)
(define-key 'fundamental '(#\c-x #\0) 'delete-window)
(define-key 'fundamental '(#\c-x #\1) 'delete-other-windows)
(define-key 'fundamental '(#\c-x #\2) 'split-window-vertically)
(define-key 'fundamental '(#\c-x #\3) 'split-window-horizontally)
(define-prefix-key 'fundamental '(#\c-x #\4))
(define-key 'fundamental '(#\C-x #\4 #\0) 'kill-buffer-and-window)
(define-key 'fundamental '(#\c-x #\4 #\c-f) 'find-file-other-window)
(define-key 'fundamental '(#\c-x #\4 #\.) 'find-tag-other-window)
(define-key 'fundamental '(#\c-x #\4 #\b) 'switch-to-buffer-other-window)
(define-key 'fundamental '(#\c-x #\4 #\d) 'dired-other-window)
(define-key 'fundamental '(#\c-x #\4 #\f) 'find-file-other-window)
(define-key 'fundamental '(#\c-x #\4 #\m) 'mail-other-window)
(define-prefix-key 'fundamental '(#\c-x #\5))
(define-key 'fundamental '(#\c-x #\5 #\c-f) 'find-file-other-frame)
(define-key 'fundamental '(#\c-x #\5 #\.) 'find-tag-other-frame)
(define-key 'fundamental '(#\c-x #\5 #\0) 'delete-frame)
(define-key 'fundamental '(#\c-x #\5 #\2) 'make-frame)
(define-key 'fundamental '(#\c-x #\5 #\b) 'switch-to-buffer-other-frame)
(define-key 'fundamental '(#\c-x #\5 #\d) 'dired-other-frame)
(define-key 'fundamental '(#\c-x #\5 #\f) 'find-file-other-frame)
(define-key 'fundamental '(#\c-x #\5 #\m) 'mail-other-frame)
(define-key 'fundamental '(#\c-x #\5 #\o) 'other-frame)

(define-key 'fundamental '(#\c-x #\;) 'set-comment-column)
(define-key 'fundamental '(#\c-x #\=) 'what-cursor-position)
(define-key 'fundamental '(#\c-x #\[) 'backward-page)
(define-key 'fundamental '(#\c-x #\]) 'forward-page)
(define-key 'fundamental '(#\c-x #\^) 'enlarge-window)
(define-prefix-key 'fundamental '(#\c-x #\a))
(define-key 'fundamental '(#\c-x #\a #\') 'expand-abbrev)
(define-key 'fundamental '(#\c-x #\a #\+) 'add-mode-abbrev)
(define-key 'fundamental '(#\c-x #\a #\-) 'inverse-add-global-abbrev)
(define-key 'fundamental '(#\c-x #\a #\c-a) 'add-mode-abbrev)
(define-key 'fundamental '(#\c-x #\a #\e) 'expand-abbrev)
(define-key 'fundamental '(#\c-x #\a #\g) 'add-global-abbrev)
(define-prefix-key 'fundamental '(#\c-x #\a #\i))
(define-key 'fundamental '(#\c-x #\a #\i #\g) 'inverse-add-global-abbrev)
(define-key 'fundamental '(#\c-x #\a #\i #\l) 'inverse-add-mode-abbrev)
(define-key 'fundamental '(#\c-x #\a #\l) 'add-mode-abbrev)
(define-key 'fundamental '(#\c-x #\b) 'switch-to-buffer)
(define-key 'fundamental '(#\c-x #\c) 'save-buffers-kill-edwin)
(define-key 'fundamental '(#\c-x #\d) 'dired)
(define-key 'fundamental '(#\c-x #\e) 'call-last-kbd-macro)
(define-key 'fundamental '(#\c-x #\f) 'set-fill-column)
(define-key 'fundamental '(#\c-x #\g) 'insert-register)
(define-key 'fundamental '(#\c-x #\h) 'mark-whole-buffer)
(define-key 'fundamental '(#\c-x #\i) 'insert-file)
(define-key 'fundamental '(#\c-x #\j) 'register-to-point)
(define-key 'fundamental '(#\c-x #\k) 'kill-buffer)
(define-key 'fundamental '(#\c-x #\l) 'count-lines-page)
(define-key 'fundamental '(#\c-x #\m) 'mail)
(define-key 'fundamental '(#\c-x #\n) 'narrow-to-region)
(define-key 'fundamental '(#\c-x #\o) 'other-window)
(define-key 'fundamental '(#\c-x #\p) 'narrow-to-page)
(define-key 'fundamental '(#\c-x #\q) 'kbd-macro-query)
(define-key 'fundamental '(#\c-x #\r) 'copy-rectangle-to-register)
(define-key 'fundamental '(#\c-x #\s) 'save-some-buffers)
(define-key 'fundamental '(#\c-x #\u) 'undo)
(define-key 'fundamental '(#\c-x #\w) 'widen)
(define-key 'fundamental '(#\c-x #\x) 'copy-to-register)
(define-key 'fundamental '(#\c-x #\z) 'suspend-edwin)
(define-key 'fundamental '(#\c-x #\{) 'shrink-window-horizontally)
(define-key 'fundamental '(#\c-x #\}) 'enlarge-window-horizontally)
(define-key 'fundamental '(#\c-x #\rubout) 'backward-kill-sentence)

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
(define-key 'fundamental (make-special-key 'page-down 1)
  'scroll-other-window-down)

;; HP bindings:
(define-key 'fundamental (make-special-key 'deletechar 0) 'delete-char)
(define-key 'fundamental (make-special-key 'deleteline 0) 'kill-line)
(define-key 'fundamental (make-special-key 'insertline 0) 'open-line)
(define-key 'fundamental (make-special-key 'next 0) 'scroll-up)
(define-key 'fundamental (make-special-key 'prior 0) 'scroll-down)
(define-key 'fundamental (make-special-key 'next 1) 'scroll-other-window)
(define-key 'fundamental (make-special-key 'prior 1) 'scroll-other-window-down)

;;; Jokes:

(define-key 'fundamental #\h-space 'hyper-space)
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
      (define-key 'fundamental '(#\c-x #\c-q) 'vc-toggle-read-only)
      (define-prefix-key 'fundamental '(#\c-x #\v))
      ;;(define-key 'fundamental '(#\c-x #\v #\a) 'vc-update-change-log)
      ;;(define-key 'fundamental '(#\c-x #\v #\c) 'vc-cancel-version)
      (define-key 'fundamental '(#\c-x #\v #\d) 'vc-directory)
      ;;(define-key 'fundamental '(#\c-x #\v #\g) 'vc-annotate)
      (define-key 'fundamental '(#\c-x #\v #\h) 'vc-insert-headers)
      (define-key 'fundamental '(#\c-x #\v #\i) 'vc-register)
      (define-key 'fundamental '(#\c-x #\v #\l) 'vc-print-log)
      ;;(define-key 'fundamental '(#\c-x #\v #\m) 'vc-merge)
      ;;(define-key 'fundamental '(#\c-x #\v #\r) 'vc-retrieve-snapshot)
      ;;(define-key 'fundamental '(#\c-x #\v #\s) 'vc-create-snapshot)
      (define-key 'fundamental '(#\c-x #\v #\u) 'vc-revert-buffer)
      (define-key 'fundamental '(#\c-x #\v #\v) 'vc-next-action)
      (define-key 'fundamental '(#\c-x #\v #\=) 'vc-diff)
      (define-key 'fundamental '(#\c-x #\v #\~) 'vc-version-other-window)
      ))