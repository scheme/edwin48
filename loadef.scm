;;; -*-Scheme-*-
;;;
;;; $Id: loadef.scm,v 1.52 2007/01/05 21:19:23 cph Exp $
;;;
;;; Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
;;;     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;;     2006, 2007 Massachusetts Institute of Technology
;;;
;;; This file is part of MIT/GNU Scheme.
;;;
;;; MIT/GNU Scheme is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or (at
;;; your option) any later version.
;;;
;;; MIT/GNU Scheme is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with MIT/GNU Scheme; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
;;; USA.
;;;
;;;

;;;; Autoload Definitions


;;; ****************

(define-library 'TECHINFO-MODE
  '("techinfo" (EDWIN)))

(define-autoload-major-mode 'techinfo 'fundamental "TechInfo" 'TECHINFO-MODE
  "Mode for accessing the TechInfo database.")

(define-autoload-command 'techinfo 'TECHINFO-MODE
  "Enter TechInfo mode.")

;;; ****************

(define-library 'TELNET-MODE
  '("telnet" (EDWIN)))

(define-autoload-major-mode 'telnet 'comint "Telnet" 'TELNET-MODE
  "Major mode for interacting with the Telnet program.")

(define-autoload-command 'telnet 'TELNET-MODE
  "Telnet to remote host.")

(define-variable telnet-mode-hook
  "An event distributor that is invoked when entering Telnet mode."
  (make-event-distributor))

;;; ****************

(define-library 'MIDAS-MODE
  '("midas" (EDWIN)))

(define-autoload-major-mode 'midas 'fundamental "Midas" 'MIDAS-MODE
  "Major mode for editing assembly code.")

(define-autoload-command 'midas-mode 'MIDAS-MODE
  "Enter Midas mode.")

(define-variable midas-mode-abbrev-table
  "Mode-specific abbrev table for assembly code.")
(define-abbrev-table 'midas-mode-abbrev-table '())

(define-variable midas-mode-hook
  "An event distributor that is invoked when entering Midas mode."
  (make-event-distributor))

;;; ****************

(define-library 'PASCAL-MODE
  '("pasmod" (EDWIN)))

(define-autoload-major-mode 'pascal 'fundamental "Pascal" 'PASCAL-MODE
  "Major mode specialized for editing Pascal code.")

(define-autoload-command 'pascal-mode 'PASCAL-MODE
  "Enter Pascal mode.")

(define-variable pascal-mode-abbrev-table
  "Mode-specific abbrev table for Pascal code.")
(define-abbrev-table 'pascal-mode-abbrev-table '())

(define-variable pascal-mode-hook
  "An event distributor that is invoked when entering Pascal mode."
  (make-event-distributor))

(define-variable pascal-shift-increment
  "Indentation increment for Pascal Shift commands."
  2)

(define-variable pascal-indentation-keywords
  "These keywords cause the lines below them to be indented to the right.
This must be a regular expression, or #F to disable the option."
  false)

;;; ****************

(define-library 'TEXINFO-MODE
  '("tximod" (EDWIN)))

(define-autoload-major-mode 'texinfo 'text "Texinfo" 'TEXINFO-MODE
  "Major mode for editing Texinfo files.")

(define-autoload-command 'texinfo-mode 'TEXINFO-MODE
  "Make the current mode be Texinfo mode.")

(define-variable texinfo-mode-abbrev-table
  "Mode-specific abbrev table for Texinfo.")
(define-abbrev-table 'texinfo-mode-abbrev-table '())

(define-variable texinfo-mode-hook
  "An event distributor that is invoked when entering Texinfo mode."
  (make-event-distributor))

;;; ****************

(define-library 'manual
  '("manual" (EDWIN)))

(define-autoload-command 'manual-entry 'MANUAL
  "Display UNIX man page.")

(define-autoload-command 'clean-manual-entry 'MANUAL
  "Clean the unix manual entry in the current buffer.
The current buffer should contain a formatted manual entry.")

(define-variable manual-entry-reuse-buffer?
  "If true, MANUAL-ENTRY uses buffer *Manual-Entry* for all entries.
Otherwise, a new buffer is created for each topic."
  false
  boolean?)

(define-variable manual-command
  "A string containing the manual page formatting command.  
Section (if any) and topic strings are appended (with space separators)
and the resulting string is provided to a shell running in a subprocess."
  false
  string-or-false?)

;;; ****************

(define-library 'print
  '("print" (EDWIN)))

(define-variable lpr-procedure
  "Procedure that spools some text to the printer, or #F for the default.
Procedure is called with four arguments: a region to be printed, a flag
indicating that the text should be printed with page headers, a title string
to appear in the header lines and on the title page, and the buffer in which
the text was originally stored (for editor variable references).  If this
variable's value is #F, the text is printed using LPR-COMMAND."
  false
  (lambda (object) (or (not object) (procedure? object))))

(define-variable lpr-command
  "Shell command for printing a file"
  "lpr"
  string?)

(define-variable lpr-switches
  "List of strings to pass as extra switch args to lpr when it is invoked."
  '()
  list-of-strings?)

(define lpr-prompt-for-name?
  ;; If true, lpr commands prompt for a name to appear on the title page.
  false)

(define lpr-print-not-special?
  ;; If true, the print-* commands are just like the lpr-* commands.
  false)

(define-autoload-command 'lpr-buffer 'PRINT
  "Print buffer contents with Unix command `lpr'.")

(define-autoload-command 'print-buffer 'PRINT
  "Print buffer contents as with Unix command `lpr -p'.")

(define-autoload-command 'lpr-region 'PRINT
  "Print region contents as with Unix command `lpr'.")

(define-autoload-command 'print-region 'PRINT
  "Print region contents as with Unix command `lpr -p'.")

;;; ****************

(define-library 'SORT
  '("sort" (EDWIN)))

(define-autoload-command 'sort-lines 'SORT
  "Sort lines by their text.")

(define-autoload-command 'sort-pages 'SORT
  "Sort pages by their text.")

(define-autoload-command 'sort-paragraphs 'SORT
  "Sort paragraphs by their text.")

(define-autoload-command 'sort-fields 'SORT
  "Sort lines by the text of a field.")

(define-autoload-command 'sort-numeric-fields 'SORT
  "Sort lines by the numeric value of a field.")

(define-autoload-command 'sort-columns 'SORT
  "Sort lines by the text in a range of columns.")

;;; ****************

(define-library 'STEPPER
  '("eystep" (EDWIN STEPPER)))

(define-autoload-command 'step-expression 'STEPPER
  "Single-step an expression.")

(define-autoload-command 'step-last-sexp 'STEPPER
  "Single-step the expression preceding point.")

(define-autoload-command 'step-defun 'STEPPER
  "Single-step the definition that the point is in or before.")

(define-library 'PAREDIT
  '("paredit" (EDWIN PAREDIT)))

(define-autoload-minor-mode 'paredit "Paredit" 'PAREDIT
  "Minor mode for pseudo-structurally editing Lisp code.")

(define-autoload-command 'paredit-mode 'PAREDIT
  "Toggle pseudo-structural editing of Lisp code.")

;;; ****************

(define-library 'NEWS-READER
  '("nntp" (EDWIN NNTP))
  '("snr" (EDWIN NEWS-READER)))

(define-autoload-command 'rnews 'NEWS-READER
  "Start a News reader.
Normally uses the server specified by the variable news-server,
but with a prefix arg prompts for the server name.
Only one News reader may be open per server; if a previous News reader
is open the that server, its buffer is selected.")

;;; ****************

(define-library 'VERILOG-MODE
  '("verilog" (EDWIN VERILOG)))

(define-autoload-major-mode 'verilog 'fundamental "Verilog" 'VERILOG-MODE
  "Major mode specialized for editing Verilog code.")

(define-autoload-command 'verilog-mode 'VERILOG-MODE
  "Enter Verilog mode.")

(define-variable verilog-mode-abbrev-table
  "Mode-specific abbrev table for Verilog code.")
(define-abbrev-table 'verilog-mode-abbrev-table '())

(define-variable verilog-mode-hook
  "An event distributor that is invoked when entering Verilog mode."
  (make-event-distributor))

(define-variable verilog-continued-statement-offset
  "Extra indent for lines not starting new statements."
  2
  exact-nonnegative-integer?)

(define-variable verilog-continued-header-offset
  "Extra indent for continuation lines of structure headers."
  4
  exact-nonnegative-integer?)

;;; ****************

(define-library 'VHDL-MODE
  '("vhdl" (EDWIN VHDL)))

(define-autoload-major-mode 'vhdl 'fundamental "VHDL" 'VHDL-MODE
  "Major mode specialized for editing VHDL code.")

(define-autoload-command 'vhdl-mode 'VHDL-MODE
  "Enter VHDL mode.")

(define-variable vhdl-mode-abbrev-table
  "Mode-specific abbrev table for VHDL code.")
(define-abbrev-table 'vhdl-mode-abbrev-table '())

(define-variable vhdl-mode-hook
  "An event distributor that is invoked when entering VHDL mode."
  (make-event-distributor))

(define-variable vhdl-continued-header-offset
  "Extra indent for continuation lines of structure headers."
  4
  exact-nonnegative-integer?)

(define-variable vhdl-continued-statement-offset
  "Extra indent for lines not starting new statements."
  2
  exact-nonnegative-integer?)

;;; ****************

(define-library 'WEBSTER
  '("webster" (EDWIN)))

(define-autoload-major-mode 'webster 'read-only "Webster" 'WEBSTER
  "Major mode for interacting with webster server.")

(define-autoload-command 'webster 'WEBSTER
  "Look up a word in Webster's dictionary.")

(define-autoload-command 'webster-define 'WEBSTER
  "Look up a word in Webster's dictionary.")

(define-autoload-command 'webster-endings 'WEBSTER
  "Look up possible endings for a word in Webster's dictionary.")

(define-autoload-command 'webster-spellings 'WEBSTER
  "Look up possible correct spellings for a word in Webster's dictionary.")

(define-variable webster-server
  "Host name of a webster server, specified as a string."
  #f
  string-or-false?)

(define-variable webster-port
  "TCP port of webster server on webster-server, specified as an integer.
This is usually 103 or 2627."
  103
  exact-nonnegative-integer?)

(define-variable webster-mode-hook
  "Hook to be run by webster-mode, after everything else."
  (make-event-distributor))

(define-variable webster-buffer-name
  "The name to use for webster interaction buffer."
  "*webster*"
  string?)

;;; ****************

(define-library 'LISPPASTE
  '("lisppaste" (EDWIN LISPPASTE)))

(define-autoload-command 'lisppaste-channels 'LISPPASTE
  "List all the channels supported by lisppaste in a temporary buffer.")

(define-autoload-command 'lisppaste-insert-paste 'LISPPASTE
  "Insert the numbered paste at the point.
With a prefix argument, also show a header describing the paste.")

(define-autoload-command 'lisppaste-insert-annotation 'LISPPASTE
  "Insert the annotation of the numbered paste at the point.
With a prefix argument, also show a header describing the annotation.")

(define-autoload-command 'lisppaste-buffer 'LISPPASTE
  "Create a new paste of the current buffer.")

(define-autoload-command 'lisppaste-region 'LISPPASTE
  "Create a new paste of the current region.")

(define-autoload-command 'lisppaste-annotate-with-buffer 'LISPPASTE
  "Annotate an existing paste with the current buffer.")

(define-autoload-command 'lisppaste-annotate-with-region 'LISPPASTE
  "Annotate an existing paste with the region.")

(define-autoload-command 'lisppaste-list-pastes 'LISPPASTE
  "List the headers of the last number of pastes.
With a prefix argument, list pastes starting at a certain number.")

(define-autoload-command 'lisppaste-list-channel-pastes 'LISPPASTE
  "List the headers of the last few pastes in a certain channel.
With a prefix argument, list pastes starting at a certain number.")

(define-variable lisppaste-rpc-uri
  "URI of the lisppaste XML-RPC service."
  "http://common-lisp.net:8185/RPC2"
  ->uri)

(define-variable lisppaste-default-channel
  "Default channel for lisppaste requests."
  #f
  string?)

(define-variable lisppaste-default-nickname
  "Default IRC nickname for lisppaste requests."
  #f
  string?)

;;; ****************

(define-library 'PASSWORD-EDIT
  '("pwedit" (EDWIN PASSWORD-EDIT))
  '("pwparse" (EDWIN PASSWORD-EDIT)))

(define-autoload-command 'view-password-file 'PASSWORD-EDIT
  "Read in a password file and show it in password-view mode.
Reads the file specified in the variable password-file.
If password-file is #f, or if prefix arg supplied, prompts for a filename.")

(define-variable password-file
  "Name of file containing passwords, or #F meaning prompt for name.
If this specifies a relative pathname, it is relative to the home directory.
See \\[view-password-file]."
  #f
  (lambda (object)
    (or (not object)
	(string? object)
	(pathname? object))))

(define-autoload-major-mode 'password-view 'read-only "Password-View"
  'PASSWORD-EDIT
  "Major mode specialized for viewing password files.")

(define-autoload-command 'toggle-pw-form 'PASSWORD-EDIT
  "Toggle the body of the password form under point.")

(define-autoload-command 'mouse-toggle-pw-form 'PASSWORD-EDIT
  "Toggle the body of the password form under mouse.")

;;; ****************

(define-library 'DEBIAN-CHANGELOG
  '("debian-changelog" (EDWIN DEBIAN-CHANGELOG)))

(define-autoload-major-mode 'debian-changelog 'text "Debian changelog"
  'DEBIAN-CHANGELOG
  "Major mode for editing Debian-style change logs.")

(define-autoload-command 'debian-changelog-mode 'DEBIAN-CHANGELOG
  "Enter Debian changelog mode.")

(define-variable add-log-full-name
  "Full name of user, for inclusion in ChangeLog headers.
This defaults to the value `mail-full-name'."
  #f
  string-or-false?)

(define-variable add-log-mailing-address
  "Electronic mail address of user, for inclusion in ChangeLog headers.
This defaults to the value of `user-mail-address'."
  #f
  string-or-false?)

(define-variable debian-changelog-mode-hook
  "An event distributor that is invoked when entering Debian changelog mode."
  (make-event-distributor))

;;;; DOS-specific commands

(if (memq microcode-id/operating-system '(DOS))
    (begin
      (define-library 'DOSCOM
	'("doscom" (EDWIN DOSJOB)))
      (define-autoload-command 'shell-command 'DOSCOM
	"Execute string COMMAND in inferior shell; display output, if any.")
      (define-autoload-command 'shell-command-on-region 'DOSCOM
	"Execute string COMMAND in inferior shell with region as input.")
      (define-autoload-procedure 'shell-command '(EDWIN DOSJOB)
	'DOSCOM)

      (define-library 'DOSSHELL
	'("dosshell" (EDWIN DOSJOB)))
      (define-autoload-major-mode 'pseudo-shell 'fundamental "Pseudo Shell"
	'DOSSHELL
	"Major mode for executing DOS commands.")
      (define-autoload-command 'shell 'DOSSHELL
	"Run an inferior pseudo shell, with I/O through buffer *shell*.")))
