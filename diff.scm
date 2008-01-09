;;; -*-Scheme-*-
;;;
;;; $Id: diff.scm,v 1.2 2006/06/16 18:33:21 riastradh Exp $
;;;
;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.
;;;
;;;

;;;; Unix diff(1) wrapper

;;; There ought to be an accompanying diff major mode, with fancy
;;; bells & whistles for converting between unified and context diffs,
;;; like in GNU Emacs.  Some day.


(define-variable diff-program
  "The name of the diff program."
  "diff"
  string?)

(define-variable diff-switches
  "A list of strings specifying switches to pass to the diff command."
  '("-c")
  (lambda (obj)
    (list-of-type? obj string?)))

(define-command diff
  "Display differences between files."
  "fOld file\nfNew file"                ;Icky interactive specification
  (lambda (old-filename new-filename)
    (select-buffer (diff-to-buffer old-filename new-filename))))

(define-command diff-backup
  "Display differences between a file and its latest backup."
  (lambda ()
    (list (prompt-for-existing-file
           "Diff with backup"
           (cond ((buffer-pathname (selected-buffer))
                  => list)
                 (else #f)))))
  (lambda (filename)
    (select-buffer
     (diff-to-buffer (or (os/newest-backup filename)
                         (editor-error "No known backup for file: "
                                       filename))
                     filename))))

(define-command diff-buffer-with-file
  "Display differences between a buffer and its original file."
  "bBuffer"
  (lambda (buffer-name)
    (let* ((buffer (find-buffer buffer-name #t))
           (pathname (buffer-pathname buffer)))
      (if (not (and pathname (file-exists? pathname)))
          (editor-error "Buffer has no associated file."))
      (diff-buffer buffer pathname
                   (lambda (temporary-pathname)
                     ;; PATHNAME is the buffer's usual storage on the
                     ;; disk; TEMPORARY-PATHNAME contains the buffer's
                     ;; current contents.
                     (diff-to-buffer pathname temporary-pathname))))))

(define-command diff-auto-save
  "Display differences from a buffer to its auto-save file."
  "bBuffer"
  (lambda (buffer-name)
    (let* ((buffer (find-buffer buffer-name #t))
           (pathname (buffer-pathname buffer))
           (auto-save (os/auto-save-pathname pathname buffer)))
      (if (not (file-exists? auto-save))
          (editor-error "Buffer has no auto-save file."))
      (diff-buffer buffer pathname
                   (lambda (temporary-pathname)
                     ;; PATHNAME is irrelevant; TEMPORARY-PATHNAME
                     ;; contains the buffer's current contents; and
                     ;; AUTO-SAVE contains the auto-saved contents.
                     (diff-to-buffer temporary-pathname auto-save))))))

(define (diff-buffer buffer pathname receiver)
  (select-buffer
   (if (buffer-modified? buffer)
       (call-with-temporary-file-pathname
        (lambda (temporary-pathname)
          (write-region (buffer-region buffer)
                        temporary-pathname
                        #f              ;No message
                        #f)             ;No line ending translation
          (receiver temporary-pathname)))
       (receiver pathname))))

(define (diff-to-buffer old-filename new-filename #!optional buffer)
  (let ((buffer (diff-to-buffer-argument buffer)))
    (buffer-reset! buffer)
    (set-buffer-major-mode! buffer (ref-mode-object fundamental))
    (buffer-put! buffer 'REVERT-BUFFER-METHOD
                 (lambda (buffer do-not-auto-save? do-not-confirm?)
                   do-not-auto-save? do-not-confirm? ;ignore
                   (diff-to-buffer old-filename new-filename buffer)))
    (execute-diff old-filename new-filename buffer)
    (set-buffer-point! buffer (buffer-start buffer))
    (set-buffer-read-only! buffer)
    buffer))

(define (diff-to-buffer-argument buffer)
  (cond ((default-object? buffer) (find-or-create-buffer "*Diff*"))
        ((string?         buffer) (find-or-create-buffer buffer))
        ((buffer?         buffer) buffer)
        (else (error:wrong-type-argument buffer "buffer or string"
                                         'DIFF-TO-BUFFER))))

(define (execute-diff old-filename new-filename buffer)
  (let ((defaults (buffer-default-directory buffer)))
    (let ((command
           (make-diff-command old-filename new-filename defaults))
          (output-mark
           (mark-left-inserting-copy (buffer-start buffer))))
      (insert-string (decorated-string-append "" " " "" command)
                     output-mark)
      (insert-newline output-mark)
      (let ((result (run-diff-process command defaults output-mark)))
        (insert-diff-exit-remarks result output-mark)))))

(define (make-diff-command old-filename new-filename defaults)
  (cons (ref-variable diff-program)
        (append (ref-variable diff-switches)
                (list (enough-namestring old-filename defaults)
                      (enough-namestring new-filename defaults)))))

(define (run-diff-process command defaults output-mark)
  (apply run-synchronous-process
         #f             ; no input region
         output-mark
         defaults
         #f             ; not a pty
         command))

(define (insert-diff-exit-remarks result output-mark)
  (insert-newline output-mark)
  (insert-string "Diff finished" output-mark)
  (if (eqv? result 0) (insert-string " (no differences)" output-mark))
  (insert-char #\. output-mark)
  (insert-newline output-mark)
  (insert-chars #\space 2 output-mark)
  (insert-string (universal-time->global-ctime-string
                  (get-universal-time))
                 output-mark)
  (insert-newline output-mark))
