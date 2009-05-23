;;; -*- Mode: Scheme; scheme48-package: tests:buffer -*-
;;;
;;; Buffer tests
;;;

;;;; Running in Scheme48:
;;;; Run in edwin48/tests/
;;;; ,exec ,load load.scm
;;;; ,in edwin:buffer
;;;; ,open srfi-78
;;;; ,load test-buffer.scm

(define (buff-string buff)
  (let ((cur-group (buffer-group buff)))
    (group-extract-string cur-group
                          (group-start-index cur-group)
                          (group-end-index cur-group))))

;;; Make a mode for the buffer
(define-major-mode buffer-editor #f "Edit Buffers" (lambda (buffer) buffer unspecific))
(check (major-mode? (name->mode 'buffer-editor)) => #t)

;;; Make a buffer
(define foo-buff (make-buffer "test-case" (name->mode 'buffer-editor) "/tmp/foo.txt"))
(check (buffer? foo-buff) => #t)

;;; Insert us some chars
(group-insert-char! (buffer-group foo-buff) 0 #\H)
(group-insert-char! (buffer-group foo-buff) 1 #\e)
(group-insert-char! (buffer-group foo-buff) 2 #\l)
(group-insert-char! (buffer-group foo-buff) 3 #\l)
(group-insert-char! (buffer-group foo-buff) 4 #\o)

;;; Add a string and a touch of salt
(group-insert-string! (buffer-group foo-buff) 5 " World!")

;;; Take it out of the oven ...
(check (string=? (buff-string foo-buff) "Hello World!") => #t)

;;; Make up for the fact that we don't have the giant 'state soup'
(define (current-point) (make-mark (buffer-group foo-buff) 0))

;;; Undo!
(undo-one-step foo-buff (undo-start foo-buff))

;;; And we're back
(check (string=? (buff-string foo-buff) "") => #t)

;;; Do some more talkie
(group-insert-string! (buffer-group foo-buff) 0 "Watson Come Here, I Want You")

;;; Did he come over?
(check (string=? (buff-string foo-buff) "Watson Come Here, I Want You") => #t)