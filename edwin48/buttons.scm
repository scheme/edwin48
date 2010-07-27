;;;; Buttons

(define-record-type button
  (%%make-button number bits down? symbol)
  button?
  (number button-number)
  (bits button-bits)
  (down? button-down?)
  (symbol button-symbol))

(define* (make-down-button number (bits 0))
  (%make-button number bits #t 'make-down-button))

(define* (make-up-button number (bits 0))
  (%make-button number bits #f 'make-up-button))

(define (%make-button number bits down? caller)
  (let ((name
         (symbol (bucky-bits->prefix bits)
                 'button-
                 number
                 (if down? '-down '-up))))
    (hash-table/intern! buttons-table name
                        (lambda ()
                          (%%make-button number bits down? name)))))

(define buttons-table
  (make-hash-table eq?))

(define (down-button? object)
  (and (button? object)
       (button-down? object)))

(define (up-button? object)
  (and (button? object)
       (not (button-down? object))))

(define (button-name button)
  (symbol-name (button-symbol button)))

(define-record-type button-event
  (make-button-event window x y)
  button-event?
  (window button-event/window)
  (x      button-event/x)
  (y      button-event/y))

(define (current-button-event)
  (or (editor-button-event current-editor)
      ;; Create a "dummy" event at point.
      (let ((window (current-window)))
	(let ((coordinates (window-point-coordinates window)))
	  (make-button-event window
			     (car coordinates)
			     (cdr coordinates))))))

(define (with-current-button-event button-event thunk)
  (let ((old-button-event unspecific))
    (dynamic-wind
     (lambda ()
       (set! old-button-event (editor-button-event current-editor))
       (set-editor-button-event! current-editor button-event)
       (set! button-event #f)
       unspecific)
     thunk
     (lambda ()
       (set-editor-button-event! current-editor old-button-event)))))

(define button1-down (make-down-button 0))
(define button2-down (make-down-button 1))
(define button3-down (make-down-button 2))
(define button4-down (make-down-button 3))
(define button5-down (make-down-button 4))
(define button1-up (make-up-button 0))
(define button2-up (make-up-button 1))
(define button3-up (make-up-button 2))
(define button4-up (make-up-button 3))
(define button5-up (make-up-button 4))