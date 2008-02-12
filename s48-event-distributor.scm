;;; -*- mode: scheme; scheme48-package: event-distributor -*-
;;;
;;; Port MIT Scheme's event distributor (events.scm) to Scheme48
;;;

(define-record-type* event-distributor
  (%make-event-distributor events (lock) (receivers))
  ())

(define (make-event-distributor)
  (%make-event-distributor (make-queue) #f '()))

(define (make-receiver-modifier keyword)
  (lambda (event-distributor receiver)
    (if (not (event-distributor? event-distributor))
	(error "Not an event distributor" event-distributor))
    (enqueue! (event-distributor-events event-distributor)
	      (cons keyword receiver))
    (process-events! event-distributor)))

(define (event-distributor/invoke! event-distributor . arguments)
  (enqueue! (event-distributor-events event-distributor)
	    (cons 'INVOKE-RECEIVERS arguments))
  (process-events! event-distributor))

(define add-event-receiver!    (make-receiver-modifier 'ADD-RECEIVER))
(define remove-event-receiver! (make-receiver-modifier 'REMOVE-RECEIVER))

(define (process-events! event-distributor)
  (let ((old-lock unspecific))
    (dynamic-wind
     (lambda ()
       (let ((lock (event-distributor-lock event-distributor)))
	 (set-event-distributor-lock! event-distributor #t)
	 (set! old-lock lock)
	 unspecific))
     (lambda ()
       (if (not old-lock)
	   (queue-map! (event-distributor-events event-distributor)
	     (lambda (event)
	       (case (car event)
		 ((INVOKE-RECEIVERS)
		  (do ((receivers
			(event-distributor-receivers event-distributor)
			(cdr receivers)))
		      ((null? receivers))
		    (apply (car receivers) (cdr event))))
		 ((ADD-RECEIVER)
		  (let ((receiver (cdr event))
			(receivers
			 (event-distributor-receivers event-distributor)))
		    (if (not (memv receiver receivers))
			(set-event-distributor-receivers!
			 event-distributor
			 (append! receivers (list receiver))))))
		 ((REMOVE-RECEIVER)
		  (set-event-distributor-receivers!
		   event-distributor
		   (delete! (cdr event)
                            (event-distributor-receivers event-distributor)
                            eqv?)))
		 (else
		  (error "Illegal event" event)))))))
     (lambda ()
       (set-event-distributor-lock! event-distributor old-lock)))))

(define (queue-map! queue procedure)
  (let ((empty (list 'EMPTY)))
    (let loop ()
      (let ((item
             (without-interrupts
              (lambda ()
                (if (queue-empty? queue)
                    empty
                    (dequeue! queue))))))
        (if (not (eq? item empty))
            (begin
              (procedure item)
              (loop)))))))