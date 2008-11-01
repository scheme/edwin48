;;; -*- mode: scheme; scheme48-package: edwin:input-event -*-

(define-record-type input-event
  (really-make-input-event type operator operands)
  input-event?
  (type     input-event/type)
  (operator input-event/operator)
  (operands input-event/operands))

(define (make-input-event type operator . operands)
  (really-make-input-event type operator operands))

(define (apply-input-event input-event)
  (if (not (input-event? input-event))
      (error:wrong-type-argument input-event "input event" apply-input-event))
  (apply (input-event/operator input-event)
	 (input-event/operands input-event)))