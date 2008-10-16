(define (start-printer)
  (receive (halt-update? peek-no-hang peek read)
      (get-console-input-operations)
    (with-current-input-terminal-mode 'raw
      (let ((term (setup-terminal)))
        (tputs (keypad-xmit term))
        (let loop ((k (read)))
          (if (key=? k (kbd (ctrl #\q)))
              (begin (display "bye")
                     (newline)
                     (tputs (keypad-local term)))
              (begin (display (key->name k))
                     (newline)
                     (loop (read)))))))))


