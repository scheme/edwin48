(define-enumerated-type key-modifier :key-modifier
  key-modifier?
  all-key-modifiers
  key-modifier-name
  key-modifier-index
  (shift ctrl meta alt))

(define-enumerated-type named-keystroke :named-keystroke
  named-keystroke?
  all-named-keystrokes
  named-keystroke-name
  named-keystroke-value
  (up down left right backspace))

(define-enum-set-type key-modifier-set :key-modifier-set
  key-modifier-set? make-key-modifier-set
  key-modifier key-modifier? all-key-modifiers key-modifier-index)

(define all-named-keys
  '((#\backspace . backspace)
    (#\del       . delete)
    (#\escape    . escape)
    (#\newline   . newline)
    (#\return    . return)
    (#\tab       . tab)
    (#\vtab      . vtab)))

(define (key-modifier-set=? set1 set2)   (enum-set=? set1 set2))
(define (key-modifier-set->list set)     (enum-set->list set))
(define (key-modifier-set-union set key) (enum-set-union set key))