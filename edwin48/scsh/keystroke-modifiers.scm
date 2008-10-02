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
