(user)

(translate "=base" "..")

;;; EDWIN-EXPORT
(config '(load "=base/config-macros.scm"))

(config '(load "=base/terminfo/interfaces.scm"
               "=base/terminfo/scsh-packages.scm"))

(config '(load "=base/soosy/interfaces.scm"
               "=base/soosy/packages.scm"))

(config '(load "=base/edwin48/srfi-packages.scm"
               "=base/edwin48/scsh/packages.scm"
               "=base/edwin48/interfaces.scm"))

(config '(load "packages.scm"))
