(user)

(translate "=base" "..")

(config '(load "=base/config-macros.scm"))

(config '(load "=base/edwin48/srfi-packages.scm"
               "=base/edwin48/scsh/packages.scm"))

(config '(load "=base/terminfo/interfaces.scm"
               "=base/terminfo/scsh-packages.scm"))

(config '(load "=base/soosy/interfaces.scm"
               "=base/soosy/packages.scm"))

(config '(load "=base/pantene/edwin-interfaces.scm"
               "=base/pantene/edwin-packages.scm"))

(config '(load "=base/edwin48/scratch-interfaces.scm"
               "=base/edwin48/scratch-packages.scm"))

(config '(load "packages.scm"))