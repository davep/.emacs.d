(require 'is-a)

;; Only bother with this if we're on macOS.
(when is-a-macOS-p

  ;; Tool for getting a Mac's "date of birth".
  (use-package macdob :ensure t)

  ;; Tool for getting some information about a Mac.
  (use-package macinfo :ensure t))
