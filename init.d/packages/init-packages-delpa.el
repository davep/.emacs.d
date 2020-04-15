;;; init-packages-delpa.el -- Load and configure my personal packages.

;;; Commentary:
;;
;; init-packages-delpa.el loads and configures my personal packages that I
;; keep in my own package repository (the sorts of packages that would not
;; make sense in a public repository).

;;; Code:

(use-package qrencode
  :ensure t)
(use-package rate-sx
  :ensure t
  :bind
  ("<f12> c" . rate-sx))
(use-package requote
  :ensure t
  :bind
  ("<f12> q" . requote))
(use-package setup-compile
  :ensure t
  :commands setup-compile
  :hook (c-mode c++mode emacs-lisp-mode))
(use-package smartsig
  :ensure t)
(use-package unbind
  :ensure t)
(use-package winsplit
  :ensure t
  :bind*
  ("C-c <right>"   . winsplit-right)
  ("C-c <left>"    . winsplit-left)
  ("C-c <up>"      . winsplit-above)
  ("C-c <down>"    . winsplit-below)
  ("C-c C-<right>" . winsplit-right-load)
  ("C-c C-<left>"  . winsplit-left-load)
  ("C-c C-<up>"    . winsplit-above-load)
  ("C-c C-<down>"  . winsplit-below-load))

(provide 'init-packages-delpa)

;;; init-packages-delpa.el ends here
