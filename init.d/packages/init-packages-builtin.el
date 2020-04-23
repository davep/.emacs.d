;;; init-packages-builtin.el -- Load and configure some builtin packages

;;; Commentary:
;;
;; init-packages-builtin.el loads and configures various packages that are
;; part of GNU Emacs itself. Note that being in Elpa is considered to be
;; "built in."

;;; Code:

(require 'is-a)
(require 'solar)
(require 'init-local)


;; Catch-all hooks, etc.
(add-hook 'text-mode-hook
          (lambda ()
            (auto-fill-mode 1)))

(provide 'init-packages-builtin)

;;; init-packages-builtin.el ends here
