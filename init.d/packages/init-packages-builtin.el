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


;; General modes.
(use-package tramp
  :custom
  (tramp-default-method        "ssh")
  (tramp-persistency-file-name (local-emacs-directory "tramp.el")))
(use-package whitespace
  :custom
  (whitespace-style '(face trailing empty tabs tab-mark))
  :config
  (add-hook
   'after-change-major-mode-hook
   (lambda ()
     (whitespace-mode (if (buffer-file-name) 1 -1)))))


;; Programming modes.
(use-package cc-mode
  :custom
  (c-basic-offset 4)
  (c-offsets-alist '((inline-open . 0)
                     (case-label . +)))
  (c-default-style '((c-mode    . "BSD")
                     (c++-mode  . "BSD")
                     (java-mode . "java")
                     (awk-mode  . "awk")
                     (other     . "gnu")))
  :bind
  (:map c-mode-map   ("RET" . newline-and-indent))
  (:map c++-mode-map ("RET" . newline-and-indent)))
(use-package js2-mode
  :mode "\\.js$"
  :custom
  (js-switch-indent-offset 4))
(use-package python
  :custom
  (python-indent-guess-indent-offset nil)
  (python-shell-interpreter "python3")
  (python-fill-docstring-style 'pep-257-nn))


;; Catch-all hooks, etc.
(add-hook 'text-mode-hook
          (lambda ()
            (auto-fill-mode 1)))
(add-hook 'prog-mode-hook
          (lambda ()
            ;; Set up for auto-filling comments.
            (setq-local comment-auto-fill-only-comments t)
            (auto-fill-mode 1)))

(provide 'init-packages-builtin)

;;; init-packages-builtin.el ends here
