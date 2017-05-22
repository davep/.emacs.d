;;; init-packages-melpa.el -- Load and configure packages from public repos.

;;; Commentary:
;;
;; init-packages-melpa.el loads and configures personal and third party
;; packages that live in public package repositories such as elpa and melpa.

;;; Code:


;; My own packages that are in melpa.
(use-package binclock
  :ensure t)
(use-package boxquote
  :ensure t
  :bind
  ("C-c i"     . boxquote-insert-file)
  ("C-c M-w"   . boxquote-kill-ring-save)
  ("C-c y"     . boxquote-yank)
  ("C-c b"     . boxquote-region)
  ("C-c C-b"   . boxquote-title)
  ("C-c C-h f" . boxquote-describe-function)
  ("C-c C-h v" . boxquote-describe-variable)
  ("C-c C-h k" . boxquote-describe-key)
  ("C-c !"     . boxquote-shell-command)
  ("C-c C-h w" . boxquote-where-is))
(use-package obfusurl
  :ensure t)
(use-package protocols
  :ensure t)
(use-package services
  :ensure t)
(use-package thinks
  :ensure t
  :bind
  ("C-c C-t" . thinks-maybe-region))
(use-package uptimes
  :ensure t
  :bind
  ([f11]        . uptimes-current)
  ([(meta f11)] . uptimes))


;; Third-party packages from elpa/melpa
(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-save-mode)
  (auto-compile-on-load-mode))
(use-package browse-kill-ring
  :ensure t
  :bind
  ("C-c k" . browse-kill-ring))
(use-package company
  :ensure t
  :diminish "CM"
  :bind
  ("s-SPC" . company-complete)
  :init
  (add-hook 'after-init-hook #'global-company-mode))
(use-package csharp-mode
  :ensure t)
(use-package dictionary
  :ensure t)
(use-package emojify
  :if is-a-unix-window-p
  :config
  (global-emojify-mode)
  :ensure t)
(use-package google-contacts
  :ensure t)
(use-package google-maps
  :ensure t)
(use-package hyde
  :ensure t
  :commands hyde)
(use-package magit
  :ensure t
  :bind
  ("C-c g" . magit-status))
(use-package markdown-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (auto-fill-mode)
              (flyspell-mode 1))))
(use-package package-lint
  :ensure t)
(use-package page-break-lines
  :ensure t
  :diminish 'page-break-lines-mode
  :init
  (global-page-break-lines-mode))
(use-package paren-face
  :ensure t
  :demand
  :config
  (global-paren-face-mode t))
(use-package powerline
  :ensure t
  :config
  (setq ns-use-srgb-colorspace nil)
  (powerline-default-theme))
(use-package powershell
  :ensure t)
(use-package restclient
  :ensure t)
(use-package sass-mode
  :ensure t
  :mode "\\.scss$")
(use-package slime
  :ensure t
  :config
  (add-hook 'slime-inferior-process-start-hook
            (lambda ()
              (require 'slime-fancy))))
(use-package w3m
  :if is-a-macOS-p
  :ensure t)
(use-package highlight-chars
  :ensure t
  :demand
  :commands hc-highlight-trailing-whitespace
  :config
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (when (buffer-file-name)
                (hc-highlight-trailing-whitespace)))))
(use-package multiple-cursors
  :ensure t
  :bind
  ("C-."     . mc/mark-next-like-this)
  ("C-,"     . mc/mark-previous-like-this)
  ("C->"     . mc/mark-all-like-this)
  ("C-c ."   . mc/mark-all-like-this-dwim)
  ("C-c C-." . mc/edit-lines))

(provide 'init-packages-melpa)

;;; init-packages-melpa.el ends here
