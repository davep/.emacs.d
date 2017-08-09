;;; init-packages-melpa.el -- Load and configure packages from public repos.

;;; Commentary:
;;
;; init-packages-melpa.el loads and configures personal and third party
;; packages that live in public package repositories such as elpa and melpa.

;;; Code:

(require 'is-a)


;; My own packages that are in melpa.
(use-package binclock
  :ensure t
  :bind
  ("C-<f11>" . binclock))
(use-package boxquote
  :ensure t
  :bind
  ("<f12> b i"   . boxquote-insert-file)
  ("<f12> b M-w" . boxquote-kill-ring-save)
  ("<f12> b y"   . boxquote-yank)
  ("<f12> b b"   . boxquote-region)
  ("<f12> b t"   . boxquote-title)
  ("<f12> b h f" . boxquote-describe-function)
  ("<f12> b h v" . boxquote-describe-variable)
  ("<f12> b h k" . boxquote-describe-key)
  ("<f12> b h w" . boxquote-where-is)
  ("<f12> b !"   . boxquote-shell-command))
(use-package cheat-sh
  :ensure t
  :bind
  ("<f12> / /" . cheat-sh-maybe-region)
  ("<f12> / l" . cheat-sh-list)
  ("<f12> / ?" . cheat-sh-help)
  ("<f12> / s" . cheat-sh-search)
  ("<f12> / t" . cheat-sh-search-topic))
(use-package dad-joke
  :ensure t
  :bind
  ("C-c C-d" . dad-joke))
(use-package eg
  :ensure t)
(use-package numbers
  :ensure t
  :bind
  ("<f12> n m"   . numbers-math)
  ("<f12> n t"   . numbers-trivia)
  ("<f12> n C-m" . numbers-random-math)
  ("<f12> n C-t" . numbers-random-trivia)
  ("<f12> n n"   . numbers-random))
(use-package obfusurl
  :ensure t)
(use-package protocols
  :ensure t)
(use-package services
  :ensure t)
(use-package slstats
  :ensure t)
(use-package thinks
  :ensure t
  :bind
  ("C-c t t" . thinks-maybe-region)
  ("C-c t y" . thinks-yank))
(use-package uptimes
  :ensure t
  :init
  (setq uptimes-database (local-emacs-directory "uptimes.el"))
  :bind
  ("<f11>"   . uptimes-current)
  ("M-<f11>" . uptimes))


;; Third-party packages from elpa/melpa
(use-package beacon
  :if (display-graphic-p)
  :ensure t
  :diminish 'beacon-mode
  :config (beacon-mode 1))
(use-package browse-kill-ring
  :ensure t
  :bind
  ("<f12> k" . browse-kill-ring))
(use-package company
  :ensure t
  :diminish "CM"
  :bind
  ("s-SPC" . company-complete)
  :init
  (add-hook 'after-init-hook #'global-company-mode))
(use-package dictionary
  :ensure t)
(use-package emojify
  :if is-a-unix-window-p
  :config
  (setq emojify-emojis-dir (local-emacs-directory "emojis"))
  (global-emojify-mode)
  :ensure t)
(use-package google-contacts
  :ensure t)
(use-package google-maps
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
(use-package hyde
  :ensure t
  :commands hyde)
(use-package magit
  :if is-a-unix-p
  :ensure t
  :bind
  ("C-c g s" . magit-status)
  ("C-c g l" . magit-log-all))
(use-package markdown-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              ;; The "C-c <arrow>" bindings in markdown-mode clash with my
              ;; window creation bindings. Remove them (there are
              ;; alternatives anyway).
              (define-key markdown-mode-map (kbd "C-c <right>") nil)
              (define-key markdown-mode-map (kbd "C-c <left>")  nil)
              (define-key markdown-mode-map (kbd "C-c <up>")    nil)
              (define-key markdown-mode-map (kbd "C-c <down>")  nil)
              (auto-fill-mode)
              (flyspell-mode 1))))
(use-package minimap
  :ensure t
  :bind
  ("C-c m" . minimap-mode)
  :config
  (setq minimap-window-location 'right))
(use-package multiple-cursors
  :ensure t
  :bind
  ("C-."     . mc/mark-next-like-this)
  ("C-,"     . mc/mark-previous-like-this)
  ("C->"     . mc/mark-all-like-this)
  ("C-c ."   . mc/mark-all-like-this-dwim)
  ("C-c C-." . mc/edit-lines))
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
  (global-paren-face-mode t)
  (set-face-foreground 'parenthesis "gray63"))
(use-package powerline
  :ensure t
  :config
  (when is-a-macOS-p
    (set (intern "ns-use-srgb-colorspace") nil))
  (set-face-background 'powerline-active1 "grey95")
  (set-face-background 'powerline-active2 "grey85")
  (set-face-background 'powerline-inactive1 "grey30")
  (set-face-background 'powerline-inactive2 "grey20")
  (powerline-default-theme))
(use-package powershell
  :ensure t)
(use-package restclient
  :ensure t
  :config
  (defun restclient-scratch ()
    "Create a scratch buffer for use with `resctclient-mode'."
    (interactive)
    (switch-to-buffer "*restclient*")
    (when (string= (buffer-string) "")
      (insert "# -*- restclient -*-\n\n"))
    (restclient-mode)))
(use-package rust-mode
  :ensure t)
(use-package sass-mode
  :ensure t
  :mode "\\.scss$"
  :config
  (setq sass-indent-offset 4))
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl")
  (add-hook 'slime-inferior-process-start-hook
            (lambda ()
              (require 'slime-fancy))))
(use-package suggest
  :ensure t)
(use-package wttrin
  :ensure t
  :config
  (setq wttrin-default-cities '("Edinburgh")
        wttrin-default-accept-language '("Accept-Language" . "en-GB"))
  :bind
  ("<f12> x" . wttrin))

(provide 'init-packages-melpa)

;;; init-packages-melpa.el ends here
