;;; init-packages-melpa.el -- Load and configure packages from public repos.

;;; Commentary:
;;
;; init-packages-melpa.el loads and configures personal and third party
;; packages that live in public package repositories such as elpa and melpa.

;;; Code:

(require 'is-a)


;; My own packages that are in melpa.
(use-package binclock
  :ensure t)
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
  ("<f12> d" . dad-joke))
(use-package eg
  :ensure t
  :bind
  ("<f12> e" . eg))
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
(use-package quiz
  :ensure t)
(use-package services
  :ensure t)
(use-package slstats
  :ensure t)
(use-package thinks
  :ensure t
  :bind
  ("<f12> t t" . thinks-maybe-region)
  ("<f12> t y" . thinks-yank))
(use-package uptimes
  :ensure t
  :custom
  (uptimes-database (local-emacs-directory "uptimes.el"))
  :bind
  ("<f11>"       . uptimes-current)
  ("<f12> <f11>" . uptimes))


;; Third-party packages from elpa/melpa
(use-package apache-mode
  :ensure t)
(use-package beacon
  :if (display-graphic-p)
  :ensure t
  :diminish
  :commands beacon-mode
  :config (beacon-mode 1))
(use-package browse-kill-ring
  :ensure t
  :bind
  ("C-x y" . browse-kill-ring))
(use-package company
  :ensure t
  :diminish
  :bind
  ("s-SPC" . company-complete)
  :commands global-company-mode
  :hook (after-init . global-company-mode))
(use-package counsel
  :ensure t
  :after ivy
  :diminish
  :commands counsel-mode
  :custom
  (counsel-find-file-ignore-regexp
   (rx
    (or
     ;; Ignore hidden files.
     (group bol ".")
     ;; Ignore compiled files.
     (group "." (or "pyc" "elc") eol))))
  :init
  (counsel-mode 1)
  :bind
  ("<f12> f g" . counsel-rg)
  ("<f12> f l" . counsel-locate)
  ("C-c C-f" . counsel-recentf)
  ("<f12> f r" . counsel-recentf)
  ("<f12> f M-x" . counsel-command-history)
  ("<f12> f c w" . counsel-colors-web)
  ("<f12> f c e" . counsel-colors-emacs))
(use-package dictionary
  :ensure t)
(use-package dockerfile-mode
  :ensure t)
(when is-a-unix-p
  (use-package emojify
    :if is-a-unix-window-p
    :commands global-emojify-mode
    :config
    (setq emojify-emojis-dir (local-emacs-directory "emojis"))
    (add-to-list 'emojify-inhibit-major-modes 'restclient-mode)
    :init
    (global-emojify-mode)
    :ensure t))
(use-package eshell-git-prompt
  :ensure t
  :custom-face
  (eshell-git-prompt-powerline-dir-face
   ((t :background "grey80")))
  (eshell-git-prompt-powerline-clean-face
   ((t :background "green")))
  (eshell-git-prompt-powerline-not-clean-face
   ((t :background "yellow")))
  :commands eshell-git-prompt-use-theme
  :init
  (eshell-git-prompt-use-theme 'powerline))
(use-package go-mode
  :ensure t)
(use-package haskell-mode
  :ensure t
  :bind
  ("C-c C-c" . haskell-process-load-file))
(use-package highlight-chars
  :ensure t
  :demand
  :commands hc-highlight-trailing-whitespace
  :config
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (when (buffer-file-name)
                (hc-highlight-trailing-whitespace)))))
(use-package highlight-indent-guides
  :ensure t
  :diminish
  :commands highlight-indent-guides-mode
  :custom
  (highlight-indent-guides-method 'character)
  :hook (prog-mode . highlight-indent-guides-mode))
(use-package hyde
  :ensure t
  :commands hyde)
(use-package hy-mode
  :ensure t)
(use-package ivy
  :ensure t
  :diminish
  :commands
  ivy-mode
  ivy-resume
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "[%d/%d] ")
  (ivy-height 15)
  :init
  (ivy-mode 1)
  :bind*
  ("C-c C-r" . ivy-resume))
(use-package ivy-hydra
  :ensure t
  :after ivy)
(use-package ivy-rich
  :ensure t
  :after ivy
  :commands
  ivy-rich-mode
  ivy-format-function-line
  :init
  (ivy-rich-mode 1)
  (setq ivy-format-function #'ivy-format-function-line))
(use-package json-mode
  :ensure t)
(use-package lorem-ipsum
  :ensure t
  :bind
  ("<f12> l p" . lorem-ipsum-insert-paragraphs)
  ("<f12> l s" . lorem-ipsum-insert-sentences)
  ("<f12> l l" . lorem-ipsum-insert-list))
(use-package itail
  :ensure t)
(when is-a-unix-p
  (use-package magit
    :ensure t
    :commands magit-after-save-refresh-status
    :config
    (add-hook 'after-save-hook #'magit-after-save-refresh-status)
    (add-hook 'git-commit-setup-hook (lambda() (flyspell-mode 1)))
    :bind
    ("<f12> g s"   . magit-status)
    ("<f12> g b"   . magit-blame)
    ("<f12> g l a" . magit-log-all)
    ("<f12> g l f" . magit-log-buffer-file))
  (use-package forge
    :ensure t
    :custom
    (forge-database-file (local-emacs-directory "forge-database.sqlite"))
    :config
    (push (list "gitlab.synpromics.com"
                "gitlab.synpromics.com/api/v4"
                "gitlab.synpromics.com"
                'forge-gitlab-repository)
          forge-alist))
  (use-package transient
    :ensure t
    :custom
    (transient-history-file (local-emacs-directory "transient-history.el"))))
(use-package markdown-mode
  :ensure t
  :config
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc")))
(use-package multiple-cursors
  :ensure t
  :bind
  ("C-."     . mc/mark-next-like-this)
  ("C-,"     . mc/mark-previous-like-this)
  ("C->"     . mc/mark-all-like-this)
  ("C-c ."   . mc/mark-all-like-this-dwim)
  ("C-c C-." . mc/edit-lines))
(use-package neotree
  :ensure t
  :config
  (add-to-list 'neo-hidden-regexp-list "__pycache__")
  (add-to-list 'neo-hidden-regexp-list ".*\\.egg-info")
  :bind
  ("<f8>"    . neotree)
  ("C-<tab>" . neotree)
  ("C-<f8>"  . neotree-dir)
  ("s-<f8>"  . neotree-dir))
(use-package package-lint
  :ensure t)
(use-package page-break-lines
  :ensure t
  :diminish
  :commands global-page-break-lines-mode
  :init
  (global-page-break-lines-mode))
(use-package paren-face
  :ensure t
  :demand
  :commands global-paren-face-mode
  :config
  (global-paren-face-mode t)
  (set-face-foreground 'parenthesis "gray78"))
(use-package powerline
  :ensure t
  :config
  (when is-a-macOS-p
    (set (intern "ns-use-srgb-colorspace") nil))
  (set-face-background 'powerline-active1 "grey95")
  (set-face-foreground 'powerline-active1 "grey50")
  (set-face-background 'powerline-active2 "grey85")
  (set-face-foreground 'powerline-active2 "grey50")
  (set-face-background 'powerline-inactive1 "grey30")
  (set-face-background 'powerline-inactive2 "grey20")
  (powerline-default-theme))
(use-package powershell
  :ensure t)
(use-package restclient
  :ensure t
  :commands restclient-mode
  :init
  (defun restclient-scratch ()
    "Create a scratch buffer for use with `resctclient-mode'."
    (interactive)
    (switch-to-buffer "*restclient*")
    (when (string= (buffer-string) "")
      (insert "# -*- restclient -*-\n\n"))
    (restclient-mode)))
(use-package rjsx-mode
  :ensure t
  :bind*
  ("<" . self-insert-command))
(use-package rust-mode
  :ensure t)
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl")
  (add-hook 'slime-inferior-process-start-hook
            (lambda ()
              (require 'slime-fancy))))
(use-package string-inflection
  :ensure t
  :bind
  ("<f12> s" . string-inflection-all-cycle))
(use-package swiper
  :ensure t
  :after ivy
  :bind
  ("C-s" . swiper))
(use-package switch-window
  :bind
  ("C-x o" . switch-window)
  :ensure t)
(use-package wc-mode
  :ensure t)
(use-package web-mode
  :ensure t
  :mode (rx ".html" eol)
  :commands web-mode-set-engine
  :config
  (setq-default web-mode-markup-indent-offset 2)
  (setq web-mode-enable-engine-detection t))
(use-package wttrin
  :ensure t
  :custom
  (wttrin-default-cities '("Edinburgh"))
  (wttrin-default-accept-language '("Accept-Language" . "en-GB"))
  :bind
  ("<f12> x x" . wttrin))
(use-package yaml-mode
  :ensure t)

(provide 'init-packages-melpa)

;;; init-packages-melpa.el ends here
