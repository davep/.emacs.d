;;; init-packages-builtin.el -- Load and configure some builtin packages

;;; Commentary:
;;
;; init-packages-builtin.el loads and configures various packages that are
;; part of GNU Emacs itself.

;;; Code:

(require 'is-a)


;; General modes.
(use-package abbrev
  :config
  (setq abbrev-file-name (local-emacs-directory "abbrev_defs.el")))
(use-package bookmark
  :config
  (setq bookmark-default-file (local-emacs-directory "bookmarks.el")))
(use-package calendar
  :config
  (setq
   diary-file             (local-emacs-directory "diary")
   calendar-day-style     'iso
   calendar-latitude      55.9
   calendar-longitude     -3.2
   calendar-location-name "Edinburgh, Scotland"))
(use-package dired
  :config
  (setq dired-use-ls-dired nil))
(use-package dired-x
  :config
  (setq-default dired-omit-files-p t)
  (setq dired-omit-files "^\\.[^.]"))
(use-package elec-pair
  :init
  (electric-pair-mode t))
(use-package eshell
  :config
  (require 'em-dirs)
  (setq
   eshell-directory-name  (concat user-emacs-directory ".eshell/")
   eshell-prompt-regexp   "^[^#$\\n]*[#$] "
   eshell-prompt-function (lambda ()
                            (concat (user-login-name)
                             ":"
                             (abbreviate-file-name (eshell/pwd))
                             (if is-a-root-user-p "#" "$")
                             " ")))
  (require 'em-prompt)
  (set-face-foreground 'eshell-prompt "indian red")
  (set-face-background 'eshell-prompt nil))
(use-package eww
  :config
  (setq eww-bookmarks-directory (concat user-emacs-directory ".eww")))
(use-package ispell
  :config
  (setq ispell-dictionary     "british"
        ispell-highlight-face 'flyspell-incorrect)
  ;; Try and get aspell working on a Windows machine.
  (let ((aspell "C:/Program Files (x86)/Aspell/bin/"))
    (when (and is-a-win32-p (file-exists-p aspell))
      (push aspell exec-path)
      (setq ispell-program-name "aspell"))))
(use-package flyspell
  :config
  (mapc (lambda (hook)
          (add-hook hook #'flyspell-prog-mode))
        '(c-mode-hook
          c++-mode-hook
          emacs-lisp-mode-hook
          js-mode-hook
          sh-mode-hook)))
(use-package footnote
  :config
  (add-hook 'footnote-mode-hook
            (lambda ()
              (setq footnote-style 'numeric-latin
                    footnote-spaced-footnotes nil
                    footnote-section-tag "-----"
                    footnote-section-tag-regexp (regexp-quote footnote-section-tag)
                    footnote-narrow-to-footnotes-when-editing t))))
(use-package gnus
  :init
  (setq
   gnus-default-nntp-server "news.eternal-september.org"
   gnus-select-method `(nntp ,gnus-default-nntp-server)))
(use-package ibuffer
  :bind
  ([(meta f6)] . ibuffer)
  ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-expert t)
  (add-hook 'ibuffer-mode-hooks
            (lambda ()
              (ibuffer-auto-mode 1))))
(use-package message
  :config
  (add-hook 'message-mode-hook
            (lambda ()
              (setq smartsig-set-signature #'msig-set
                    smartsig-abbrev-table  'message-mode-abbrev-table)
              (smartsig-clear)
              (smartsig-add "emacs"   "~/.sigs/emacs"   "emacs" "xemacs" "elisp" "gnu" "lbdb" "uptimes" "quickurl" "smartsig" "boxquote")
              (smartsig-add "sawfish" "~/.sigs/sawfish" "sawfish" "sawmill" "librep" "rep" "gnome"))))
(use-package org
  :config
  (setq org-agenda-files
        '("~/Dropbox/Sync/Org/elisp.org"
          "~/Dropbox/Sync/Org/inbox.org")
        org-default-notes-file "~/Dropbox/Sync/Org/inbox.org"
        org-directory "~/Dropbox/Sync/Org"
        org-log-done 'time))
(use-package quickurl
  :bind
  ("C-c u" . quickurl)
  :config
  (setq quickurl-url-file (locate-user-emacs-file ".quickurls.el")))
(use-package paren
  :init
  (show-paren-mode t))
(use-package savehist
  :config
  :init
  (setq savehist-file (local-emacs-directory "history.el"))
  (savehist-mode t))
(use-package tramp
  :init
  (setq
   tramp-default-method        "ssh"
   tramp-persistency-file-name (local-emacs-directory "tramp.el")))


;; Programming modes.
(use-package cc-mode
  :config
  (setq c-basic-offset 4
        c-offsets-alist '((inline-open . 0)
                          (case-label . +)
                          (inclass . ++))
        c-default-style '((c-mode    . "BSD")
                          (c++-mode  . "BSD")
                          (java-mode . "java")
                          (awk-mode  . "awk")
                          (other     . "gnu")))
  :bind
  (:map c-mode-map   ("RET" . newline-and-indent))
  (:map c++-mode-map ("RET" . newline-and-indent)))
(use-package js
  :config
  (setq js-switch-indent-offset 4)
  :bind
  (:map js-mode-map ("RET" . newline-and-indent)))
(use-package opascal
  :bind
  (:map opascal-mode-map ("RET" . newline-and-indent)))
(use-package pascal
  :config
  (setq pascal-auto-newline t
        pascal-indent-level 2
        pascal-tab-always-indent t)
  :bind
  (:map pascal-mode-map ("RET" . newline-and-indent)))


;; Catch-all hooks, etc.
(add-hook 'text-mode-hook
          (lambda()
            (flyspell-mode 1)
            (footnote-mode 1)))
(add-hook 'org-mode-hook
          (lambda ()
            (auto-fill-mode)
            (flyspell-mode 1)))

(provide 'init-packages-builtin)

;;; init-packages-builtin.el ends here
