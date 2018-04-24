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
  (setq bookmark-default-file (local-emacs-directory "bookmarks.el"))
  :bind
  ("<f12> m b" . bookmark-set)
  ("<f12> m l" . bookmark-bmenu-list)
  ("<f12> m g" . bookmark-jump))
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
  (setq dired-omit-files "^\\.[^.]\\|__pycache__"))
(use-package elec-pair
  :commands electric-pair-mode
  :init
  (electric-pair-mode t))
(use-package eshell
  :config
  (require 'em-dirs)
  (setq
   eshell-directory-name  (locate-user-emacs-file ".eshell/")
   eshell-prompt-regexp   "^[^#\\$]*[#\\$] "
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
  (setq eww-bookmarks-directory (locate-user-emacs-file ".eww")))
(use-package gamegrid
  :config
  (setq gamegrid-user-score-file-directory (local-emacs-directory "games/")))
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
  :commands
  flyspell-mode
  flyspell-prog-mode
  :init
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))
(use-package footnote
  :commands footnote-mode
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
  ("M-<f6>" . ibuffer)
  ("C-x C-b" . ibuffer)
  :commands
  ibuffer-auto-mode
  ibuffer-switch-to-saved-filter-groups
  ibuffer-do-sort-by-alphabetic
  :init
  (setq ibuffer-expert t
        ibuffer-show-empty-filter-groups nil
        ibuffer-saved-filter-groups
        '(("davep"
           ("Magit"    (name . "\*magit"))
           ("Org"      (mode . org-mode))
           (".emacs.d" (filename . "/.emacs.d/"))
           ("elisp"    (mode . emacs-lisp-mode))
           ("Help"     (or (name . "\*Help\*")
                           (name . "\*Apropos\*")
                           (name . "\*info\*")))
           ("Internal" (or (name . "\*Compile-log\*")
                           (name . "\*Buffer List\*")
                           (name . "\*Messages\*")
                           (name . "\*Completions\*"))))))
  (add-hook 'ibuffer-mode-hooks
            (lambda ()
              (ibuffer-auto-mode 1)
              (ibuffer-switch-to-saved-filter-groups "davep")
              (ibuffer-do-sort-by-alphabetic))))
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
  (setq org-directory (or (getenv "ORG_DIRECTORY") "~/notebook/")
        org-default-notes-file (concat (file-name-as-directory org-directory) "inbox.org")
        org-agenda-files (list org-default-notes-file)
        org-log-done 'time
        org-src-fontify-natively t)
  (mapc #'load (directory-files "~/.config/org/" t "\\.el$"))
  :bind
  ("<f12> o a" . org-agenda)
  ("<f12> o t" . org-todo-list))
(use-package quickurl
  :bind
  ("<f12> u u" . quickurl)
  ("<f12> u l" . quickurl-list)
  :config
  (setq quickurl-url-file (locate-user-emacs-file ".quickurls.el")))
(use-package paren
  :commands show-paren-mode
  :init
  (show-paren-mode t))
(use-package savehist
  :commands savehist-mode
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
(use-package python
  :config
  (setq python-indent-guess-indent-offset nil)
  (setq python-shell-interpreter "python3"))


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
