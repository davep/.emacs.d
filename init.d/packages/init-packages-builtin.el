;;; init-packages-builtin.el -- Load and configure some builtin packages

;;; Commentary:
;;
;; init-packages-builtin.el loads and configures various packages that are
;; part of GNU Emacs itself.

;;; Code:

(require 'is-a)


;; General modes.
(use-package abbrev
  :custom
  (abbrev-file-name (local-emacs-directory "abbrev_defs.el")))
(use-package autorevert
  :diminish auto-revert-mode)
(use-package bookmark
  :custom
  (bookmark-default-file (local-emacs-directory "bookmarks.el"))
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
(use-package dictionary
  :bind
  ("<f12> ? d" . dictionary-lookup-definition))
(use-package eldoc
  :diminish)
(use-package elec-pair
  :commands electric-pair-mode
  :init
  (electric-pair-mode t)
  :custom
  (electric-pair-inhibit-predicate
   (lambda (c)
     ;; Don't complete { when in web-mode and using the Django engine,
     ;; as it does its own thing.
     (or
      ;; This isn't ideal. I often don't want pair completion inside the
      ;; minibuffer, but there are times when I might (`eval-expression'
      ;; being one such time when I'd want it). Given the former is more
      ;; common than the latter, and I can't really see a good way of only
      ;; turning this off for some things, let's turn it off any time we're
      ;; in the minibuffer.
      (eq major-mode 'minibuffer-inactive-mode)
      (and
       (eq major-mode 'web-mode)
       (string= web-mode-engine "django")
       (= c ?{))))))
(use-package eshell
  :custom
  (eshell-directory-name  (locate-user-emacs-file ".eshell/"))
  (eshell-prompt-regexp   "^[^#\\$]*[#\\$] ")
  (eshell-promptq-function (lambda ()
                             (concat (user-login-name)
                                     ":"
                                     (abbreviate-file-name (eshell/pwd))
                                     (if is-a-root-user-p "#" "$")
                                     " ")))
  :custom-face
  (eshell-prompt ((t (:foreground "indian red" :weight bold))))
  :custom
  (eshell-destroy-buffer-when-process-dies t)
  (eshell-visual-options
   '(("git" "commit" "log")))
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands "htop"))))
(use-package eww
  :custom
  (eww-bookmarks-directory (locate-user-emacs-file ".eww")))
(use-package gamegrid
  :config
  (setq gamegrid-user-score-file-directory (local-emacs-directory "games/")))
(use-package ispell
  :custom
  (ispell-dictionary     "british")
  (ispell-highlight-face 'flyspell-incorrect)
  :config
  ;; Try and get aspell working on a Windows machine.
  (let ((aspell "C:/Program Files (x86)/Aspell/bin/"))
    (when (and is-a-win32-p (file-exists-p aspell))
      (push aspell exec-path)
      (setq ispell-program-name "aspell"))))
(use-package flyspell
  :diminish
  :commands
  flyspell-mode
  flyspell-prog-mode
  :hook (prog-mode . flyspell-prog-mode))
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
  :custom
  (gnus-default-nntp-server "news.eternal-september.org")
  (gnus-select-method `(nntp ,gnus-default-nntp-server)))
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
           ("*Magit*"      (derived-mode . magit-mode))
           ("Org"          (or
                            (mode . org-mode)
                            (mode . org-agenda-mode)))
           (".emacs.d"     (filename . "/.emacs.d/"))
           ("shell"        (mode . sh-mode))
           ("elisp"        (mode . emacs-lisp-mode))
           ("Makefile"     (derived-mode . makefile-mode))
           ("python"       (mode . python-mode))
           ("JavaScript"   (mode . js2-mode))
           ("web"          (or
                            (mode . web-mode)
                            (mode . scss-mode)))
           ("text files"   (or
                            (derived-mode . text-mode)
                            (mode . fasta-mode)))
           ("directories"  (mode . dired-mode))
           ("Help"         (or
                            (name . "\*Help\*")
                            (name . "\*Apropos\*")
                            (name . "\*info\*")))
           ("Internal"     (or
                            (name . "\*Compile-log\*")
                            (name . "\*Buffer List\*")
                            (name . "\*Backtrace\*")
                            (name . "\*Messages\*")
                            (name . "\*Completions\*")
                            (name . "\*Calendar\*")
                            (name . "\*tramp/sudo")
                            (name . "\*Packages\*")
                            (mode . inferior-python-mode))))))
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
  :custom
  (org-directory (or (getenv "ORG_DIRECTORY") "~/notebook/"))
  (org-default-notes-file (concat (file-name-as-directory org-directory) "inbox.org"))
  (org-agenda-files (list org-default-notes-file))
  (org-link-file-path-type 'relative)
  (org-log-done 'time)
  (org-src-fontify-natively t)
  (org-cycle-separator-lines 1)
  :config
  (defun org-davep-config ()
    "Load up the rest of my org-mode config."
    (interactive)
    (let ((org-local-config "~/.config/org/"))
      (when (file-exists-p org-local-config)
        (mapc #'load (directory-files org-local-config t "\\.el$")))))
  (funcall 'org-davep-config)
  :bind
  ("<f12> o a" . org-agenda)
  ("<f12> o t" . org-todo-list))
(use-package quickurl
  :bind
  ("<f12> u u" . quickurl)
  ("<f12> u l" . quickurl-list)
  :custom
  (quickurl-url-file (locate-user-emacs-file ".quickurls.el")))
(use-package paren
  :commands show-paren-mode
  :init
  (show-paren-mode t))
(use-package recentf
  :commands recentf-save-list
  :custom
  (recentf-save-file (local-emacs-directory "recentf"))
  :init
  ;; Auto-save the list every 30 minutes.
  (run-at-time nil (* 30 60) #'recentf-save-list))
(use-package savehist
  :commands savehist-mode
  :custom
  (savehist-file (local-emacs-directory "history.el"))
  :init
  (savehist-mode t))
(use-package tramp
  :custom
  (tramp-default-method        "ssh")
  (tramp-persistency-file-name (local-emacs-directory "tramp.el")))


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
            (auto-fill-mode 1)
            (flyspell-mode 1)
            (footnote-mode 1)))
(add-hook 'prog-mode-hook
          (lambda ()
            ;; Set up for auto-filling comments.
            (setq-local comment-auto-fill-only-comments t)
            (auto-fill-mode 1)))

(provide 'init-packages-builtin)

;;; init-packages-builtin.el ends here
