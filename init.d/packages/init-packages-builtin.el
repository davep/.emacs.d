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
        (mapc #'load (directory-files org-local-config t (rx ".el" eol))))))
  (funcall 'org-davep-config)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))
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
  :config
  (add-to-list 'recentf-exclude (rx ".local/share/emacs/"))
  :init
  ;; Auto-save the list every 30 minutes.
  (run-at-time nil (* 30 60)
               (lambda ()
                 (let ((inhibit-message t))
                   (recentf-save-list)))))
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
