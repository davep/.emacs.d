;;; init-packages-builtin.el -- Load and configure some builtin packages

;;; Commentary:
;;
;; init-packages-builtin.el loads and configures various packages that are
;; part of GNU Emacs itself.

;;; Code:


;; General modes.
(use-package calendar
  :config
  (setq
   diary-file             "~/.emacs.d/.diary"
   calendar-day-style     'iso
   calendar-latitude      55.9
   calendar-longitude     -3.2
   calendar-location-name "Edinburgh, Scotland"))
(use-package dired-x
  :config
  (setq-default dired-omit-files-p t)
  (setq dired-omit-files "^\\.[^.]"))
(use-package eshell
  :config
  (setq
   eshell-directory-name  "~/.emacs.d/.eshell/"
   eshell-prompt-regexp   "^[^#$\\n]*[#$] "
   eshell-prompt-function (lambda ()
                            (concat (user-login-name)
                             ":"
                             (abbreviate-file-name (eshell/pwd))
                             (if is-a-root-user-p "#" "$")
                             " "))))
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
  :config
  (add-hook 'ibuffer-mode-hooks
            (lambda ()
              (ibuffer-auto-mode 1))))
(use-package message
  :config
  (add-hook 'message-mode-hook
            (lambda ()
              (setq smartsig-set-signature #'msig-set)
              (setq smartsig-abbrev-table  'message-mode-abbrev-table)
              (smartsig-clear)
              (smartsig-add "emacs"   "~/.sigs/emacs"   "emacs" "xemacs" "elisp" "gnu" "lbdb" "uptimes" "quickurl" "smartsig" "boxquote")
              (smartsig-add "sawfish" "~/.sigs/sawfish" "sawfish" "sawmill" "librep" "rep" "gnome"))))
(use-package quickurl
  :bind
  ("C-c u" . quickurl)
  :config
  (setq quickurl-url-file (locate-user-emacs-file ".quickurls.el")))
(use-package tramp
  :init
  (setq
   tramp-default-method       "ssh"
   tramp-persistency-file-name "~/.emacs.d/.tramp.el"))


;; Programming modes.
(use-package cc-mode
  :bind
  (:map c-mode-map   ("RET" . newline-and-indent))
  (:map c++-mode-map ("RET" . newline-and-indent)))
(use-package js
  :bind
  (:map js-mode-map ("RET" . newline-and-indent)))
(use-package opascal
  :bind
  (:map opascal-mode-map ("RET" . newline-and-indent)))
(use-package pascal
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
