;;; davep-packages-builtin.el -- Load and configure some builtin packages

;;; Commentary:
;;
;; davep-packages-builtin.el loads and configures various packages that are
;; part of GNU Emacs itself.

;;; Code:

(use-package flyspell
  :config
  (mapc (lambda (hook)
          (add-hook hook #'flyspell-prog-mode))
        '(c-mode-hook
          c++-mode-hook
          emacs-lisp-mode-hook
          js-mode-hook)))
(use-package footnote
  :config
  (add-hook 'footnote-mode-hook
            (lambda ()
              (setq footnote-style 'numeric-latin
                    footnote-spaced-footnotes nil
                    footnote-section-tag "-----"
                    footnote-section-tag-regexp (regexp-quote footnote-section-tag)
                    footnote-narrow-to-footnotes-when-editing t))))
(use-package ibuffer
  :bind
  ([(meta f6)] . ibuffer)
  :config
  (add-hook 'ibuffer-mode-hooks
            (lambda ()
              (ibuffer-auto-mode 1))))
(use-package quickurl
  :bind
  ("C-c u" . quickurl)
  :config
  (setq quickurl-url-file (locate-user-emacs-file ".quickurls.el")))
(use-package pascal
  :bind
  (:map pascal-mode-map ("RET" . newline-and-indent)))
(use-package opascal
  :bind
  (:map opascal-mode-map ("RET" . newline-and-indent)))
(use-package js
  :bind
  (:map js-mode-map ("RET" . newline-and-indent)))
(use-package cc-mode
  :config
  (add-hook 'c-mode-hook   #'setup-compile)
  (add-hook 'c++-mode-hook #'setup-compile)
  :bind
  (:map c-mode-map   ("RET" . newline-and-indent))
  (:map c++-mode-map ("RET" . newline-and-indent)))
(use-package dired-x
  :config
  (setq-default dired-omit-files-p t)
  (setq dired-omit-files "^\\.[^.]"))
(use-package message
  :config
  (add-hook 'message-mode-hook
            (lambda ()
              (setq smartsig-set-signature #'msig-set)
              (setq smartsig-abbrev-table  'message-mode-abbrev-table)
              (smartsig-clear)
              (smartsig-add "emacs"   "~/.sigs/emacs"   "emacs" "xemacs" "elisp" "gnu" "lbdb" "uptimes" "quickurl" "smartsig" "boxquote")
              (smartsig-add "sawfish" "~/.sigs/sawfish" "sawfish" "sawmill" "librep" "rep" "gnome"))))

(provide 'davep-packages-builtin)

;;; davep-packages-builtin.el ends here
