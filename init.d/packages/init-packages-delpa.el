;;; init-packages-delpa.el -- Load and configure my personal packages.

;;; Commentary:
;;
;; init-packages-delpa.el loads and configures my personal packages that I
;; keep in my own package repository (the sorts of packages that would not
;; make sense in a public repository).

;;; Code:

(use-package become
  :ensure t
  :config
  (unless noninteractive
    (add-hook 'before-save-hook #'become-free-of-trailing-whitespace))
  :bind
  ([(control c) (control tab)] . become-freshly-indented-no-tabs))
(use-package constellations
  :ensure t)
(use-package csrclr
  :ensure t)
(use-package davep-org
  :ensure t)
(use-package expando
  :ensure t
  :bind
  ("C-c C-e" . expando-macro))
(use-package fscroll
  :ensure t)
(use-package graburl
  :ensure t)
(use-package icmp-info
  :ensure t)
(use-package insert
  :ensure t
  :bind
  ("C-RET"   . insert-line-split-keeping-fill-prefix)
  ("C-c RET" . insert-line-split-keeping-fill-prefix)
  ("C-c f"   . insert-filename)
  ("C-c s"   . insert-sexp-link)
  ("C-c a"   . insert-autoload-cookie)
  ("C-c C-;" . insert-break-comment))
(use-package itch
  :ensure t
  :bind
  ("M-s" . itch-scratch-buffer))
(use-package longmacs
  :ensure t)
(use-package macdob
  :if is-a-macOS-p
  :ensure t)
(use-package moving
  :ensure t
  :bind
  ([home]          . moving-home)
  ([(super left)]  . moving-home)
  ([end]           . moving-end)
  ([(super right)] . moving-end))
(use-package msig
  :ensure t)
(use-package ngn
  :ensure t)
(use-package nuke-buffers
  :ensure t
  :bind
  ([(control meta f11)] . nuke-buffers))
(use-package nukneval
  :ensure t)
(use-package org-davep
  :ensure t
  :bind
  ("C-c o i" . org-davep-open-inbox)
  ("C-c o d" . org-davep-open-dir)
  ("C-c o a" . org-agenda))
(use-package qrencode
  :ensure t)
(use-package setup-compile
  :ensure t
  :config
  (add-hook 'c-mode-hook          #'setup-compile)
  (add-hook 'c++-mode-hook        #'setup-compile)
  (add-hook 'emacs-lisp-mode-hook #'setup-compile))
(use-package smartsig
  :ensure t)
(use-package unbind
  :ensure t)
(use-package webinfo
  :ensure t)
(use-package winsplit
  :ensure t
  :bind
  ([(control c) (right)]         . winsplit-right)
  ([(control c) (left)]          . winsplit-left)
  ([(control c) (up)]            . winsplit-above)
  ([(control c) (down)]          . winsplit-below)
  ([(control c) (control right)] . winsplit-right-load)
  ([(control c) (control left)]  . winsplit-left-load)
  ([(control c) (control up)]    . winsplit-above-load)
  ([(control c) (control down)]  . winsplit-below-load))
(use-package xbase
  :mode ("\\.\\(prg\\|ch\\|ppo\\)$" . xbase-mode)
  :ensure t)

(provide 'init-packages-delpa)

;;; init-packages-delpa.el ends here
