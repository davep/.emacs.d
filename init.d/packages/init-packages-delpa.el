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
  ("<f12> <tab>" . become-freshly-indented-no-tabs))
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
  ("C-c i a" . insert-autoload-cookie)
  ("C-c i f" . insert-filename)
  ("C-c i m" . insert-melpa-badge)
  ("C-c i s" . insert-sexp-link)
  ("C-c i y" . insert-youtube-markdown)
  ("C-c i ;" . insert-break-comment))
(use-package itch
  :ensure t
  :bind
  ("<f12> s" . itch-scratch-buffer))
(use-package longmacs
  :ensure t)
(use-package macdob
  :if is-a-macOS-p
  :ensure t)
(use-package moving
  :ensure t
  :bind
  ("<home>"    . moving-home)
  ("s-<left>"  . moving-home)
  ("<end>"     . moving-end)
  ("s-<right>" . moving-end))
(use-package msig
  :ensure t)
(use-package ngn
  :ensure t)
(use-package nuke-buffers
  :ensure t
  :bind
  ("C-M-<f11>" . nuke-buffers))
(use-package nukneval
  :ensure t)
(use-package org-davep
  :ensure t
  :bind
  ("<f12> o i" . org-davep-open-inbox)
  ("<f12> o d" . org-davep-open-dir)
  ("<f12> o a" . org-agenda))
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
  ("C-c <right>"   . winsplit-right)
  ("C-c <left>"    . winsplit-left)
  ("C-c <up>"      . winsplit-above)
  ("C-c <down>"    . winsplit-below)
  ("C-c C-<right>" . winsplit-right-load)
  ("C-c C-<left>"  . winsplit-left-load)
  ("C-c C-<up>"    . winsplit-above-load)
  ("C-c C-<down>"  . winsplit-below-load))
(use-package xbase
  :mode ("\\.\\(prg\\|ch\\|ppo\\)$" . xbase-mode)
  :ensure t)

(provide 'init-packages-delpa)

;;; init-packages-delpa.el ends here
