;;; init-packages-delpa.el -- Load and configure my personal packages.

;;; Commentary:
;;
;; init-packages-delpa.el loads and configures my personal packages that I
;; keep in my own package repository (the sorts of packages that would not
;; make sense in a public repository).

;;; Code:

(use-package expando
  :ensure t
  :bind
  ("C-c e" . expando-macro))
(use-package fscroll
  :ensure t)
(use-package funhead
  :ensure t
  :bind
  ("<f12> i h" . funhead))
(use-package gitweb
  :ensure t
  :bind
  ("<f12> g w" . gitweb))
(use-package graburl
  :ensure t)
(use-package icmp-info
  :ensure t)
(use-package itch
  :ensure t
  :bind*
  ("M-s"         . itch-scratch-buffer)
  ("<XF86Eject>" . itch-scratch-buffer))
(use-package longmacs
  :ensure t)
(use-package make-phony
  :ensure t
  :bind
  ("<f12> i p" . make-phony))
(use-package moving
  :ensure t
  :bind
  ("<home>"    . moving-home)
  ("s-<left>"  . moving-home)
  ("<end>"     . moving-end)
  ("s-<right>" . moving-end)
  ("s-<up>"    . moving-backward-page)
  ("s-<down>"  . moving-forward-page))
(use-package msig
  :ensure t)
(use-package ngn
  :ensure t)
(use-package nuke-buffers
  :ensure t
  :config
  (add-to-list 'nuke-buffers-ignore "*node process*")
  (add-to-list 'nuke-buffers-ignore "*JS scratch*")
  (add-to-list 'nuke-buffers-ignore "*JS REPL*")
  :bind
  ("C-M-<f11>"     . nuke-buffers)
  ("C-s-<f11>"     . nuke-buffers)
  ("C-<XF86Eject>" . nuke-buffers))
(use-package nukneval
  :ensure t)
(use-package org-davep
  :ensure t
  :bind
  ("<f12> o i" . org-davep-open-inbox)
  ("<f12> o d" . org-davep-open-dir))
(use-package pypath
  :ensure t)
(use-package qrencode
  :ensure t)
(use-package rate-sx
  :ensure t
  :bind
  ("<f12> c" . rate-sx))
(use-package requote
  :ensure t
  :bind
  ("<f12> q" . requote))
(use-package setup-compile
  :ensure t
  :commands setup-compile
  :hook (c-mode c++mode emacs-lisp-mode))
(use-package smartsig
  :ensure t)
(use-package unbind
  :ensure t)
(use-package winsplit
  :ensure t
  :bind*
  ("C-c <right>"   . winsplit-right)
  ("C-c <left>"    . winsplit-left)
  ("C-c <up>"      . winsplit-above)
  ("C-c <down>"    . winsplit-below)
  ("C-c C-<right>" . winsplit-right-load)
  ("C-c C-<left>"  . winsplit-left-load)
  ("C-c C-<up>"    . winsplit-above-load)
  ("C-c C-<down>"  . winsplit-below-load))

(provide 'init-packages-delpa)

;;; init-packages-delpa.el ends here
