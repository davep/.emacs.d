;;; init-packages-delpa.el -- Load and configure my personal packages.

;;; Commentary:
;;
;; init-packages-delpa.el loads and configures my personal packages that I
;; keep in my own package repository (the sorts of packages that would not
;; make sense in a public repository).

;;; Code:

(use-package become
  :ensure t
  :commands become-free-of-trailing-whitespace
  :init
  (unless noninteractive
    (add-hook 'before-save-hook #'become-free-of-trailing-whitespace))
  :bind
  ("<f12> <tab>" . become-freshly-indented-no-tabs))
(use-package commoji
  :ensure t
  :bind
  ("<f12> g e" . commoji))
(use-package constellations
  :ensure t)
(use-package csrclr
  :ensure t)
(use-package davep-org
  :ensure t)
(use-package end-it
  :ensure t
  :bind
  ("<f12> i e" . end-it))
(use-package expando
  :ensure t
  :bind
  ("C-c e" . expando-macro))
(use-package fasta
  :ensure t)
(when is-a-macOS-p
  (use-package festival
    :ensure t
    :config
    (setq festival-program "/usr/local/festival/festival/bin/festival")
    (setq festival-auto-start t)))
(use-package fscroll
  :ensure t)
(use-package funhead
  :ensure t
  :bind
  ("<f12> i h" . funhead))
(use-package graburl
  :ensure t)
(use-package icmp-info
  :ensure t)
(use-package insert
  :ensure t
  :bind
  ("C-<return>" . insert-line-split-keeping-fill-prefix)
  ("<f12> i a"  . insert-autoload-cookie)
  ("<f12> i f"  . insert-filename)
  ("<f12> i m"  . insert-melpa-badge)
  ("<f12> i s"  . insert-sexp-link)
  ("<f12> i y"  . insert-youtube-markdown)
  ("<f12> i ;"  . insert-break-comment))
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
(when is-a-macOS-p
  (use-package macdob
    :ensure t)
  (use-package macinfo
    :ensure t))
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
(use-package webinfo
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
(use-package wordcloud
  :ensure t)

(provide 'init-packages-delpa)

;;; init-packages-delpa.el ends here
