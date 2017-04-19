;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensure use-package is available.
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs builtin packages, that aren't normally loaded.
(use-package footnote
    :ensure t
    :config
    (add-hook 'footnote-mode-hook
              #'(lambda ()
                  (setq footnote-style 'numeric-latin
                        footnote-spaced-footnotes nil
                        footnote-section-tag "-----"
                        footnote-section-tag-regexp (regexp-quote footnote-section-tag)
                        footnote-narrow-to-footnotes-when-editing t))))
(use-package ibuffer
    :ensure t
    :bind
    ([(meta f6)] . ibuffer)
    :config
    (add-hook 'ibuffer-mode-hooks
              #'(lambda ()
                  (ibuffer-auto-mode 1))))
(use-package quickurl
    :ensure t
    :bind
    ("C-c u" . quickurl)
    :config
    (setq quickurl-url-file (locate-user-emacs-file ".quickurls.el")))
(use-package opascal
    :ensure t
    :bind
    (:map opascal-mode-map ("RET" . newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My personal packages. Normally loaded in from delpa.
(use-package become
    :ensure t
    :config
    (unless noninteractive
      (add-hook 'before-save-hook #'become-free-of-trailing-whitespace)))
(use-package binclock
    :ensure t)
(use-package constellations
    :ensure t)
(use-package csrclr
    :ensure t)
(use-package davep-org
    :ensure t)
(use-package fscroll
    :ensure t)
(use-package icmp-info
    :ensure t)
(use-package insert
    :ensure t
    :bind
    ("C-RET"   . insert-line-split-keeping-fill-prefix)
    ("C-c RET" . insert-line-split-keeping-fill-prefix)
    ("C-c f"   . insert-filename))
(use-package itch
    :ensure t
    :bind
    ("M-s" . itch-scratch-buffer))
(use-package longmacs
    :ensure t)
(use-package moving
    :ensure t
    :bind
    ([home] . moving-home)
    ([end]  . moving-end))
(use-package nukneval
    :ensure t)
(use-package org-davep
    :ensure t
    :bind
    ("C-c o"   . org-davep-open-inbox)
    ("C-c C-o" . org-davep-open-dir)
    ("C-c a"   . org-agenda))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages I've written, that are in melpa.
(use-package boxquote
    :ensure t
    :bind
    ("C-c i"     . boxquote-insert-file)
    ("C-c M-w"   . boxquote-kill-ring-save)
    ("C-c y"     . boxquote-yank)
    ("C-c b"     . boxquote-region)
    ("C-c C-b"   . boxquote-title)
    ("C-c C-h f" . boxquote-describe-function)
    ("C-c C-h v" . boxquote-describe-variable)
    ("C-c C-h k" . boxquote-describe-key)
    ("C-c !"     . boxquote-shell-command)
    ("C-c C-h w" . boxquote-where-is))
(use-package obfusurl
    :ensure t)
(use-package protocols
    :ensure t)
(use-package services
    :ensure t)
(use-package thinks
    :ensure t
    :bind
    ("C-c C-t" . thinks-maybe-region))
(use-package uptimes
    :ensure t
    :bind
    ([f11]        . uptimes-current)
    ([(meta f11)] . uptimes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Third-party packages from elpa/melpa
(use-package auto-compile
    :ensure t
    :config
    (auto-compile-on-save-mode)
    (auto-compile-on-load-mode))
(use-package browse-kill-ring
    :ensure t
    :bind
    ("C-c k" . browse-kill-ring))
(use-package csharp-mode
    :ensure t)
(use-package dictionary
    :ensure t)
(use-package emojify
    :if davep:unix-window-p
    :ensure t)
(use-package hyde
    :ensure t
    :commands hyde)
(use-package markdown-mode
    :ensure t
    :config
    (add-hook 'markdown-mode-hook #'(lambda ()
                                      (auto-fill-mode)
                                      (flyspell-mode 1))))
(use-package package-lint
    :ensure t)
(use-package paren-face
    :ensure t
    :demand
    :config
    (global-paren-face-mode t))
(use-package powershell
    :ensure t)
(use-package restclient
    :ensure t)
(use-package sass-mode
    :ensure t
    :mode "\\.scss$")
(use-package slime
    :ensure t)
(use-package w3m
    :if davep:macOS-p
    :ensure t)
(use-package highlight-chars
    :ensure t
    :demand
    :commands hc-highlight-trailing-whitespace
    :config
    (add-hook 'after-change-major-mode-hook
              #'(lambda ()
                  (when (buffer-file-name)
                    (hc-highlight-trailing-whitespace)))))
(use-package multiple-cursors
    :ensure t
    :bind
    ("C-."   . mc/mark-next-like-this)
    ("C-,"   . mc/mark-previous-like-this)
    ("C->"   . mc/mark-all-like-this)
    ("C-c ." . mc/mark-all-like-this-dwim))

(provide 'davep-packages)
