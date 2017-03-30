;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensure use-package is available.
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My personal packages. Normally loaded in from delpa.
(use-package become   :ensure t)
(use-package binclock :ensure t)
(use-package csrclr   :ensure t)
(use-package fscroll  :ensure t)
(use-package insert   :ensure t
             :bind
             ("C-RET"   . insert-line-split-keeping-fill-prefix)
             ("C-c RET" . insert-line-split-keeping-fill-prefix)
             ("C-c f"   . insert-filename))
(use-package itch     :ensure t
             :bind
             ("M-s" . itch-scratch-buffer))
(use-package moving   :ensure t
             :bind
             ([home] . moving-home) ([end] . moving-end))
(use-package nukneval :ensure t)
(use-package unbind   :ensure t)
(use-package webinfo  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages I've written, that are in melpa.
(use-package boxquote  :ensure t
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
(use-package obfusurl  :ensure t)
(use-package protocols :ensure t)
(use-package services  :ensure t)
(use-package thinks    :ensure t :bind ("C-c C-t" . thinks-maybe-region))
(use-package uptimes   :ensure t
             :bind
             ([f11] . uptimes-current) ("C-c t" . uptimes))

(provide 'davep-packages)