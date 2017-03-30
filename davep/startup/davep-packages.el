;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensure use-package is available.
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My personal packages. Normally loaded in from delpa.
(use-package become   :ensure t)
(use-package binclock :ensure t)
(use-package csrclr   :ensure t)
(use-package fscroll  :ensure t)
(use-package insert   :ensure t)
(use-package itch     :ensure t)
(use-package moving   :ensure t)
(use-package nukneval :ensure t)
(use-package unbind   :ensure t)
(use-package webinfo  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages I've written, that are in melpa.
(use-package boxquote  :ensure t)
(use-package obfusurl  :ensure t)
(use-package protocols :ensure t)
(use-package services  :ensure t)
(use-package thinks    :ensure t)
(use-package uptimes   :ensure t)

(provide 'davep-packages)
