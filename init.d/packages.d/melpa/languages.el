(use-package apache-mode :ensure t)

(use-package cider :ensure t)

(use-package dockerfile-mode :ensure t)

(use-package fish-mode :ensure t)

(use-package gitconfig-mode :ensure t)

(use-package go-mode :ensure t)

(use-package haskell-mode :ensure t
  :bind ("C-c C-c" . haskell-process-load-file))

(use-package hy-mode :ensure t)

(use-package ini-mode :ensure t
  :mode (rx (or
             "Pipfile"
             ".pylintrc")
            eol))

(use-package json-mode :ensure t)

(use-package julia-mode :ensure t :interpreter "julia")

(use-package powershell :ensure t)

(use-package rjsx-mode :ensure t
  :bind*
  ("<" . self-insert-command))

(use-package rust-mode :ensure t)

(use-package web-mode :ensure t
  :mode (rx ".html" eol)
  :commands web-mode-set-engine
  :config
  (setq-default web-mode-markup-indent-offset 2)
  (setq web-mode-enable-engine-detection t))

(use-package yaml-mode :ensure t)

(use-package zig-mode :ensure t)
