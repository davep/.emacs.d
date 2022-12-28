(use-package yasnippet
  :ensure t
  :custom
  (yas-snipper-dirs (list (locate-user-emacs-file ".snippets")))
  :config
  (yas-global-mode 1))

