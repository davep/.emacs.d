(use-package yasnippet
  :ensure t
  :custom
  (yas-snippet-dirs (list (locate-user-emacs-file ".snippets")))
  :config
  (yas-global-mode 1))

