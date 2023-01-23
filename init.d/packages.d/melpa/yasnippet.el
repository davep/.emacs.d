(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :custom
  (yas-snippet-dirs (list (locate-user-emacs-file ".snippets")))
  :hook (eglot-managed-mode . (lambda () (setq-local company-backends '((company-capf :with company-yasnippet)))))
  :config
  (yas-global-mode 1))

(use-package ivy-yasnippet
  :ensure t
  :bind
  ("<f12> y" . ivy-yasnippet))
