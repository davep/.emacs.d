(use-package shell-maker
  :ensure t
  :custom
  (shell-maker-history-path local-emacs-directory))

(use-package chatgpt-shell
  :ensure t
  :custom
  (chatgpt-shell-openai-key (lambda () (auth-source-pick-first-password :host "api.openai.com"))))
