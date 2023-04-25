(use-package chatgpt-shell
  :ensure t
  :custom
  (chatgpt-shell-openai-key (lambda () (auth-source-pick-first-password :host "api.openai.com"))))
