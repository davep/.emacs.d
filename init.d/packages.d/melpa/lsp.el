;; <URL:https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package>
(use-package lsp-mode
  :ensure t
  :commands lsp
  ;; I mainly added this to use with Python, but with the projects I want to
  ;; use this with, I'm having a lot of problems getting pyls to either
  ;; install (likely really a pipenv problem) or, when it does, I can't seem
  ;; to configure the linting so that it uses pylint (which is kind of
  ;; important to how I do things). So, for now, as daft as it seems, I'm
  ;; going to have lsp-mode installed but not configured for any language
  ;; just yet.
  ;;
  ;; :hook
  ;; (python-mode . lsp)
  )

(use-package lsp-ui
  :ensure t
  :after lsp-mode flycheck
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :ensure t
  :commands ls-ivy-workspace-symbol)

;;; lsp.el ends here
