;; <URL:https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package>
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
  (python-mode . lsp)
  :custom
  (lsp-pyls-configuration-sources ["pylint"])
  (lsp-session-file (local-emacs-directory ".lsp-session-v1")))

(use-package lsp-ui
  :ensure t
  :after lsp-mode flycheck
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :ensure t
  :commands ls-ivy-workspace-symbol)

;;; lsp.el ends here
