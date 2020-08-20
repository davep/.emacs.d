;; <URL:https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package>
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
  (python-mode . lsp)
  :custom
  (lsp-pyls-configuration-sources ["pylint"])
  (lsp-pyls-plugins-pydocstyle-enabled nil)
  (lsp-pyls-plugins-pyflakes-enabled nil)
  (lsp-pyls-plugins-flake8-enabled nil)
  (lsp-pyls-plugins-pylint-enabled t)
  (lsp-session-file (local-emacs-directory ".lsp-session-v1"))
  (lsp-enable-snippet nil))

(use-package lsp-ui
  :ensure t
  :after lsp-mode flycheck
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :ensure t
  :commands ls-ivy-workspace-symbol)

;;; lsp.el ends here
