;; <URL:https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package>
(use-package lsp-mode
  :ensure t
  :commands lsp)

;; <URL:https://emacs-lsp.github.io/lsp-pyright/>
(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp)))
  :custom
  ;; Don't dump files in my Emacs config directory.
  (lsp-session-file (local-emacs-directory ".lsp-session-v1"))
  ;; Encourage strict type checking.
  (lsp-pyright-typechecking-mode "strict"))

(use-package lsp-ui
  :ensure t
  :after lsp-mode flycheck
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :ensure t
  :commands ls-ivy-workspace-symbol)

;;; lsp.el ends here
