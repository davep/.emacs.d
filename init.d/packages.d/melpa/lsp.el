;; <URL:https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package>
(use-package lsp-mode
  :ensure t
  :commands lsp)

;; https://emacs-lsp.github.io/lsp-python-ms/
(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp)))
  :init
  (setq lsp-python-ms-executable (executable-find "python-language-server"))
  :custom
  ;; Don't dump files in my Emacs config directory.
  (lsp-session-file (local-emacs-directory ".lsp-session-v1")))

(use-package lsp-ui
  :ensure t
  :after lsp-mode flycheck
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :ensure t
  :commands ls-ivy-workspace-symbol)

;;; lsp.el ends here
