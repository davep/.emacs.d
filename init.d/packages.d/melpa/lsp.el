;; <URL:https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package>
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
  (python-mode . lsp)
  :custom
  ;; The following is an attempt to try and convince lsp-mode and/or pyls to
  ;; use pylint and nothing else to lint my code. Right now it doesn't seem
  ;; to be making much difference. Despite everything else being disabled it
  ;; seems to still be falling back to using pydocstyle. :-/
  (lsp-pyls-configuration-sources ["pylint"])
  (lsp-pyls-plugins-pydocstyle-enabled nil)
  (lsp-pyls-plugins-pyflakes-enabled nil)
  (lsp-pyls-plugins-flake8-enabled nil)
  (lsp-pyls-plugins-pylint-enabled t)
  ;; Because of the above, try and tweak pydocstyle to use the style I
  ;; prefer for all my code. Mostly this is about having slightly longer
  ;; lines and not being so damn upset about whitespace.
  (lsp-pyls-plugins-pycodestyle-max-line-length 120) ; This isn't a punched card.
  (lsp-pyls-plugins-pycodestyle-ignore
   ["E221" "E251" "E272"                ; I like to line things up.
    "E201" "E202"                       ; I like space inside parens.
    "E302"                              ; I don't need double vertical spacing.
    "E266"                              ; Emacs-like end of file markers is
                                        ; a hill I will die on.
    ])
  ;; Don't dump files in my Emacs config directory.
  (lsp-session-file (local-emacs-directory ".lsp-session-v1"))
  ;; Because I don't have yasnippet set up and lsp-mode seems to be getting
  ;; upset about that.
  (lsp-enable-snippet nil))

(use-package lsp-ui
  :ensure t
  :after lsp-mode flycheck
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :ensure t
  :commands ls-ivy-workspace-symbol)

;;; lsp.el ends here
