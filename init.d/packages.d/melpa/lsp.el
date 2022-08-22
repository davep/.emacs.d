;; <URL:https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package>
(use-package lsp-mode
  :ensure t
  :commands lsp)

;; <URL:https://emacs-lsp.github.io/lsp-pyright/>
(use-package lsp-pyright
  :ensure t
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp)))
  :custom
  ;; Don't dump files in my Emacs config directory.
  (lsp-session-file (local-emacs-directory ".lsp-session-v1"))
  ;; Encourage strict type checking.
  (lsp-pyright-typechecking-mode "strict"))

;; :hook
;; (python-mode . lsp)
;; :custom
;; ;; The following is an attempt to try and convince lsp-mode and/or pyls to
;; ;; use pylint and nothing else to lint my code. Right now it doesn't seem
;; ;; to be making much difference. Despite everything else being disabled it
;; ;; seems to still be falling back to using pydocstyle. :-/
;; (lsp-pyls-configuration-sources ["pylint"])
;; (lsp-pyls-plugins-pydocstyle-enabled nil)
;; (lsp-pyls-plugins-pyflakes-enabled nil)
;; (lsp-pyls-plugins-flake8-enabled nil)
;; (lsp-pyls-plugins-pylint-enabled t)
;; ;; Because of the above, try and tweak pydocstyle to use the style I
;; ;; prefer for all my code. Mostly this is about having slightly longer
;; ;; lines and not being so damn upset about whitespace.
;; (lsp-pyls-plugins-pycodestyle-max-line-length 120) ; This isn't a punched card.
;; (lsp-pyls-plugins-pycodestyle-ignore
;;  [
;;   ;; I like to line things up.
;;   "E221" "E241" "E251" "E271" "E272"
;;   ;; I like space inside parens.
;;   "E201" "E202"
;;   ;; Sometimes just one space before a comment.
;;   "E261"
;;   ; I don't need double vertical spacing.
;;   "E302" "E305"
;;   ;; I don't like space around parameter equals.
;;   "E252"
;;   ;; Emacs-like end of file markers is a hill I will die on.
;;   "E266"
;;   ;; W503 and W504 are kind of mutually exclusive, and looking at the docs
;;   ;; (https://www.flake8rules.com/rules/W503.html) W503 is deprecated
;;   ;; anyway. So let's ignore W503.
;;   "W503"
;;   ])
;; ;; Don't dump files in my Emacs config directory.
;; (lsp-session-file (local-emacs-directory ".lsp-session-v1"))
;; ;; Because I don't have yasnippet set up and lsp-mode seems to be getting
;; ;; upset about that.
;; (lsp-enable-snippet nil))

(use-package lsp-ui
  :ensure t
  :after lsp-mode flycheck
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :ensure t
  :commands ls-ivy-workspace-symbol)

;;; lsp.el ends here
