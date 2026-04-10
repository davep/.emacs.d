;; -*- lexical-binding: t; -*-

(use-package copilot
  :ensure t
  :defer t
  :diminish "O_o"
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("C-<tab>" . copilot-accept-completion-by-word)
              ("C-n" . copilot-next-completion)
              ("C-p" . copilot-previous-completion))
  :custom
  (copilot-indent-offset-warning-disable t)
  (copilot-max-char-warning-disable t))

;;; copilot.el ends here
