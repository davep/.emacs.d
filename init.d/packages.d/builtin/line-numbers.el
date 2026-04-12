;; -*- lexical-binding: t; -*-

(use-package display-line-numbers
  :if (display-graphic-p)
  :commands (display-line-numbers-mode)
  :hook
  (prog-mode . display-line-numbers-mode)
  (text-mode . display-line-numbers-mode)
  (conf-mode . display-line-numbers-mode))

;;; line-numbers.el ends here
