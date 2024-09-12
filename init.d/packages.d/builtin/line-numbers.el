(use-package display-line-numbers
  :if (display-graphic-p)
  :commands (display-line-numbers-mode)
  :hook
  (prog-mode . display-line-numbers-mode)
  (text-mode . display-line-numbers-mode))
