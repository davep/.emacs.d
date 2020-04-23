(use-package highlight-indent-guides
  :ensure t
  :diminish
  :commands highlight-indent-guides-mode
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  :hook (prog-mode . (lambda ()
                       (unless (derived-mode-p 'makefile-mode)
                         (highlight-indent-guides-mode 1)))))
