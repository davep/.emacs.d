(use-package cc-mode
  :custom
  (c-basic-offset 4)
  (c-offsets-alist '((inline-open . 0)
                     (case-label . +)))
  (c-default-style '((c-mode    . "BSD")
                     (c++-mode  . "BSD")
                     (java-mode . "java")
                     (awk-mode  . "awk")
                     (other     . "gnu")))
  :bind
  (:map c-mode-map   ("RET" . newline-and-indent))
  (:map c++-mode-map ("RET" . newline-and-indent)))

(use-package js2-mode
  :mode "\\.js$"
  :custom
  (js-switch-indent-offset 4))

(use-package python
  :custom
  (python-indent-guess-indent-offset nil)
  (python-shell-interpreter "python3")
  (python-fill-docstring-style 'pep-257-nn))

(add-hook 'prog-mode-hook
          (lambda ()
            ;; Make it clear where the empty lines are
            (setq-local indicate-empty-lines t)
            ;; Set up for auto-filling comments.
            (setq-local comment-auto-fill-only-comments t)
            (auto-fill-mode 1)))

(add-to-list 'auto-mode-alist '("\\.tcss$" . scss-mode))
