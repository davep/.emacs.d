(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  :hook
  (python-mode . (lambda ()
                   (add-to-list 'flycheck-disabled-checkers 'python-flake8))))

(use-package flycheck-rust
  :ensure t
  ;; :hook
  ;; ??? what here?
  )
