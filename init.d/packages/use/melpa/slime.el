(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "ros run")
  (add-hook 'slime-inferior-process-start-hook
            (lambda ()
              (require 'slime-fancy))))
