(use-package eshell
  :defines eshell-visual-commands
  :functions eshell/pwd
  :custom
  (eshell-directory-name  (locate-user-emacs-file ".eshell/"))
  (eshell-prompt-regexp   "^[^#\\$]*[#\\$] ")
  (eshell-promptq-function (lambda ()
                             (concat (user-login-name)
                                     ":"
                                     (abbreviate-file-name (eshell/pwd))
                                     (if is-a-root-user-p "#" "$")
                                     " ")))
  :custom-face
  (eshell-prompt ((t (:foreground "indian red" :weight bold))))
  :custom
  (eshell-destroy-buffer-when-process-dies t)
  (eshell-visual-options '(("git" "commit" "log")))
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands "htop"))))
