(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(case-fold-search t)
 '(column-number-mode t)
 '(cursor-in-non-selected-windows nil)
 '(diff-switches "-u")
 '(electric-pair-mode t)
 '(eshell-prompt-function
   (lambda nil
     (concat
      (user-login-name)
      ":"
      (eshell/pwd)
      (if rootp "#" "$")
      " ")))
 '(eshell-prompt-regexp "^[^#$\\n]*[#$] ")
 '(fill-column 76)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(ispell-program-name "aspell")
 '(mouse-avoidance-mode (quote jump) nil (avoid))
 '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(scroll-error-top-bottom t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh")
 '(truncate-lines t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
