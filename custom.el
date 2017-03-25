(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-image-file-mode t)
 '(blink-cursor-mode nil)
 '(case-fold-search t)
 '(column-number-mode t)
 '(cursor-in-non-selected-windows nil)
 '(diff-switches "-u")
 '(dired-use-ls-dired nil)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-format "%F %H:%M")
 '(display-time-mode t)
 '(electric-pair-mode t)
 '(eshell-prompt-function
   (lambda nil
     (concat
      (user-login-name)
      ":"
      (eshell/pwd)
      (if davep:rootp "#" "$")
      " ")))
 '(eshell-prompt-regexp "^[^#$\\n]*[#$] ")
 '(fill-column 76)
 '(global-emojify-mode t)
 '(global-linum-mode t)
 '(ibuffer-expert t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(ispell-dictionary "british")
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(js-switch-indent-offset 4)
 '(line-number-mode t)
 '(load-prefer-newer t)
 '(mouse-avoidance-mode (quote jump) nil (avoid))
 '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (uptimes w3m slime sass-mode restclient powershell markdown-mode hyde highlight-chars emojify dictionary csharp-mode browse-kill-ring boxquote auto-compile)))
 '(sass-indent-offset 4)
 '(savehist-mode t)
 '(scroll-error-top-bottom t)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh")
 '(truncate-lines t)
 '(uptimes-database "~/.emacs.d/uptimes")
 '(user-mail-address "davep@davep.org"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
