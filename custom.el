(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-image-file-mode t)
 '(blink-cursor-mode nil)
 '(case-fold-search t)
 '(clean-buffer-list-delay-general 1)
 '(clean-buffer-list-delay-special 1)
 '(clean-buffer-list-kill-buffer-names
   (quote
    ("*Help*" "*Apropos*" "*Buffer List*" "*Compile-Log*" "*info*" "*vc*" "*vc-diff*" "*diff*" "*uptimes*" "*markdown-output*" "*Checkdoc Status*" "*Compile-Log*")))
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
 '(initial-scratch-message ";; Get shit done!

")
 '(ispell-dictionary "british")
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(js-switch-indent-offset 4)
 '(line-number-mode t)
 '(load-prefer-newer t)
 '(mouse-avoidance-mode (quote jump) nil (avoid))
 '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta))
 '(org-agenda-files
   (quote
    ("~/Dropbox/Sync/Org/elisp.org" "~/Dropbox/Sync/Org/inbox.org")))
 '(org-default-notes-file "~/Dropbox/Sync/Org/inbox.org")
 '(org-directory "~/Dropbox/Sync/Org")
 '(org-log-done (quote time))
 '(package-archive-upload-base "~/develop/elisp/delpa/")
 '(package-selected-packages
   (quote
    (webinfo unbind nukneval moving fscroll binclock csrclr use-package handyurl services protocols obfusurl paren-face parenface uptimes thinks package-lint w3m slime sass-mode restclient powershell markdown-mode hyde highlight-chars emojify dictionary csharp-mode browse-kill-ring auto-compile)))
 '(safe-local-variable-values
   (quote
    ((eval when
           (and
            (buffer-file-name)
            (file-regular-p
             (buffer-file-name))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (featurep
                (quote package-build))
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require
                (quote package-build))))
           (package-build-minor-mode)
           (set
            (make-local-variable
             (quote package-build-working-dir))
            (expand-file-name "../working/"))
           (set
            (make-local-variable
             (quote package-build-archive-dir))
            (expand-file-name "../packages/"))
           (set
            (make-local-variable
             (quote package-build-recipes-dir))
            default-directory)))))
 '(sass-indent-offset 4)
 '(savehist-mode t)
 '(scroll-error-top-bottom t)
 '(sentence-end-double-space nil)
 '(shift-select-mode nil)
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
 '(parenthesis ((t (:inherit shadow :foreground "gray63")))))
