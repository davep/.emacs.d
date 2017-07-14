(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-list-file-prefix "~/.emacs.d/.auto-save-list/saves-")
 '(backup-by-copying t)
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/.backups"))))
 '(c-basic-offset 4)
 '(c-default-style
   (quote
    ((c-mode . "BSD")
     (c++-mode . "BSD")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(c-offsets-alist (quote ((inline-open . 0) (case-label . +) (inclass . ++))))
 '(case-fold-search t)
 '(delete-old-versions t)
 '(diff-switches "-u")
 '(dired-use-ls-dired nil)
 '(ibuffer-expert t)
 '(js-switch-indent-offset 4)
 '(kept-new-versions 10)
 '(load-prefer-newer t)
 '(package-selected-packages
   (quote
    (graburl numbers suggest minimap dad-joke wttrin beacon qrencode cheat-sh page-break-lines company slstats expando is-a msig signature smartsig macdob powerline google-contacts google-maps nuke-buffers longmacs magit setup-compile ngn winsplit org-davep icmp-info multiple-cursors xbase davep-org constellations boxquote itch insert become webinfo unbind nukneval moving fscroll binclock csrclr use-package handyurl services protocols obfusurl paren-face parenface uptimes thinks package-lint slime sass-mode restclient powershell markdown-mode hyde highlight-chars emojify dictionary browse-kill-ring auto-compile)))
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
 '(version-control t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eshell-prompt ((t (:foreground "indian red" :weight bold))))
 '(parenthesis ((t (:inherit shadow :foreground "gray63"))))
 '(powerline-active1 ((t (:inherit mode-line :background "grey95"))))
 '(powerline-active2 ((t (:inherit mode-line :background "grey85"))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "grey30"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "grey20")))))
