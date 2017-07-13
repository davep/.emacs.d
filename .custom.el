(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-file-name "~/.emacs.d/.abbrev_defs")
 '(auto-image-file-mode t)
 '(auto-save-list-file-prefix "~/.emacs.d/.auto-save-list/saves-")
 '(backup-by-copying t)
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/.backups"))))
 '(bookmark-default-file "~/.emacs.d/.bookmarks.el")
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
 '(electric-pair-mode t)
 '(eww-bookmarks-directory "~/.emacs.d/.eww")
 '(ibuffer-expert t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message ";; Get shit done!

")
 '(ispell-dictionary "british")
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(js-switch-indent-offset 4)
 '(kept-new-versions 10)
 '(load-prefer-newer t)
 '(mouse-avoidance-mode (quote jump) nil (avoid))
 '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta))
 '(nsm-save-host-names t)
 '(nsm-settings-file "~/.emacs.d/.network-security.data")
 '(oauth2-token-file "~/.emacs.d/.oauth2.plstore")
 '(org-agenda-files
   (quote
    ("~/Dropbox/Sync/Org/elisp.org" "~/Dropbox/Sync/Org/inbox.org")))
 '(org-default-notes-file "~/Dropbox/Sync/Org/inbox.org")
 '(org-directory "~/Dropbox/Sync/Org")
 '(org-log-done (quote time))
 '(package-archive-upload-base "~/develop/elisp/delpa/")
 '(package-selected-packages
   (quote
    (graburl numbers suggest minimap dad-joke wttrin beacon qrencode cheat-sh page-break-lines company slstats expando is-a msig signature smartsig macdob powerline google-contacts google-maps nuke-buffers longmacs magit setup-compile ngn winsplit org-davep icmp-info multiple-cursors xbase davep-org constellations boxquote itch insert become webinfo unbind nukneval moving fscroll binclock csrclr use-package handyurl services protocols obfusurl paren-face parenface uptimes thinks package-lint slime sass-mode restclient powershell markdown-mode hyde highlight-chars emojify dictionary browse-kill-ring auto-compile)))
 '(package-user-dir "~/.emacs.d/.packages")
 '(pascal-auto-newline t)
 '(pascal-indent-level 2)
 '(pascal-tab-always-indent t)
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
 '(savehist-file "~/.emacs.d/.history.el")
 '(savehist-mode t)
 '(scroll-conservatively 101)
 '(scroll-error-top-bottom t)
 '(send-mail-function (quote smtpmail-send-it))
 '(sentence-end-double-space nil)
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587)
 '(url-cache-directory "~/.emacs.d/.url/cache")
 '(url-configuration-directory "~/.emacs.d/.url/")
 '(url-cookie-file "/Users/davep/.emacs.d/.url/cookies")
 '(user-mail-address "davep@davep.org")
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
