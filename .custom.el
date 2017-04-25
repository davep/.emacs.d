(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-image-file-mode t)
 '(auto-save-list-file-prefix "~/.emacs.d/.auto-save-list/saves-")
 '(blink-cursor-mode nil)
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
 '(calendar-latitude 55.9)
 '(calendar-location-name "Edinburgh, Scotland")
 '(calendar-longitude -3.2)
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
 '(emojify-emojis-dir "~/.emacs.d/.emojis")
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
 '(global-linum-mode t)
 '(gnus-default-nntp-server "news.eternal-september.org")
 '(gnus-nntp-server "news.eternal-september.org")
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
 '(nsm-save-host-names t)
 '(nsm-settings-file "~/.emacs.d/.network-security.data")
 '(org-agenda-files
   (quote
    ("~/Dropbox/Sync/Org/elisp.org" "~/Dropbox/Sync/Org/inbox.org")))
 '(org-default-notes-file "~/Dropbox/Sync/Org/inbox.org")
 '(org-directory "~/Dropbox/Sync/Org")
 '(org-log-done (quote time))
 '(package-archive-upload-base "~/develop/elisp/delpa/")
 '(package-selected-packages
   (quote
    (google-maps nuke-buffers longmacs magit setup-compile ngn winsplit org-davep icmp-info multiple-cursors xbase davep-org constellations boxquote itch insert become webinfo unbind nukneval moving fscroll binclock csrclr use-package handyurl services protocols obfusurl paren-face parenface uptimes thinks package-lint w3m slime sass-mode restclient powershell markdown-mode hyde highlight-chars emojify dictionary csharp-mode browse-kill-ring auto-compile)))
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
 '(sass-indent-offset 4)
 '(savehist-file "~/.emacs.d/.history.el")
 '(savehist-mode t)
 '(scroll-conservatively 101)
 '(scroll-error-top-bottom t)
 '(sentence-end-double-space nil)
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh")
 '(tramp-persistency-file-name "/Users/davep/.emacs.d/.tramp.el")
 '(truncate-lines t)
 '(url-cache-directory "/Users/davep/.emacs.d/.url/cache")
 '(user-mail-address "davep@davep.org"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(parenthesis ((t (:inherit shadow :foreground "gray63")))))
