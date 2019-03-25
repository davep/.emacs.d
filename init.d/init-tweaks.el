;;; init-tweaks.el --- Miscellaneous "tweaks" to my emacs environment

;;; Commentary:
;;
;; init-tweaks.el contains miscellaneous tweaks to the Emacs environment.
;; These are the sorts of changes that don't directly relate to packages,
;; etc, and aren't about how Emacs looks.

;;; Code:

(require 'is-a)

;; Ignore case when searching.
(setq-default case-fold-search t)
(setq ido-case-fold case-fold-search)

;; Allow minibufferception.
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; Auto-saves and backing up. Here I try and keep my work directories as
;; clean as possible.
(let ((auto-saves (local-emacs-directory "auto-save-files/")))
  (unless (file-exists-p auto-saves)
    (make-directory auto-saves t))
  (setq auto-save-list-file-prefix     (local-emacs-directory "auto-save-list/saves-")
        auto-save-file-name-transforms `((".*" ,auto-saves t))
        backup-directory-alist         `((".*" . ,(local-emacs-directory "backups")))
        backup-by-copying              t
        version-control                t
        delete-old-versions            t
        kept-new-versions              10
        load-prefer-newer              t))

;; Sort out ensuring that some "local" bin directories are in the exec-path.
;; There seems to be an issue on macOS, and on some forms of GNU/Linux,
;; where the PATH isn't inherited if we're run from the dock.
(when is-a-unix-p
  (mapc (lambda (prefix)
          (let ((bin (concat prefix "bin")))
            (when (file-exists-p bin)
              ;; Update Emacs' exec-path.
              (unless (member bin exec-path)
                (push bin exec-path))
              ;; Also ensure PATH for this process matches.
              (unless (string-match-p (regexp-quote bin) (getenv "PATH"))
                (setenv "PATH" (concat (expand-file-name bin) ":" (getenv "PATH")))))))
        '("~/" "~/.local/" "/usr/local/")))

;; Enable mouse avoidance mode.
(mouse-avoidance-mode 'animate)

;; Enable some disabled commands.
(mapc (lambda (command)
        (put command 'disabled nil))
      '(downcase-region
        erase-buffer
        scroll-left
        narrow-to-page
        narrow-to-region
        upcase-region))

;; Some functions in elisp seem to be missing sensible indent declare forms.
;; Let's fix them.
(mapc (lambda (indent)
        (put (car indent) 'lisp-indent-function (cdr indent)))
      '((with-current-buffer-window . 3)
        (with-temp-buffer-window    . 3)))

;; Ensure that various types of scripts are executable when we create/save
;; them. Saves having to remember to drop to the shell and chmod +x.
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(provide 'init-tweaks)

;;; init-tweaks.el ends here
