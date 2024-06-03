;;; init-tweaks.el --- Miscellaneous "tweaks" to my Emacs environment

;;; Commentary:
;;
;; init-tweaks.el contains miscellaneous tweaks to the Emacs environment.
;; These are the sorts of changes that don't directly relate to packages,
;; etc, and aren't about how Emacs looks.

;;; Code:

(require 'is-a)
(require 'init-local)

;; Ensure that /bin/sh is used when running external stuff. This helps solve
;; issues when I'm using novel shells. More to the point, this gets round a
;; number of issues I had when moving to fish as my shell.
(setq shell-file-name "/bin/sh")

;; Ignore case when searching.
(setq-default case-fold-search t)

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

;; WoMan is my preference for man-page viewing, but it doesn't seem to be
;; doing terribly well on macOS. So, for now...
(when is-a-macOS-p
  (fset 'woman 'man))

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

;; I suspect there's a far better place I can put this, but for now...
;;
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(provide 'init-tweaks)

;;; init-tweaks.el ends here
