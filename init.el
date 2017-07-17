;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add my local init directory to the load path.
(push (concat user-emacs-directory "init.d/") load-path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up things for the local Emacs directory.
(defconst local-emacs-directory "~/.local/share/emacs/"
  "Directory where local Emacs content lives.

This is the directory where files I don't want to sync via
github, and which I don't want to lose if I erase my ~/.emacs.d/
directory, should live.")

(defun local-emacs-directory (content)
  "Return the local Emacs directory path for CONTENT.

A side-effect of calling this function is that it ensures the
directory exist and, if it doesn't, it creates it."
  (unless (file-exists-p local-emacs-directory)
    (make-directory local-emacs-directory t))
  (concat local-emacs-directory content))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensure custom values go in their own file.
(load (setq custom-file (local-emacs-directory "custom.el")) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure the package system is up and running early on.
;; (package-initialize) happens in here (and it's mentioned here to stop the
;; package system stomping on my init file).
(require 'init-repos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Now that melpa is sorted and use-package is available, ensure that
;; auto-compile kicks in. I like my ~/.emacs.d/ to be compiled so this is
;; very handy when I've made lots of changes and pulled them down to a
;; machine that had pre-existing elc files that will now be out of date.
(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-save-mode)
  (auto-compile-on-load-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; is-a.el is a bit special, as I use very early on. Load it now.
(use-package is-a :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load various startup things.
(unless noninteractive
  (require 'init-editing)
  (require 'init-tweaks)
  (require 'init-keys)
  (require 'init-style))
(require 'init-net)
(require 'init-packages)

;;; init.el ends here
