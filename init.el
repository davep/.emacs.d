;;; init.el --- Entry point of my Emacs configuration.  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; See https://github.com/davep/.emacs.d#readme for details.

;;; Code:

;; Sort out ensuring that some "local" bin directories are in the exec-path.
;; There seems to be an issue on macOS, and on some forms of GNU/Linux,
;; where the PATH isn't inherited if we're run from the dock.
(mapc (lambda (prefix)
        (let ((bin (concat prefix "bin")))
          (when (file-exists-p bin)
            ;; Update Emacs' exec-path.
            (unless (member bin exec-path)
              (push bin exec-path))
            ;; Also ensure PATH for this process matches.
            (unless (string-match-p (regexp-quote bin) (getenv "PATH"))
              (setenv "PATH" (concat (expand-file-name bin) ":" (getenv "PATH")))))))
      '("~/" "~/.local/" "~/.cargo/" "/usr/local/" "~/.local/share/gems/" "/opt/homebrew/" "~/.go/"))

;; early-init.el sets up the load path, and everything else that needs to be
;; in place before any other Lisp is loaded or compiled. Emacs loads it for
;; me when starting normally, but not under -batch, which is how the
;; Makefiles byte-compile things; so pick it up here if it's been skipped.
(unless early-init-file
  (load (expand-file-name "early-init" user-emacs-directory) nil t))

;; Ensure local storage is defined and set up.
(require 'init-local)

;; I know where my local stuff is, so let's get the eln-cache to live there.
(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path (local-emacs-directory "eln-cache")))

;; Configure networking and URL settings before repository operations.
(require 'init-net)

;; Make sure the package system is up and running early on.
;; (package-initialize) happens in here (and it's mentioned here to stop
;; the package system stomping on my init file).
(require 'init-repos)

;; Now that the repos are set up, ensure `use-package' is in play.
(require 'use-package)

;; Ensure my theme of choice is loaded up. I would ideally be loading this
;; inside init-style, but it needs to be available because of custom.el.
(use-package modus-themes :ensure t)

;; Ensure custom values go in their own file.
(load (setq custom-file (local-emacs-directory "custom.el")) t)

;; Now that melpa is sorted and use-package is available, ensure that
;; auto-compile kicks in. I like my ~/.emacs.d/ to be compiled so this is
;; very handy when I've made lots of changes and pulled them down to a
;; machine that had pre-existing elc files that will now be out of date.
(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-save-mode)
  (auto-compile-on-load-mode))

;; is-a.el is a bit special, as I use very early on. Load it now.
(use-package is-a
  :ensure t
  :vc (:url "https://github.com/davep/is-a.el.git" :rev :newest))

;; Load various startup things.
(require 'init-packages)
(unless noninteractive
  (require 'init-editing)
  (require 'init-tweaks)
  (require 'init-keys)
  (require 'init-style))

;; Finally, load any local config. This is for things I don't want to be
;; tracking via my ~/.init.d repo code (things local to a machine, etc).
(load (local-emacs-directory "local-init.el") t)

;;; init.el ends here
