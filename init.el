;;; init.el --- Entry point of my Emacs configuration.

;;; Commentary:
;;
;; See https://github.com/davep/.emacs.d#readme for details.

;;; Code:

;; Don't let gc slow things down as we start up.
(let ((gc-cons-threshold most-positive-fixnum))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Add my local init directory to the load path.
  (push (expand-file-name "init.d/" user-emacs-directory) load-path)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Ensure local storage is defined and set up.
  (require 'init-local)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Ensure custom values go in their own file.
  (load (setq custom-file (local-emacs-directory "custom.el")) t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Make sure the package system is up and running early on.
  ;; (package-initialize) happens in here (and it's mentioned here to stop the
  ;; package system stomping on my init file).
  (require 'init-repos)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Now that melpa is sorted and use-package is available, ensure that
  ;; auto-compile kicks in. I like my ~/.emacs.d/ to be compiled so this is
  ;; very handy when I've made lots of changes and pulled them down to a
  ;; machine that had pre-existing elc files that will now be out of date.
  (use-package auto-compile
    :ensure t
    :config
    (auto-compile-on-save-mode)
    (auto-compile-on-load-mode))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; is-a.el is a bit special, as I use very early on. Load it now.
  (use-package is-a :ensure t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Load various startup things.
  (unless noninteractive
    (require 'init-editing)
    (require 'init-tweaks)
    (require 'init-keys)
    (require 'init-style))
  (require 'init-net)
  (require 'init-packages))

;;; init.el ends here
