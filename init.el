;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure the package system is up and running early on.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("delpa" . "http://blog.davep.org/delpa/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local config/lib directory support.

(defun davep:user-path (path)
  "Given `file', return a path for it in the local config."
  (concat user-emacs-directory path))

(defvar davep:local (davep:user-path "davep/")
  "My local config and code directory.")

(defvar davep:startup (davep:user-path "davep/startup")
  "My local startup code.")

(push davep:local         load-path)
(push davep:startup       load-path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensure custom values go in their own file.
(load (setq custom-file (davep:user-path "custom.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load up the env tests.
(require 'davep-env-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On the Mac local bin doesn't seem to be in the path if I run from
;; the dock. Fix this.
(let ((local "/usr/local/bin"))
  (when (and davep:osx-p (not (member local exec-path)))
    (push local exec-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If we're on a Unix of some sort, add a local bin (if it's there).
(when (and davep:unixp (file-exists-p "~/bin"))
  (push "~/bin/" exec-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load various startup things.
(unless noninteractive
  (require 'davep-packages)
  (require 'davep-keys)
  (require 'davep-languages)
  (require 'davep-style)
  (require 'davep-misc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable some disabled commands
(mapc #'(lambda (command)
          (put command 'disabled nil))
      '(downcase-region
        erase-buffer
        narrow-to-page
        narrow-to-region
        upcase-region))
