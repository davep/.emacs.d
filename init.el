;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure the package system is up and running early on.
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local config/lib directory support.

(defun davep:user-path (path)
  "Given `file', return a path for it in the local config."
  (concat user-emacs-directory path))

(defvar davep:local (davep:user-path "davep/")
  "My local config and code directory.")

(defvar davep:startup (davep:user-path "davep/startup")
  "My local startup code.")

(defvar davep:lib (davep:user-path "davep/lib")
  "My local library code.")

(defvar davep:lib-3rd-party (davep:user-path "davep/lib-3rd-party")
  "My local third party code.")

(push davep:local         load-path)
(push davep:startup       load-path)
(push davep:lib           load-path)
(push davep:lib-3rd-party load-path)

(defun have-own-package-p (package)
  "Does a package of my own exist in this environment?"
  (locate-library package nil (list davep:lib)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensure custom values go in their own file.
(load (setq custom-file (davep:user-path "custom.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load various startup things.
(require 'davep-env-tests)
(unless noninteractive
  (require 'davep-keys)
  (require 'davep-languages)
  (require 'davep-style)
  (require 'davep-misc)
  (require 'uptimes)
  (require 'csrclr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local autoloading.
(require 'autoloading)
(load-davep-autoloads)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable some disabled commands
(mapc #'(lambda (command)
          (put command 'disabled nil))
      '(upcase-region
        downcase-region
        narrow-to-region
        narrow-to-page
        erase-buffer))
