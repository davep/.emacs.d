;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make use of the Common Lisp compatibility module.
(require 'cl)

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

(push davep:local   load-path)
(push davep:startup load-path)
(push davep:lib     load-path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensure custom values go in their own file.
(setq custom-file (davep:user-path "custom.el"))
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load various startup things.
(load "env-tests")
(require 'uptimes)
(require 'csrclr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local autoloading.
(require 'autoloading)
(load-davep-autoloads)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language styles.

(add-hook 'c-mode-hook			; C
          #'(lambda ()
              (c-set-style "BSD")
              (setq c-basic-offset 4)
              (c-set-offset 'case-label '+)
              (setup-compile "gcc -Wall -O2")
              (define-key c-mode-map "\C-m" #'newline-and-indent)))

