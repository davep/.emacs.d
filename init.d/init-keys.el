;;; init-keys.el --- Personal keyboard bindings and keyboard tweaks

;;; Commentary:
;;
;; init-keys.el contains my personal keyboard bindings as well as general
;; keyboard tweaks and OS-specific settings. Any package-specific bindings
;; will be with the relevant `use-package' elsewhere in this config.

;;; Code:

(require 'is-a)
(require 'bind-key)
(require 'package-x)

;; Movement
(bind-keys
 ("M-g"       . goto-line)
 ("M-<left>"  . backward-sexp)
 ("M-<right>" . forward-sexp)
 ("s-<up>"    . moving-backward-page)
 ("s-<down>"  . moving-forward-page)
 ("s-s"       . isearch-forward-symbol-at-point)
 ("C-s-<tab>" . other-window))
(when is-a-linux-p
  (bind-keys
   ("C-s-<right>" . windmove-right)
   ("C-s-<down>"  . windmove-down)
   ("C-s-<left>"  . windmove-left)
   ("C-s-<up>"    . windmove-up)))
(when is-a-macOS-window-p
  (bind-keys
   ("M-s-<right>" . windmove-right)
   ("M-s-<down>"  . windmove-down)
   ("M-s-<left>"  . windmove-left)
   ("M-s-<up>"    . windmove-up)))

;; Editing
(bind-keys
 ("C-c ;"         . comment-or-uncomment-region)
 ("C-<backspace>" . backward-kill-word)
 ("C-c a"         . align))

;; Tools and "apps" and things.
(bind-keys
 ("C-<f6>"      . list-processes)
 ("<f6>"        . bury-buffer)
 ("<f9>"        . compile)
 ("<f12> r"     . ielm)
 ("<f12> h"     . eshell)
 ("<f12> w"     . eww)
 ("<f12> <f12>" . describe-personal-keybindings)
 ("<f12> p l"   . package-list-packages)
 ("<f12> p r"   . package-refresh-contents)
 ("<f12> p t"   . package-lint-current-buffer)
 ("<f12> p u"   . package-upload-buffer)
 ("<f12> ? w"   . woman))

;; Don't do shift-selecting.
(setq shift-select-mode nil)

;; Things specific to macOS.
(when is-a-macOS-window-p
  (set (intern "ns-alternate-modifier") 'super)
  (set (intern "ns-command-modifier")   'meta)
  (bind-key "s-<tab>" #'completion-at-point))

;; Things specific to Windows.
(when is-a-win32-p
  (set (intern "w32-lwindow-modifier") 'meta))

(provide 'init-keys)

;;; init-keys.el ends here
