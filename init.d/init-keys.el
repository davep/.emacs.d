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
(bind-key "M-g"       #'goto-line)
(bind-key "M-<left>"  #'backward-sexp)
(bind-key "M-<right>" #'forward-sexp)
(bind-key "s-<up>"    #'moving-backward-page)
(bind-key "s-<down>"  #'moving-forward-page)

;; Editing
(bind-key "C-c ;"         #'comment-or-uncomment-region)
(bind-key "C-<backspace>" #'backward-kill-word)

;; Tools and "apps" and things.
(bind-key [(control f6)]  #'list-processes)
(bind-key [f6]            #'bury-buffer)
(bind-key [f7]            #'describe-personal-keybindings)
(bind-key [f9]            #'compile)
(bind-key [(f12) (h)]     #'eshell)
(bind-key "C-c C-f"       #'view-file)
(bind-key [(f12) (r)]     #'ielm)
(bind-key "C-c p l"       #'package-list-packages)
(bind-key "C-c p r"       #'package-refresh-contents)
(bind-key "C-c p p"       #'package-lint-current-buffer)
(bind-key "C-c p u"       #'package-upload-buffer)
(bind-key [(f12) (w)]     #'eww)

;; Don't do shift-selecting.
(setq shift-select-mode nil)

;; Things specific to macOS.
(when is-a-macOS-window-p
  (set (intern "ns-alternate-modifier") 'super)
  (set (intern "ns-command-modifier")   'meta)
  (bind-key [(super tab)] #'completion-at-point))

;; Things specific to Windows.
(when is-a-win32-p
  (set (intern "w32-lwindow-modifier") 'meta))

(provide 'init-keys)

;;; init-keys.el ends here
