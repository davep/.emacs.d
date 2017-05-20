;;; init-keys.el --- Personal keyboard bindings

;;; Commentary:
;;
;; init-keys.el contains my personal keyboard bindings.

;;; Code:

(require 'is-a)
(require 'bind-key)

;; Movement
(bind-key "M-g"          #'goto-line)
(bind-key [(meta left)]  #'backward-sexp)
(bind-key [(meta right)] #'forward-sexp)

;; Editing
(bind-key "C-c ;"               #'comment-or-uncomment-region)
(bind-key [delete]              #'delete-char)
(bind-key [(control backspace)] #'backward-kill-word)

;; Tools and "apps" and things.
(bind-key [(control f6)]  #'list-processes)
(bind-key [f6]            #'bury-buffer)
(bind-key [f7]            #'describe-personal-keybindings)
(bind-key [f9]            #'compile)
(bind-key [(control f11)] #'clean-buffer-list)
(bind-key "C-c e"         #'eshell)
(bind-key "C-c C-f"       #'view-file)
(bind-key "s-r"           #'ielm)

;; Things specific to macOS.
(when is-a-macOS-window-p
  (bind-key [(super tab)] #'completion-at-point))

;; Things specific to Windows.
(when is-a-win32-p
  (set (intern "w32-lwindow-modifier") 'meta))

(provide 'init-keys)

;;; init-keys.el ends here
