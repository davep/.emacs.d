;;; davep-keys.el --- Personal keyboard bindings

;;; Commentary:
;;
;; davep-keys.el contains my personal keyboard bindings.

;;; Code:

(require 'is-a)
(require 'bind-key)

;; General bindinds
(bind-key [(control f6)]        #'list-processes)
(bind-key [f6]                  #'bury-buffer)
(bind-key [f9]                  #'compile)
(bind-key "M-g"                 #'goto-line)
(bind-key [(meta left)]         #'backward-sexp)
(bind-key [(meta right)]        #'forward-sexp)
(bind-key "C-c e"               #'eshell)
(bind-key [(control f11)]       #'clean-buffer-list)
(bind-key "C-c ;"               #'comment-or-uncomment-region)
(bind-key "C-c C-f"             #'view-file)
(bind-key "M-p"                 (lambda () (interactive) (other-window -1)))
(bind-key "M-n"                 #'other-window)
(bind-key [delete]              #'delete-char)
(bind-key [(control backspace)] #'backward-kill-word)
(bind-key [(meta backspace)]    #'undo)
(bind-key "C-z"                 #'undo)
(bind-key "s-r"                 #'ielm)

;; Things specific to macOS.
(when is-a-macOS-window-p
  (bind-key [(super tab)] #'completion-at-point))

;; Things specific to Windows.
(when is-a-win32-p
  (set (intern "w32-lwindow-modifier") 'meta))

(provide 'davep-keys)

;;; davep-keys.el ends here
