;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personal keyboard bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'bind-key)

;; General bindinds
(bind-key "C-x C-b"             #'ibuffer)
(bind-key [(meta f6)]           #'ibuffer)
(bind-key [(control f6)]        #'list-processes)
(bind-key [f6]                  #'bury-buffer)
(bind-key [f9]                  #'compile)
(bind-key "M-g"                 #'goto-line)
(bind-key [(meta left)]         #'backward-sexp)
(bind-key [(meta right)]        #'forward-sexp)
(bind-key "C-c e"               #'eshell)
(bind-key [(control f11)]       #'clean-buffer-list)
(bind-key "C-c r"               #'comment-region)
(bind-key "C-c C-f"             #'view-file)
(bind-key "M-p"                 #'(lambda () (interactive) (other-window -1)))
(bind-key "M-n"                 #'other-window)
(bind-key "C-c k"               #'browse-kill-ring)
(bind-key [delete]              #'delete-char)
(bind-key [(control backspace)] #'backward-kill-word)
(bind-key [(meta backspace)]    #'undo)
(bind-key "C-z"                 #'undo)
(bind-key "C-c o"               #'(lambda () (interactive) (require 'org) (find-file (concat org-directory "/inbox.org"))))
(bind-key "C-c C-o"             #'(lambda () (interactive) (require 'org) (find-file org-directory)))
(bind-key "C-c a"               #'org-agenda)

;; Things specific to macOS.
(when davep:osx-window-p
  (define-key global-map [(super tab)] #'completion-at-point))

;; Things specific to Windows.
(when davep:win32p
  (set 'w32-lwindow-modifier 'meta))

(provide 'davep-keys)
