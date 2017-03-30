;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal keyboard bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map [(control x)
                        (control b)]     #'ibuffer)
(define-key global-map [(meta f6)]       #'ibuffer)
(define-key global-map [(control f6)]    #'list-processes)
(define-key global-map [f6]              #'bury-buffer)
(define-key global-map [f9]              #'compile)
(define-key global-map [(meta g)]        #'goto-line)
(define-key global-map [(alt l)]         #'ispell-word)
(define-key global-map [(meta left)]     #'backward-sexp)
(define-key global-map [(meta right)]    #'forward-sexp)
(define-key global-map [(meta p)]        #'(lambda ()
                                             (interactive)
                                             (other-window -1)))
(define-key global-map [(meta n)]        #'other-window)
(define-key global-map [(control c) (e)] #'eshell)
(define-key global-map [(control c) (k)] #'browse-kill-ring)
(define-key global-map [(control c) (o)] #'(lambda ()
                                             (interactive)
                                             (require 'org)
                                             (find-file (concat org-directory "/inbox.org"))))
(define-key global-map [(control c)
                        (control o)]     #'(lambda ()
                                             (interactive)
                                             (require 'org)
                                             (find-file org-directory)))
(define-key global-map [(control c) (r)] #'comment-region)
(define-key global-map [(control f11)]   #'clean-buffer-list)
(define-key global-map [delete]          #'delete-char)
(define-key global-map [(control
                         backspace)]     #'backward-kill-word)
(define-key global-map [(meta
                         backspace)]     #'undo)
(define-key global-map [(control z)]     #'undo)
(define-key global-map [(control c)
                        (control f)]     #'view-file)
(define-key global-map [(control c) (a)] #'org-agenda)
(when davep:osx-window-p
  (define-key global-map [(super tab)] #'completion-at-point))
(when davep:win32p
  (setq w32-lwindow-modifier 'meta))

(provide 'davep-keys)
