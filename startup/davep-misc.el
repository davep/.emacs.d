;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make it harder to kill an emacs server instance.
(defun long-term-emacs ()
  "Turn this emacs sesssion into a long term emacs.

This involves disabling C-x C-c and also calling `server-start'."
  (interactive)
  (server-start)
  (define-key global-map [(control x) (control c)]
    #'(lambda ()
        (interactive)
        (message "C-x C-c is disabled"))))

(provide 'davep-misc)
