(use-package recentf
  :commands recentf-save-list
  :custom
  (recentf-save-file (local-emacs-directory "recentf"))
  :config
  (add-to-list 'recentf-exclude (rx ".local/share/emacs/"))
  :init
  ;; Auto-save the list every 30 minutes.
  (run-at-time nil (* 30 60)
               (lambda ()
                 (let ((inhibit-message t))
                   (recentf-save-list)))))
