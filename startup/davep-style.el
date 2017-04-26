;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window position and size.
(let ((styles
       '(
         ("bellerophon" .
          (lambda ()
            (set-frame-size (selected-frame) 90 50)
            (set-frame-position (selected-frame) 230 60)))
         ("ariel" .
          (lambda ()
            (set-frame-size (selected-frame) 90 50)
            (set-frame-position (selected-frame) 300 80))))))
  (when window-system
    (funcall (or (cdr (assoc (car (split-string (downcase (system-name)) "\\.")) styles))
                 (lambda () nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window titles.
(let ((format (concat invocation-name "@" (system-name) " - [%b]")))
  (setq frame-title-format format
        icon-title-format  format))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Really don't need menus in a terminal.
(unless window-system
  (menu-bar-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default colour scheme.
(load-theme (if window-system 'adwaita 'deeper-blue) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensure line numbers are off in the terminal.
(unless window-system
  (global-linum-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use a powerline-friendly font when running graphically on a Mac.
(when davep:macOS-window-p
  (let ((font "DejaVu Sans Mono for Powerline"))
    (set-face-attribute 'default nil :font font)
    (set-frame-font font nil t)))

(provide 'davep-style)
