;;; init-style.el --- Style Emacs to my taste

;;; Commentary:
;;
;; init-style.el contains code that styles Emacs to my taste. It includes
;; things like the default window size/position, theme choices, etc.

;;; Code:

;; No toolbar. Ever. Just.... no!
(tool-bar-mode -1)

;; Default colour scheme.
(load-theme (if (display-graphic-p) 'adwaita 'deeper-blue) t)

;; Window position and size.
(when (display-graphic-p)
  (let ((short-name (car (split-string (downcase (system-name)) "\\.")))
        (gndn (lambda () nil))
        (styles
         '(
           ("bellerophon" .
            (lambda ()
              (set-frame-size (selected-frame) 90 50)
              (set-frame-position (selected-frame) 230 60)))
           ("ariel" .
            (lambda ()
              (set-frame-size (selected-frame) 90 50)
              (set-frame-position (selected-frame) 300 80))))))
    (funcall (or (cdr (assoc short-name styles)) gndn))))

;; Window titles.
(let ((format (list (user-login-name) "@" (downcase (system-name))
                    " - "
                    "[%b" '(:eval (if (buffer-file-name)
                                      (format " (%s)" (abbreviate-file-name (buffer-file-name)))
                                    "")) "]")))
  (setq frame-title-format format
        icon-title-format  format))

;; Mode line.
(setq display-time-24hr-format t
      display-time-day-and-date t
      display-time-format "%F %H:%M")
(display-time-mode t)
(column-number-mode t)

;; If we're not on a graphical display...
(unless (display-graphic-p)
  ;; ...get rid of the menu bar.
  (menu-bar-mode -1))

;; When we're on a graphical display...
(when (display-graphic-p)
  ;; If we're on Emacs 26 or better...
  (if (> emacs-major-version 25)
      ;; ...use native line numbers. This isn't quite as nice as using
      ;; linum-mode because this turns them on for *all* buffers. But this
      ;; is also me playing with Emacs 26 before it's released, so it helps
      ;; to be a little "over the top". At some point I should set this
      ;; locally by hooks in buffers where line numbers really make sense.
      (setq-default display-line-numbers t)
    ;; ...otherwise use linum-mode.
    (global-linum-mode 1)))

;; Style what happens inside a buffer/frame.
(blink-cursor-mode -1)                  ; Don't blink the cursor.
(setq-default
 ;; Wrap a little further over.
 fill-column 76
 ;; Make empty lines after the last line more obvious.
 indicate-empty-lines t
 ;; No "soft wrapping" of lines, ever.
 truncate-lines t
 ;; Only show a cursor in the focused window
 cursor-in-non-selected-windows nil)

(provide 'init-style)

;;; init-style.el ends here
