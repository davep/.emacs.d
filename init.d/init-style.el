;;; init-style.el --- Style Emacs to my taste

;;; Commentary:
;;
;; init-style.el contains code that styles Emacs to my taste. It includes
;; things like the default window size/position, theme choices, etc.

;;; Code:

(require 'init-local)

;; Default colour scheme.
(load-theme (if (display-graphic-p) 'adwaita 'deeper-blue) t)

;; Ensure I have the same base font no matter which macOS Emacs I'm using.
(when is-a-macOS-window-p
  (add-to-list 'default-frame-alist '(font . "menlo")))

;; Have comments always be in italic.
(set-face-attribute 'font-lock-comment-face nil :italic t)

;; First off, let's tweak what the final booted version of Emacs looks like
;; when the loading of the init is over.
(setq inhibit-startup-screen t
      initial-scratch-message
      (format ";; Get %s done!\n\n"
              ;; Ensure my standard scratch buffer entry isn't quite so
              ;; "crude" when I'm at work. ;)
              (if (file-exists-p (local-emacs-directory ".nsfw"))
                  "stuff"
                "shit")))

;; No toolbar. Ever. Just.... no!
(tool-bar-mode -1)

;; Let's not bother with the menu either, except if we're on a Mac, where it
;; costs nothing to have one.
(unless is-a-macOS-window-p
  (menu-bar-mode -1))

;; Window titles.
(let ((format (list (user-login-name) "@" (downcase (system-name))
                    " - "
                    "[%b" '(:eval (if (buffer-file-name)
                                      (format " (%s)" (abbreviate-file-name (buffer-file-name)))
                                    "")) "]")))
  (setq frame-title-format format
        icon-title-format  format))

;; Mode line.
(when is-a-unix-terminal-p
  (require 'time)
  (set (intern "display-time-24hr-format") t)
  (set (intern "display-time-day-and-date") t)
  (set (intern "display-time-format") "%F %H:%M")
  (display-time-mode 1))
(column-number-mode t)

;; When we're on a graphical display...
(when (display-graphic-p)
  ;; Always visit images as images.
  (auto-image-file-mode)
  ;; If we're on Emacs 26 or better...
  (if (> emacs-major-version 25)
      ;; ...only show line numbers in certain types of modes.
      (progn
        (add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))
        (add-hook 'text-mode-hook (lambda () (display-line-numbers-mode 1)))
        (add-hook 'restclient-mode-hook (lambda () (display-line-numbers-mode 1))))
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
 cursor-in-non-selected-windows nil
 ;; Sensible scrolling.
 scroll-conservatively 101
 ;; Don't error just because we're near the bounds of a buffer.
 scroll-error-top-bottom t)

(provide 'init-style)

;;; init-style.el ends here
