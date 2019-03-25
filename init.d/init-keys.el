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
(bind-key "s-s"       #'isearch-forward-symbol-at-point)
(bind-key "C-s-<tab>" #'other-window)
(when is-a-linux-p
  (bind-key "C-s-<right>" #'other-window)
  (bind-key "C-s-<down>" #'other-window)
  (bind-key "C-s-<left>" (lambda ()
                           (interactive) (other-window -1 )))
  (bind-key "C-s-<up>" (lambda ()
                         (interactive) (other-window -1 )))
  (bind-key "M-s-<right>" #'other-frame)
  (bind-key "M-s-<left>" (lambda ()
                           (interactive) (other-frame -1))))
(when is-a-macOS-window-p
  (bind-key "M-s-<right>" #'other-window)
  (bind-key "M-s-<down>" #'other-window)
  (bind-key "M-s-<left>" (lambda ()
                           (interactive) (other-window -1 )))
  (bind-key "M-s-<up>" (lambda ()
                         (interactive) (other-window -1 ))))

;; Editing
(bind-key "C-c ;"         #'comment-or-uncomment-region)
(bind-key "C-<backspace>" #'backward-kill-word)
(bind-key "C-c a"         #'align)

;; Tools and "apps" and things.
(bind-key "C-<f6>"      #'list-processes)
(bind-key "<f6>"        #'bury-buffer)
(bind-key "<f9>"        #'compile)
(bind-key "<f12> r"     #'ielm)
(bind-key "<f12> h"     #'eshell)
(bind-key "<f12> w"     #'eww)
(bind-key "<f12> <f12>" #'describe-personal-keybindings)
(bind-key "<f12> p l"   #'package-list-packages)
(bind-key "<f12> p r"   #'package-refresh-contents)
(bind-key "<f12> p t"   #'package-lint-current-buffer)
(bind-key "<f12> p u"   #'package-upload-buffer)
(bind-key "<f12> ? w"   #'woman)

;; Don't do shift-selecting.
(setq shift-select-mode nil)

;; Things specific to macOS.
(when is-a-macOS-window-p
  (set (intern "ns-alternate-modifier") 'super)
  (set (intern "ns-command-modifier")   'meta)
  (bind-key "s-<tab>" #'completion-at-point))

;; Things specific to Windows.
(when is-a-win32-p
  (set (intern "w32-lwindow-modifier") 'meta))

(provide 'init-keys)

;;; init-keys.el ends here
