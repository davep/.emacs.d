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
(require 'use-package)

;; Movement
(bind-keys
 ("M-<left>"  . backward-sexp)
 ("M-<right>" . forward-sexp)
 ("M-<up>"    . scroll-down-line)
 ("M-<down>"  . scroll-up-line)
 ("s-s"       . isearch-forward-symbol-at-point)
 ("C-s-<tab>" . other-window))

;; Also movement: set up all the faster goto-line keys.
(use-package goto-line-faster :ensure t)

;; Window swapping key combinations. The choices here reflect the fact that
;; I spend most of my time in a GUI Emacs on macOS. I double up here because
;; sometimes I'm using a full keyboard, sometimes I might be using a smaller
;; keyboard (small Magic keyboard, or a Macbook keyboard), and these
;; combinations seem to fall into just the right spot for muscle memory.
(bind-keys
 ("M-s-<right>" . windmove-right)       ; Nicer on small Magic keyboard.
 ("M-s-<down>"  . windmove-down)
 ("M-s-<left>"  . windmove-left)
 ("M-s-<up>"    . windmove-up)
 ("C-S-<right>" . windmove-right)       ; Nicer on full-size keyboard.
 ("C-S-<down>"  . windmove-down)
 ("C-S-<left>"  . windmove-left)
 ("C-S-<up>"    . windmove-up))

;; Editing
(bind-keys
 ("C-c ;"         . comment-or-uncomment-region)
 ("C-<backspace>" . backward-kill-word)
 ("s-<backspace>" . backward-kill-word)
 ("C-c a"         . align)
 ("s-<tab>"       . completion-at-point))

;; Tools and "apps" and things.
(bind-keys
 ("M-RET"         . toggle-frame-fullscreen)
 ("C-<f6>"        . list-processes)
 ("<f6>"          . bury-buffer)
 ("<f9>"          . compile)
 ("<f12> r"       . ielm)
 ("<f12> h"       . eshell)
 ("<f12> w"       . eww)
 ("<f12> <f12>"   . describe-personal-keybindings)
 ("<f12> C-<f12>" . describe-bindings)
 ("<f12> p l"     . package-list-packages)
 ("<f12> p r"     . package-refresh-contents)
 ("<f12> p t"     . package-lint-current-buffer)
 ("<f12> p u"     . package-upload-buffer)
 ("<f12> ? w"     . woman)
 ("<f12> f x"     . xref-find-references)
 ("C-x C-9"       . (lambda () (interactive) (text-scale-set 3))))

;; Don't do shift-selecting.
(setq shift-select-mode nil)

;; Things specific to macOS.
(when is-a-macOS-window-p
  ;; Have "option" be "super".
  (set (intern "mac-option-modifier") 'super)
  ;; Have "cmd" be "meta"
  (set (intern "mac-command-modifier") 'meta)
  ;; Have left "cmd" be "hyper"
  (set (intern "mac-left-command-modifier") 'hyper))

;; Things specific to Windows.
(when is-a-win32-p
  (set (intern "w32-lwindow-modifier") 'meta))

;; Give https://www.masteringemacs.org/article/text-expansion-hippie-expand
;; a try. I've been using dabbrev-expand since forever, and never thought to
;; look at this.
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(provide 'init-keys)

;;; init-keys.el ends here
