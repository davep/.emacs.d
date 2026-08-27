;;; early-init.el --- Early initialisation.  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; early-init.el is loaded before the package system is brought up and
;; before init.el, so it's where anything that needs to be in effect before
;; any other Lisp is loaded or compiled has to go.

;;; Code:

;; Make my local init directory available, and with it the local storage
;; locations.
(push (expand-file-name "init.d/" user-emacs-directory) load-path)
(require 'init-local)

;; Say where packages live before Emacs gets a chance to go looking in the
;; default location; startup does this before init.el is loaded.
(setq package-user-dir      (local-emacs-directory "packages")
      package-gnupghome-dir (local-emacs-directory "gnupg"))

;; init-repos.el calls `package-initialize' itself. Left to its own devices
;; startup would now activate everything first, given the above, and that
;; work would just be done twice; it also wouldn't happen at all under
;; -batch, so doing it by hand keeps both paths the same.
(setq package-enable-at-startup nil)

;; Everything in init.d/packages.d/ is a `use-package' declaration, and
;; `use-package' does its installing at *byte-compile* time. Asynchronous
;; native-compilation workers are "emacs -Q" subprocesses: they read neither
;; this file nor init.el, so when one of them compiles a declaration it
;; reinstalls the package into the stock ~/.emacs.d/elpa. There's nothing to
;; be gained from natively compiling a pile of `use-package' forms anyway, so
;; keep them out of the JIT compiler's hands.
(setq native-comp-jit-compilation-deny-list
      (cons (concat "\\`" (regexp-quote
                           (expand-file-name "init.d/" user-emacs-directory)))
            (bound-and-true-p native-comp-jit-compilation-deny-list)))

;; Belt and braces: tell any async compilation worker that does get started
;; where my packages live, so it finds them already installed rather than
;; building a fresh elpa directory of its own.
(setq native-comp-async-env-modifier-form
      `(setq package-user-dir      ,package-user-dir
             package-gnupghome-dir ,package-gnupghome-dir))

;;; early-init.el ends here
