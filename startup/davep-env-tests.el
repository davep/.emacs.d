;;; davep-env-tests.el --- Tools for testing the environment we're running in

;;; Commentary:
;;
;; davep-env-tests.el provides tools for testing the environment we're
;; runnuing in. This would normally be loaded pretty early on in
;; ~/.emacs.d/init.el so that other startup code can make decisions about
;; what to load and how to configure what it loads.

;;; Code:

(defconst davep:gnu-emacs-p
  (not (null (string-match "GNU Emacs" (emacs-version))))
  "Are we running under GNU Emacs?")

(defconst davep:x-emacs-p
  (not (null (string-match "Lucid\\|XEmacs" (emacs-version))))
  "Are we running under XEmacs?")

(defconst davep:win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst davep:linuxp
  (or (eq system-type 'gnu/linux)
      (eq system-type 'linux))
  "Are we running on a GNU/Linux system?")

(defconst davep:linux-x-p
  (and window-system davep:linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst davep:linux-terminal-p
  (and (not window-system) davep:linuxp)
  "Are we running on GNU/Linux, in a terminal?")

(defconst davep:linux-console-p
  (and davep:linux-terminal-p (string= (getenv "TERM") "linux"))
  "Does it look like we're on a Linux console?")

(defconst davep:macOS-p
  (eq system-type 'darwin)
  "Are we running on some form of macOS?")

(defconst davep:macOS-terminal-p
  (and davep:macOS-p (not window-system))
  "Are we running in an macOS terminal?")

(defconst davep:macOS-window-p
  (and davep:macOS-p (not davep:macOS-terminal-p))
  "Are we running in an macOS window?")

(defconst davep:unixp
  (or davep:linuxp davep:macOS-p)
  "Are we on some form of Unix?")

(defconst davep:unix-window-p
  (and davep:unixp window-system)
  "Are we on some form of Unix and in a graphical environment?")

(defconst davep:unix-terminal-p
  (and davep:unixp (not window-system))
  "Are we on some form of Unix but not running as a graphical app?")

(defconst davep:rootp
  (and davep:unixp (zerop (user-uid)))
  "Are we running as root?")

(provide 'davep-env-tests)

;;; davep-env-tests.el ends here
