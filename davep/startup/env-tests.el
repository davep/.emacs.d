;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For testing our environment...

(defconst win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst linuxp
  (or (eq system-type 'gnu/linux)
      (eq system-type 'linux))
  "Are we running on a GNU/Linux system?")

(defconst linux-x-p
  (and window-system linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst linux-terminal-p
  (and (not window-system) linuxp)
  "Are we running on GNU/Linux, in a terminal?")

(defconst linux-console-p
  (and linux-terminal-p (string= (getenv "TERM") "linux"))
  "Does it look like we're on a Linux console?")

(defconst osx-p
  (eq system-type 'darwin)
  "Are we running on some form of OS X?")

(defconst osx-terminal-p
  (and osx-p (not window-system))
  "Are we running in an OS X terminal?")

(defconst osx-window-p
  (and osx-p (not osx-terminal-p))
  "Are we running in an OS X window?")
