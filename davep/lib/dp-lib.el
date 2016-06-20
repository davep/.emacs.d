;;; Collection of handy functions for emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff that's required.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (require 'cl)
  (unless (string-match "Lucid\\|XEmacs" (emacs-version))
    (require 'thingatpt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following are defined in ~/.emacs (or should be). To keep the
;; compiler quiet I'll defvar them here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (defvar davep:linux-x-p)
  (defvar davep:win32p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Are we running on a given machine?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun system-name-p (name)
  "davep: Is the `system-name' the same as NAME?"
  (string-equal (upcase name) (upcase (system-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Are we being displayed on a given display?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-name-p (name)
  "davep: Are we running on the given display?"
  (let* ((display-values
          (split-string
           (if (boundp 'x-display-name) (symbol-value 'x-display-name) "")
           "[:]"))
         (display (when (= (length display-values) 2) (car display-values))))
    (when display
      (string= (upcase display) (upcase name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup a default compile command for a buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (defvar compile-command))

(defun setup-compile (default-command)
  "davep: Setup the compile command for a buffer"
  (interactive "sDefault compile command: \n")
  (or (file-exists-p "GNUmakefile")
      (file-exists-p "makefile")
      (file-exists-p "Makefile")
      (progn (make-local-variable 'compile-command)
             (setq compile-command
                   (concat default-command " " buffer-file-name
                           " -o " (file-name-sans-extension
                                   (file-name-nondirectory (buffer-file-name))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quickly switch to the scratch buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scratch-buffer ()
  "davep: Quickly switch to the *scratch* buffer"
  (interactive)
  (switch-to-buffer "*scratch*")
  (lisp-interaction-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find a file and insert its name into the buffer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-filename (file)
  "davep: Insert a filename allowing for interactive browsing to the name"
  (interactive "fFile: ")
  (insert file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert the current buffer's filename into the buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-buffer-filename (&optional name-only)
  "davep: Insert the filename of the current buffer.

NAME-ONLY is a prefix argument, nil means insert the full name of the file,
any other value means insert the name without the directory."
  (interactive "P")
  (let ((filename (buffer-file-name)))
    (if (null filename)
        (error "Buffer has no filename")
      (insert (if name-only
                  (file-name-nondirectory filename)
                filename)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run something in another frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-in-new-frame (exp)
  "davep: Create another frame (or split window when frames are not
avaialble) and funcall exp."
  (if (or davep:linux-x-p davep:win32p)
      (select-frame (make-frame '((minibuffer . nil))))
    (split-window)
    (other-window 1))
  (funcall exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String slicing and dicing functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-left (str &optional n)
  "davep: Return the n leftmost characters of a string"
  (substring str 0 (if (null n) 1 n)))

(defun string-right (str &optional n)
  "davep: Return the n rightmost characters of a string"
  (substring str (- (length str) (if (null n) 1 n))))

(defun string-reverse (str)
  "davep: Reverse a string"
  (cl-coerce (reverse (cl-coerce str 'list)) 'string))

(defun string-padl (str width &optional char)
  "davep: Left pad a string to a specified length"
  (if (< (length str) width)
      (concat (make-string (- width (length str))
                           (if (null char) 32 char))
              str)
    str))

(defun string-padr (str width &optional char)
  "davep: Right pad a string to a specified length"
  (if (< (length str) width)
      (concat str (make-string (- width (length str))
                               (if (null char) 32 char)))
    str))

(defun string-padc (str width &optional char)
  "davep: Pad center a string to a specified length"
  (if (< (length str) width)
      (let* ((char (if (null char) 32 char))
             (str (concat (make-string (/ (- width (length str)) 2) char) str)))
        (string-padr str width char))
    str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert a decimal value to a binary string.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun to-binary (num &optional pad)
  "davep: Convert a decimal value to a binary string"
  (interactive "nValue: \nnBits: ")
  (let ((binary (cl-coerce
                 (nreverse
                  (loop for bit = 1 then (ash bit 1)
                        until (> bit num)
                        collect (if (zerop (logand num bit)) ?0 ?1)))
                 'string)))
    (when pad
      (setq binary (string-padl binary pad ?0)))
    (when (called-interactively-p 'any)
      (message "%dd == %sb" num binary))
    binary))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert a binary string to a decimal value.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun from-binary (binary)
  "davep: Convert a binary string to a decimal value"
  (interactive "sBinary: ")
  (let ((decimal
         (let ((binary (nreverse (cl-coerce binary 'list))))
           (loop for bit in binary
                 for bitv = 1 then (ash bitv 1)
                 when (= bit ?1) sum bitv))))
    (when (called-interactively-p 'any)
      (message "%sb == %dd" binary decimal))
    decimal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert a list of decimal values to binary strings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dec-list-to-binary (&rest dec-list)
  "davep: Convert a list of decimal values to binary strings"
  (mapcar #'to-binary dec-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert a list of binary strings to decimal values.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bin-list-to-decimal (&rest bin-list)
  "davep: Convert a list of binary strings to decimal values"
  (mapcar #'from-binary bin-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert a hex string to a decimal value.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun from-hex (hex)
  "davep: Convert from hex to decimal."
  (interactive "sHex: ")
  (let ((n (read (concat "?\\x" hex))))
    (when (called-interactively-p 'any)
      (message "Hex: %s Decimal: %d" hex n))
    n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert a decimal value to a hex string.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun to-hex (n)
  "davep: Convert from decimal to hex."
  (interactive "nDecimal: ")
  (let ((hex (format "%x" n)))
    (when (called-interactively-p 'any)
      (message "Decimal: %d Hex: %s" n hex))
    hex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Return the emacs version as a floating point number.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dp-emacs-version ()
  "davep: Return emacs version number as a float value"
  (string-match "\\([0-9]+\\.[0-9]+\\)" (emacs-version))
  (string-to-number (match-string 0 (emacs-version))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide a wrapper around my sndplay utility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sndplay-library "/usr/local/lib/sounds"
  "*Location of the sound library.")

(defun sndplay-sounds ()
  "davep: Compile a list of sounds for use with `completing-read'."
  (loop for sound in (directory-files sndplay-library nil "[^.].*")
        collect (cons (file-name-sans-extension sound) sound)))
  
(defun sndplay (sound)
  "davep: elisp wrapper for sndplay"
  (interactive (list (completing-read "Sound: " (sndplay-sounds))))
  (call-process "sndplay" nil 0 nil sound))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find out who called us (and who called them, and who...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun funcname (&optional n)
  "davep: Return the name of the function at stack level N.

N is optional and if not supplied defaults to 0. (this is designed to work
just like Clipper's procname() function)."
  (cadr (backtrace-frame (+ (or n 0) 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `perldoc', just like `man'. Came from Kai.Grossjohann
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun perldoc (args)
  "davep: Fire up perldoc."
  (interactive "sPerlDoc: ")
  (require 'man)
  (let ((manual-program "perldoc"))
    (manual-entry args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Work out a numeric suffix.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun numeric-suffix (n)
  "davep: Return the numeric suffix for N."
  (let ((n (abs n)))
    (if (let ((n (mod n 100))) (and (> n 9) (< n 20)))
        "th"
      (case (mod n 10)
        (1 "st")
        (2 "nd")
        (3 "rd")
        (t "th")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A function to help remember how to compose things.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun help-compose ()
  "davep: Show compose instructions for ISO-8859-1."
  (interactive)
  (view-file "/usr/X11R6/lib/X11/locale/iso8859-1/Compose"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handy for making emacs look nicer on the console
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun invert-all-uninverted-faces ()
  "davep: Invert all faces that haven't yet been inverted."
  (interactive)
  (mapc #'(lambda (face)
            (unless (get face 'invert-all-uninverted-faces-inverted)
              (invert-face face)
              (put face 'invert-all-uninverted-faces-inverted t)))
        (face-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for messing with codings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun become-dos-buffer ()
  "Turn the current buffer into a DOS buffer."
  (interactive)
  (set-buffer-file-coding-system 'dos))

(defun become-unix-buffer ()
  "Turn the current buffer into a Unix buffer."
  (interactive)
  (set-buffer-file-coding-system 'unix))

(provide 'dp-lib)
