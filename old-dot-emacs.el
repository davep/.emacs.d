;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make use of the Common Lisp compatibility module.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For testing our environment...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst gnu-emacs-p
  (not (null (string-match "GNU Emacs" (emacs-version))))
  "Are we running under GNU emacs?")

(defconst gnu-emacs-20-p
  (and gnu-emacs-p (= emacs-major-version 20))
  "Are we running under GNU emacs 21?")

(defconst gnu-emacs-21-p
  (and gnu-emacs-p (>= emacs-major-version 21))
  "Are we running under GNU emacs 21 (or higher)?")

(defconst gnu-emacs-24-p
  (and gnu-emacs-p (>= emacs-major-version 24))
  "Are we running under GNU emacs 24 (or higher)?")

(defconst x-emacs-p
  (not (null (string-match "Lucid\\|XEmacs" (emacs-version))))
  "Are we running under XEmacs?")

(defconst dosp
  (eq system-type 'ms-dos)
  "Are we running on a DOS system?")

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

(defconst true-window-system-p
  (and window-system (not dosp))
  "Are we running under a real window system?")

(defconst own-lisp-directory
  (if gnu-emacs-p
      (or (getenv "OWN_ELISP") "~davep/lib/elisp/")
    (or (getenv "OWN_XELISP") (if win32p "~/lib/xelisp/" "~davep/lib/xelisp/")))
  "Where does my personal library of lisp code live?")

(defconst 3rd-party-lisp-directory
  (concat own-lisp-directory "3rd-party-lisp/")
  "Where does my personal library of 3rd party code live?")

(defconst email-editor-p
  (not (null (member "post-mode" command-line-args)))
  "Were we called as an email/news editor?")

(defconst rootp
  (and linuxp (zerop (user-uid)))
  "Are we running as root?")

(defun have-own-package-p (package)
  "Does a package of my own exist in this environment?"
  (let ((load-path (list own-lisp-directory)))
    (locate-library package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up path information.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nconc load-path (if linuxp
                     '("/usr/share/emacs/site-lisp" ".")
                   '(".")))
(push own-lisp-directory load-path)     ; Add my own elisp directory.
(push 3rd-party-lisp-directory load-path) ; Added my 3rd party elisp directory.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My local autoload files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar davep-generated-autoload-file (concat own-lisp-directory "davep-autoloads.el")
  "My local autoloads file.")

(defvar davep-generated-3rd-party-autoload-file (concat 3rd-party-lisp-directory "3rd-party-autoloads.el")
  "My local 3rd party lisp autoloads file.")

(defun update-davep-autoloads-core (file dir)
  "Core function for updating local autoload files."
  (let ((generated-autoload-file file))
    (cond (gnu-emacs-p
           (unless (file-exists-p file)
             (with-temp-buffer
               (insert 12)
               (setf (buffer-file-name) file)
               (save-buffer)))
           (require 'autoload)
           (update-autoloads-from-directories dir))
          (x-emacs-p
           (update-autoloads-from-directory dir)))
    (byte-compile-file file)))

(defun update-davep-autoloads ()	
  "Update my local autoloads file."
  (interactive)
  (update-davep-autoloads-core davep-generated-autoload-file own-lisp-directory))

(defun update-davep-3rd-party-autoloads ()
  "Update my local 3rd party lisp autoloads file."
  (interactive)
  (update-davep-autoloads-core davep-generated-3rd-party-autoload-file 3rd-party-lisp-directory))

;; If my autoloads don't exist, create them.
(unless (or rootp (file-exists-p davep-generated-autoload-file))
  (update-davep-autoloads))
(unless (or rootp (file-exists-p davep-generated-3rd-party-autoload-file))
  (update-davep-3rd-party-autoloads))

;; Load them.
(unless rootp
  (load (file-name-sans-extension davep-generated-autoload-file))
  (load (file-name-sans-extension davep-generated-3rd-party-autoload-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What is required for this file?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (have-own-package-p "uptimes")    ; Get uptimes in as early as
  (when x-emacs-p                       ; possible.
    (setq uptimes-auto-save nil
	  uptimes-database "~/.xemacs-uptimes"))
  (require 'uptimes))
(require 'dp-lib)                       ; My own library of functions.
(require 'fscroll)                      ; Full scrolling.
(unless x-emacs-p
  (require 'csrclr))                    ; Cursor colouring.
(when email-editor-p                    ; When doing email or news include my
  (require 'posting))                   ; post-mode config file.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t         ; No startup message.
      case-fold-search nil              ; Case-insensitive searching.
      scroll-step 1                     ; Sane scrolling.
      line-number-mode t                ; Show line numbers.
      column-number-mode t              ; Showcolumn numbers.
      default-major-mode 'text-mode     ; Default to text-mode.
      default-fill-column 76            ; Default fill column.
      next-line-add-newlines nil        ; Don't add newlines for me.
      font-lock-maximum-decoration t    ; Lots of colour.
      sentence-end-double-space nil     ; I don't double space after full stops.
      inhibit-eol-conversion linuxp     ; Let me deal with EOL issues on Linux.
      query-user-mail-address nil       ; Shuts up xemacs' HTML mode.
      mail-host-address "davep.org"     ; Machine for email.
      sawfish-buffer-symbol-lists nil   ; Don't buffer symbols in sawfish-mode.
      sawfish-apropos-searches-info-files nil ; Don't search info during sawfish apropos.
      hscroll-mode-name nil             ; Save some mode line indicator space.
      eldoc-minor-mode-string nil       ; Save some more mode line indicator space.
      diff-switches "-u"                ; How I like to see diffs.
      compilation-scroll-output t       ; Always scroll the output of a compile.
      eshell-prompt-function            ; The prompt to use with eshell.
      (lambda ()
        (concat (user-login-name)
                ":"
                (eshell/pwd)
                (if rootp "#" "$")
                " "))
      eshell-prompt-regexp "^[^#$\n]*[#$] " ; Regexp to match the eshell prompt.
      mc-default-scheme 'mc-scheme-gpg  ; Default mailcrypt scheme.
      mc-gpg-comment nil                ; Use GPG's default comment.
      ngn-newsrc "~/.jnewsrc"           ; Tell ngn-* where to look for group names.
      muttrc-manual-path "/usr/local/doc/mutt/manual.txt" ; Tell muttrc-mode where the mutt manual lives.
      add-log-time-format               ; Date format for ChangeLogs.
      (lambda ()
        (format-time-string "%Y-%m-%d %R UTC%z" (current-time)))
      add-log-current-defun-function    ; Find harbour functions in ChangeLogs.
      #'harbour-aware-add-log-current-defun
      ange-ftp-try-passive-mode t       ; Ensure ange-ftp uses passive mode.
      w3m-home-page "http://hagbard/"   ; Home page for w3m.
      tramp-default-method "ssh")       ; Default method for tramp
(unless gnu-emacs-24-p
  (setq-default enable-multibyte-characters nil)) ; Don't use MULE.
(setq-default indent-tabs-mode nil)     ; Don't use tabs.
(cond (gnu-emacs-p                      ; Highlight brackets and the like.
       (show-paren-mode t))
      (x-emacs-p
       (paren-set-mode 'paren)))
(cond (gnu-emacs-p                     ; Let's be a little European.
       (set-language-environment "Latin-1"))
      (x-emacs-p
       (standard-display-european 1)))
(when (and gnu-emacs-p true-window-system-p)
  (set-scroll-bar-mode 'right))         ; Scroll bars on the right.
(cond (linuxp                           ; When running on Linux.
       (setq inferior-lisp-program "lisp" ; What is lisp?
             scheme-program-name "guile" ; What is scheme?
             ispell-dictionary "british" ; Colour, not color.
             printer-name "lp"          ; Text printer.
             ps-printer-name "lp"       ; Postscript printer.
             ps-print-header nil))      ; Don't print a header.
      (win32p                           ; When running on 95/98/NT/2K.
       (setq inferior-lisp-program "sbcl"
             browse-url-netscape-program "c:/program files/netscape/communicator/program/netscape"
             slashdot-headline-database "//hagbard/davep/.slashdot-headlines"
             quickurl-url-file          "m:/.quickurls"
             ispell-program-name        "aspell" ; Use aspell instead of ispell
             ispell-dictionary "british" ; Colour, not color.
             telcode-host "hagbard"
             garble-program "garble"
             services-file "C:/Windows/system32/drivers/etc/services"
             protocols-file "C:/Windows/system32/drivers/etc/protocol"))
      (dosp                             ; When running on DOS.
       (setq slashdot-headline-database "m:/.slashdot-headlines"
             quickurl-url-file          "m:/.quickurls"
             garble-program "garble")))
(cond ((or
        (and window-system gnu-emacs-p) ; In GNU-emacs under a window system.
        gnu-emacs-21-p)                 ; In GNU-emacs 21, under anything.
       (when true-window-system-p
         (transient-mark-mode 1))       ; Show marks.
       (global-font-lock-mode t)        ; Font-lock everything.
       (when true-window-system-p
         (require 'parenface)))         ; Add my paren face.
      (x-emacs-p                        ; In XEmacs
       (font-lock-mode)                 ; Font-lock everything.
       (require 'parenface)))           ; Add my paren face.
(when linux-x-p                         ; When running on Linux under X.
  (mouse-avoidance-mode 'animate)       ; Get the mouse cursor out of the way.
  (setq tex-dvi-view-command "kdvi"))   ; So I can view .dvi files.
(when x-emacs-p                         ; XEmacs specific bits.
  (setq efs-use-passive-mode t          ; Use passive ftp.
        package-get-remote              ; Where to download packages from.
        '(("sunsite.doc.ic.ac.uk"
           "packages/xemacs/packages"))))
(when gnu-emacs-21-p                    ; GNU emacs 21 specific bits.
  (blink-cursor-mode 0)                 ; Blinking cursor, just say no.
  (tool-bar-mode 0)                     ; Toolbars, just say no.
  (when linux-x-p
    (tooltip-mode 0))                   ; Tooltips, just say no.
  (setq default-indicate-empty-lines t) ; Give me an empty line clue.
  (auto-image-file-mode 1)              ; Display images as images when visiting them.
  (setq debug-on-error nil              ; I'll turn on the debugger when I need it.
        eval-expression-debug-on-error nil)
  (setq-default
   cursor-in-non-selected-windows nil)) ; Don't show the cursor in non-selected windows.

;; Highlight the fact that we're running as root.
(when (and window-system rootp)
  (set-face-background 'modeline "red")
  (set-face-foreground 'modeline "yellow"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs Wiki support.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq emacs-wiki-projects
      `(("Hermes" .
         ((emacs-wiki-directories
           . ("~/develop/hermes/Documentation/Wiki/Source"))
          (emacs-wiki-publishing-directory
           . "~/develop/hermes/Documentation/Wiki/HTML")
          (emacs-wiki-style-sheet
           . "<link rel=\"stylesheet\" type=\"text/css\" href=\"hermes.css\">")
          (emacs-wiki-maintainer . "NotYetWritten.html")))))
(setq emacs-wiki-file-ignore-regexp "\\`\\(\\.?#.*\\|.*,v\\|.*~\\|\\.\\.?\\)\\|.cvsignore\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable some disabled commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'upcase-region    'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'erase-buffer     'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control display of time on the control bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq display-time-24hr-format t)       ; 24hr format.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable partial completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless x-emacs-p
  (if gnu-emacs-24-p
      (setq completion-styles '(partial-completion initials)
            completion-pcm-complete-word-inserts-delimiters t)
    (partial-completion-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control the calendar.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq calendar-location-name "Billingborough, England"
      calendar-latitude  [52 53 north]  ; 52° 53' 41.1" North
      calendar-longitude [0 20 west]    ;  0° 20' 22" West
      diary-file "~/.diary")
(add-hook 'initial-calendar-window-hook #'mark-calendar-holidays)
(add-hook 'initial-calendar-window-hook #'mark-diary-entries)
(add-hook 'diary-display-hook           #'fancy-diary-display)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control ibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ibuffer-expert t)
(add-hook 'ibuffer-mode-hooks
          (lambda () (ibuffer-auto-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc autoloads.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'zenirc            "zenircrc"         "zenirc"               t)
(autoload 'mc-encrypt        "mailcrypt"        "mailcrypt"            t)
(autoload 'mc-encrypt-region "mc-toplev"        "mailcrypt"            t)
(autoload 'mc-decrypt        "mailcrypt"        "mailcrypt"            t)
(autoload 'mc-sign           "mailcrypt"        "mailcrypt"            t)
(autoload 'mc-sign-region    "mc-toplev"        "mailcrypt"            t)
(autoload 'sml-mode          "sml-mode"         "sml-mode"             t)
(autoload 'run-sml           "sml-proc"         "Start an sml session" t)
(autoload 'tuareg-mode       "tuareg"           "OCaml mode"           t)
(autoload 'w3m               "w3m"              "w3m"                  t)
(autoload 'mutt-alias-insert "mutt-alias"       "Insert mutt alias"    t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup horizontal scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond (gnu-emacs-p
       (when gnu-emacs-21-p
         (setq-default truncate-lines t)
         (when (locate-library "hscroll")
           (require 'hscroll)
           (hscroll-global-mode))))
      (x-emacs-p
       (setq-default truncate-lines t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window and icon titles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((format (concat invocation-name "@" (system-name) " - [%b]")))
  (setq frame-title-format format
        icon-title-format  format))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer list trimming.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (or x-emacs-p email-editor-p)
  (require 'midnight)
  (setq clean-buffer-list-delay-special 0)
  (push "*Completions*"         clean-buffer-list-kill-buffer-names)
  (push "*compilation*"         clean-buffer-list-kill-buffer-names)
  (push "*uptimes*"             clean-buffer-list-kill-buffer-names)
  (push "*festival*"            clean-buffer-list-kill-buffer-names)
  (push "*quickurl-urls*"       clean-buffer-list-kill-buffer-names)
  (push "*Process List*"        clean-buffer-list-kill-buffer-names)
  (push "*weights*"             clean-buffer-list-kill-buffer-names)
  (push "*Calendar*"            clean-buffer-list-kill-buffer-names)
  (push "*wm*"                  clean-buffer-list-kill-buffer-names)
  (push "*Holidays*"            clean-buffer-list-kill-buffer-names)
  (push "*Directory*"           clean-buffer-list-kill-buffer-names)
  (push "*SQL*"                 clean-buffer-list-kill-buffer-names)
  (push "*telcode*"             clean-buffer-list-kill-buffer-names)
  (push "*cmulisp*"             clean-buffer-list-kill-buffer-names)
  (push "*Faces*"               clean-buffer-list-kill-buffer-names)
  (push "*tld*"                 clean-buffer-list-kill-buffer-names)
  (push "*Phases of Moon*"      clean-buffer-list-kill-buffer-names)
  (push "*Fancy Diary Entries*" clean-buffer-list-kill-buffer-names)
  (push "*Bookmark List*"       clean-buffer-list-kill-buffer-names)
  (push "*netqueue*"            clean-buffer-list-kill-buffer-names)
  (push "*MailCrypt*"           clean-buffer-list-kill-buffer-names)
  (push "*VC-log*"              clean-buffer-list-kill-buffer-names)
  (push "*icmp list*"           clean-buffer-list-kill-buffer-names)
  (push "*Kill Ring*"           clean-buffer-list-kill-buffer-names)
  (push "*webinfo*"             clean-buffer-list-kill-buffer-names)
  (push "^\\*Garble"            clean-buffer-list-kill-regexps)
  (push "^\\*rcp"               clean-buffer-list-kill-regexps)
  (push "^\\*sawfish"           clean-buffer-list-kill-regexps)
  (push "^\\*Customize"         clean-buffer-list-kill-regexps)
  (push "^\\*Man"               clean-buffer-list-kill-regexps)
  (push "X-Ray\\*$"             clean-buffer-list-kill-regexps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Any other auto-modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless email-editor-p
  (push (cons "\\.Xdefaults"              #'xrdb-mode)       auto-mode-alist)
  (push (cons "\\.cl$"                    #'lisp-mode)       auto-mode-alist)
  (push (cons "ChangeLog$"                #'change-log-mode) auto-mode-alist)
  (push (cons "\\.\\(pas\\|dpr\\|dpk\\)$" #'delphi-mode)     auto-mode-alist)
  (push (cons "\\.php3$"                  #'c-mode)          auto-mode-alist)
  (push (cons "\\.php$"                   #'c-mode)          auto-mode-alist)
  (push (cons "\\.inc$"                   #'c-mode)          auto-mode-alist)
  (push (cons "\\.jl$"                    #'sawfish-mode)    auto-mode-alist)
  (push (cons "\\.sawfishrc$"             #'sawfish-mode)    auto-mode-alist)
  (push (cons "\\.sawfish/rc$"            #'sawfish-mode)    auto-mode-alist)
  (push (cons "\\.coffee"                 #'coffee-mode)     auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; personal keyboard bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map [(control x)
                        (control b)]     #'ibuffer)
(define-key global-map [(meta f6)]       #'ibuffer)
(define-key global-map [(control f6)]    #'list-processes)
(define-key global-map [f6]              #'bury-buffer)
(define-key global-map [f9]              #'compile)
(define-key global-map [f10]             #'slashdot-headlines)
(define-key global-map [(meta g)]        #'goto-line)
(define-key global-map [(alt s)]         #'scratch-buffer)
(define-key global-map [(meta s)]        #'scratch-buffer)
(define-key global-map [(alt l)]         #'ispell-word)
(define-key global-map [(meta left)]     #'backward-sexp)
(define-key global-map [(meta right)]    #'forward-sexp)
(define-key global-map [(meta p)]        #'(lambda ()
                                             (interactive)
                                             (other-window -1)))
(define-key global-map [(meta n)]        #'other-window)
(define-key global-map [(control c) (c)] #'cut-here)
(define-key global-map [(control c)
                        (control i)]     #'cut-file-here)
(define-key global-map [(control
                         return)]        #'split-line-keeping-fill-prefix)
(define-key global-map (kbd "C-c RET")   #'split-line-keeping-fill-prefix)
(define-key global-map [(control c) (e)] #'eshell)
(define-key global-map [(control c) (f)] #'insert-filename)
(define-key global-map [(control c) (l)] #'lbdb-maybe-region)
(define-key global-map [(control c) (u)] #'insert-url)
(define-key global-map [(control c) (n)] #'insert-navigator-url)
(define-key global-map [(control c) (k)] #'browse-kill-ring)
(define-key global-map [(control c) (r)] #'comment-region)
(define-key global-map [(control c)
                        (control t)]     #'thinks-maybe-region)
(define-key global-map [(control c)
                        (control n)]     #'ngn-insert)
(define-key global-map [(f11)]           #'uptimes-this)
(define-key global-map [(control f11)]   #'clean-buffer-list)
(define-key global-map [home]            #'smart-home)
(define-key global-map [end]             #'smart-end)
(define-key global-map [delete]          #'delete-char)
(define-key global-map [(control
                         backspace)]     #'backward-kill-word)
(define-key global-map [(meta
                         backspace)]     #'undo)
(define-key global-map [(meta i)]        #'(lambda (name)
                                             (interactive
                                              (list (read-file-name "File: " "~/lib/boilerplate/" nil t)))
                                             (insert-file name)))
(define-key global-map [(control c)
                        (control f)]     #'view-file)
(when (featurep 'uptimes)
  (define-key global-map [(control c) (t)] #'uptimes))
(when (have-own-package-p "boxquote")
  (define-key global-map [(control c) (i)] #'boxquote-insert-file)
  (define-key global-map [(control c)
                          (meta w)]        #'boxquote-kill-ring-save)
  (define-key global-map [(control c) (y)] #'boxquote-yank)
  (define-key global-map [(control c) (b)] #'boxquote-region)
  (define-key global-map [(control c)
                          (control b)]     #'boxquote-title)
  (define-key global-map [(control c)
                          (control h) (f)] #'boxquote-describe-function)
  (define-key global-map [(control c)
                          (control h) (v)] #'boxquote-describe-variable)
  (define-key global-map [(control c)
                          (control h) (k)] #'boxquote-describe-key)
  (define-key global-map [(control c) (!)] #'boxquote-shell-command)
  (define-key global-map [(control c)
                          (control h) (w)] #'boxquote-where-is))
(when (have-own-package-p "weightrec")
  (define-key global-map [(control c) (w)] #'weight-record)
  (define-key global-map [(control c)
                          (control w)]     #'weight-history))
(when (have-own-package-p "quickurl")
  (define-key global-map [(control c) (control v)] #'quickurl-ask)
  (define-key global-map [(control c) (v)]         #'quickurl)
  (define-key global-map [(control c) (control u)] #'quickurl-add-url)
  (define-key global-map [(f12)]                   #'quickurl-list))
(add-hook 'post-mode-hook
          #'(lambda ()
              ;; post-mode (which inherits from mail-mode) steals C-c C-v
              ;; and C-c C-i, steal them back.
              (define-key post-mode-map [(control c) (control v)] #'quickurl-ask)
              (define-key post-mode-map [(control c) (control i)] #'cut-file-here)
              ;; Also use boxquote's paragraph filler.
              (define-key post-mode-map [(meta q)] #'boxquote-fill-paragraph)))

(defun long-term-emacs ()
  "Turn this emacs session into a long term emacs.

This involves disabling C-x C-c and also starting the GNU server."
  (interactive)
  (require (if (locate-library "gnuserv-compat") 'gnuserv-compat 'gnuserv))
  (gnuserv-start)
  (define-key global-map [(control x) (control c)]
    #'(lambda ()
        (interactive)
        (message "C-x C-c is disabled"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control the display of the menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and gnu-emacs-p (or (not window-system) dosp))
  (menu-bar-mode 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define some handy abbrevs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mapc (lambda (abbrev)
        (define-abbrev global-abbrev-table (car abbrev) (cdr abbrev)))
      '(("toa"   . "The Oasis")
        ("toau"  . "The Oasis <URL:http://www.the-oasis.net/>")
        ("clc"   . "comp.lang.clipper")
        ("clcu"  . "<URL:news:comp.lang.clipper>")
        ("gg"    . "Google Groups")
        ("ggu"   . "Google Groups  <URL:http://groups.google.com/>")
        ("fsfu"  . "Free Software Foundation <URL:http://www.fsf.org/>")
        ("gnuu"  . "GNU project <URL:http://www.gnu.org/>")
        ("ges"   . "gnu.emacs.sources")
        ("gesu"  . "<URL:news:gnu.emacs.sources>")
        ("ucol"  . "uk.comp.os.linux")
        ("ucolu" . "<URL:news:uk.comp.os.linux>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set any system-specific look and feel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when gnu-emacs-p
  (cond ((system-name-p "hagbard.davep.org")
         ;;(set-default-font "-Misc-Fixed-Medium-R-Normal--15-140-75-75-C-90-ISO8859-1")
         ;;(set-default-font "-Adobe-Courier-Medium-R-Normal--12-120-75-75-M-70-ISO8859-1")
         (let ((default-font
                   (if gnu-emacs-21-p
                       ;;"-misc-fixed-medium-r-semicondensed-*-*-120-*-*-c-*-iso8859-9"
                       "-Adobe-Courier-Medium-R-Normal--12-120-75-75-M-70-ISO8859-1"
                     "-Misc-Fixed-Medium-R-Normal--13-120-75-75-C-80-ISO8859-1")))
           (set-default-font default-font)
           (push (cons 'font default-font) default-frame-alist)))
        ((or (system-name-p "DARWIN") (system-name-p "TYCHO") (system-name-p "IAPETUS") (system-name-p "SAGAN"))
         (set-default-font "-*-Courier New-normal-r-*-*-12-120-*-*-c-*-*-ansi-")
         (setq default-frame-alist `((top . 10) (left . 50)
                                     (width . 100) (height . ,(if (system-name-p "SAGAN") 45 60))))
         (setq initial-frame-alist default-frame-alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dictd support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (or linuxp win32p)
  (setq dictionary-server (cond ((system-name-p "sagan.davep.org") "sagan")
                                ((system-name-p "lambda.davep.org") "lambda")
                                ((system-name-p "potato.vegetable.org.uk") "dict.org")
                                (t "hagbard")))
  (autoload 'dictionary-search "dictionary" "Ask for a word and search it in all dictionaries" t)
  (autoload 'dictionary-match-words "dictionary" "Ask for a word a search all matching words in the dictionaries" t)
  (define-key global-map "\C-cs" #'dictionary-search)
  (define-key global-map "\C-cd" #'dictionary-match-words))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Footnote management.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'footnote-mode-hook
          #'(lambda ()
              (setq footnote-style 'numeric-latin
                    footnote-spaced-footnotes nil
                    footnote-section-tag "-----"
                    footnote-section-tag-regexp (regexp-quote footnote-section-tag)
                    footnote-narrow-to-footnotes-when-editing t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create a pseudo mode for editing hash commented files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'hashcmnt-mode 'sh-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create a pseudo mode for editing slrn's score file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'score-mode 'sh-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use cperl-mode instead of perl-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defalias 'perl-mode 'cperl-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'java-mode-hook
          #'(lambda ()
              (c-set-style "Java")
              (setq c-basic-offset 4)
              (c-set-offset 'substatement-open 0)
              (c-set-offset 'case-label '+)
              (define-key java-mode-map "\C-m" #'newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Until there's a csharp-mode kicking about, use java-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'csharp-mode 'java-mode)
(push (cons "\\.cs$" 'csharp-mode) auto-mode-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; text authoring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'text-mode-hook #'(lambda () (auto-fill-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; awk programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'awk-mode-hook
          #'(lambda ()
              (c-set-style "BSD")
              (define-key c-mode-map "\C-m" #'newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SML programming.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(push (cons "\\.sml$" 'sml-mode) auto-mode-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OCaml programming.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(push (cons "\\.ml\\w?" 'tuareg-mode) auto-mode-alist)
(push (cons "ocaml" 'tuareg-mode) interpreter-mode-alist)
(add-hook 'tuareg-mode-hook
          #'(lambda ()
              (define-key tuareg-mode-map "\C-m" #'newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(push (cons "\\.py$"  'python-mode) auto-mode-alist)
(push (cons "\\.pyw$" 'python-mode) auto-mode-alist)
(push (cons "python"  'python-mode) interpreter-mode-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dylan programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(push (cons "\\.dylan$" 'dylan-mode) auto-mode-alist)
(add-hook 'dylan-mode-hook
          #'(lambda ()
              (define-key dylan-mode-map "\C-m" #'newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(push (cons "\\.rb$" 'ruby-mode) auto-mode-alist)
(push (cons "\\.builder$" 'ruby-mode) auto-mode-alist)
(push (cons "ruby" 'ruby-mode) interpreter-mode-alist)
(push (cons "\\.rhtml$" 'html-mode) auto-mode-alist)
(push (cons "\\.html\\.erb$" 'html-mode) auto-mode-alist)
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          #'(lambda ()
              (define-key ruby-mode-map "\C-m" #'newline-and-indent)
              (inf-ruby-keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c-mode-hook
          #'(lambda ()
              (c-set-style "BSD")
              (setq c-basic-offset 4)
              (when (in-harbour-dir-p)  ; harbour uses indent 3 for C source.
                (setq c-basic-offset 3))
              (c-set-offset 'case-label '+)
              (setup-compile "gcc -Wall -O2")
              (define-key c-mode-map "\C-m" #'newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c++-mode-hook
          #'(lambda ()
              (c-set-style "BSD")
              (setq c-basic-offset 4)
              (c-set-offset 'case-label '+)
              (c-set-offset 'inline-open 0)
              (c-set-offset 'access-label '-)
              (c-set-offset 'inclass '++)
              (setup-compile (concat (if dosp "gxx" "g++") " -Wall -O2"))
              (define-key c++-mode-map "\C-m" #'newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pascal programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'pascal-mode-hook
          #'(lambda ()
              (setq pascal-indent-level 2)
              (setq pascal-auto-newline t)
              (setq pascal-tab-always-indent t)
              (define-key pascal-mode-map "\C-m" #'newline-and-indent)))
(add-hook 'delphi-mode-hook
          #'(lambda ()
              (setq delphi-indent-level 2)
              (setq delphi-tab-always-indents t)
              (define-key delphi-mode-map "\C-m" #'newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clipper programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(push (cons "\\.\\(prg\\|ch\\|ppo\\)$" 'xbase-mode) auto-mode-alist)
(add-hook 'xbase-mode-hook
          #'(lambda ()
              ;; I like to use just one RETURN in a function and I like it
              ;; to match up with the defun.
              (xbase-add-rule 'xbase-return "^[\t ]*return" 'xbase-defun nil t 0)
              ;; Auto indentation.
              (define-key xbase-mode-map "\C-m" #'reindent-then-newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COBOL programming (just for mucking about, really, honest!)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(push (cons "\\.\\(cob\\|cobol\\)$" 'cobol-mode) auto-mode-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smalltalk programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(push (cons "\\.st$" 'smalltalk-mode) auto-mode-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML authoring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'html-mode-hook
          #'(lambda ()
              (set (make-local-variable 'quickurl-format-function)
                   #'(lambda (url)
                       (format "<A HREF=\"%s\">%s</A>"
                               (quickurl-url-url url)
                               (quickurl-url-description url))))))
(push (cons "\\.css$" 'css-mode) auto-mode-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go programming.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'go-mode-hook
          #'(lambda ()
              (setq tab-width 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CoffeeScript programming.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'coffee-mode-hook
          #'(lambda ()
              (setq tab-width 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Acquire some WinDOS oriented modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and gnu-emacs-p win32p)
  (require 'generic-x)
  (push (cons "\\.\\(btm\\|cmd\\)$" 'bat-generic-mode) auto-mode-alist)
  (push (cons "\\.vbs$"  'visual-basic-mode) auto-mode-alist)
  (push (cons "\\.irbf$" 'visual-basic-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(push (cons "\\.js$" 'javascript-mode) auto-mode-alist)
(push (cons "\\.json$" 'javascript-mode) auto-mode-alist)
(add-hook 'javascript-mode-hook
          #'(lambda ()
              (define-key javascript-mode-map "\C-m" #'newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(push (if win32p "d:/usr/slime-2.0" "/usr/local/src/slime") load-path)
(autoload 'slime "slime" "SLIME" t)
(autoload 'slime-mode "slime" "SLIME" t)
(autoload 'slime-connect "slime" "SLIME" t)
(autoload 'inferior-slime-mode "slime" "SLIME" t)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; .asd files are lisp files.
(push (cons "\\.asd$" 'lisp-mode) auto-mode-alist)

(defun lisp-modes-indents ()
  ;; The following apply to various lisp modes, do them all here and don't
  ;; worry about it.
  (put 'if                      'common-lisp-indent-function 2)
  (put 'with-open-file          'lisp-indent-function 1)
  (put 'print-unreadable-object 'lisp-indent-function 1)
  (put 'with-output-to-string   'lisp-indent-function 1)
  (put 'easy-menu-define        'lisp-indent-function 3)
  (put 'eval-when-compile       'lisp-indent-function 0)
  (put 'eval-and-compile        'lisp-indent-function 0)
  (put 'with-temp-buffer        'lisp-indent-function 0)
  (put 'define-derived-mode     'lisp-indent-function 3)
  (put 'with-nntp-client        'lisp-indent-function 1)
  (put 'with-text-face          'lisp-indent-function 1)
  (put 'with-drawing-options    'lisp-indent-function 1)
  (put 'with-output-as-presentation 'lisp-indent-function 1)
  (put 'with-reconnect          'lisp-indent-function 1)
  (put 'with-standard-page      'lisp-indent-function 1)
  (put 'handler-case            'lisp-indent-function 1))

(defun dp-emacs-lisp-hook ()
  ;; Indent using Common Lisp rules (make things like `flet' indent
  ;; correctly).
  (set (make-local-variable 'lisp-indent-function) 'common-lisp-indent-function)
  ;; Handle any other indents.
  (lisp-modes-indents)
  ;; Think for me...
  (daves-generic-keys)
  ;; Tell me what to think...
  (unless email-editor-p
    (turn-on-eldoc-mode)))

(add-hook 'emacs-lisp-mode-hook       #'dp-emacs-lisp-hook)
(add-hook 'lisp-interaction-mode-hook #'dp-emacs-lisp-hook)

(defun scheme-modes-indents ()
  ;; For bigloo
  (put 'module 'lisp-indent-function 1)
  ;; For chicken
  (put 'let-values 'scheme-indent-function 1))

(add-hook 'scheme-mode-hook #'scheme-modes-indents)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other modes with simple hooks.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'lisp-mode-hook   #'daves-generic-keys)
(add-hook 'lisp-mode-hook   #'lisp-modes-indents)
(add-hook 'scheme-mode-hook #'daves-generic-keys)
(add-hook 'python-mode-hook #'daves-generic-keys)
(add-hook 'perl-mode-hook   #'daves-generic-keys)
(add-hook 'cperl-mode-hook  #'daves-generic-keys)
(when linux-x-p
  (add-hook 'vc-log-mode-hook #'(lambda () (flyspell-mode 1))))
(add-hook 'message-mode-hook #'(lambda() (flyspell-mode 1)))
(add-hook 'text-mode-hook #'(lambda() (flyspell-mode 1)))

(defun daves-generic-keys ()
  "davep: Set up my generic key mappings"
  (define-key (current-local-map) "\C-m" #'newline-and-indent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add support for other interpreters.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(push (cons "clocc" 'lisp-mode)  interpreter-mode-alist)
(push (cons "scsh" 'scheme-mode) interpreter-mode-alist)

;;; ~/.emacs ends here.
