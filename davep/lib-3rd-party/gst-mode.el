;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 1988-92, 1994-95, 1999, 2000 Free Software Foundation, Inc.
;;; Written by Steve Byrne.
;;;
;;; This file is part of GNU Smalltalk.
;;;
;;; GNU Smalltalk is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation; either version 2, or (at your option) any later 
;;; version.
;;;
;;; GNU Smalltalk is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;; for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with GNU Smalltalk; see the file COPYING.  If not, write to the Free
;;; Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'smalltalk-mode)

;;; Incorporates Frank Caggiano's changes for Emacs 19.

(defvar smalltalk-prompt-pattern "^st> *"
  "Regexp to match prompts in smalltalk buffer.")

(defvar *gst-process* nil
  "Holds the GNU Smalltalk process")
(defvar  gst-args '("-Vp")
  "Arguments to pass to GNU Smalltalk")

(defvar smalltalk-classes nil 
  "The set of class names...used for completion")

(defvar smalltalk-command-string nil
  "Non nil means that we're accumulating output from Smalltalk")

(defvar smalltalk-eval-data nil
  "?")

(defvar smalltalk-ctl-t-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-d" 'smalltalk-toggle-decl-tracing)
    (define-key keymap "\C-e" 'smalltalk-toggle-exec-tracing)
    (define-key keymap "\C-v" 'smalltalk-toggle-verbose-exec-tracing))
  "Keymap of subcommands of C-c C-t, tracing related commands")

(defvar smalltalk-interactor-mode-map
  (let ((keymap (copy-keymap smalltalk-mode-map)))
    (define-key keymap "\C-m" 'comint-send-input)
    (define-key keymap "\C-c\C-d" 'comint-delchar-or-maybe-eof)
    (define-key keymap "\C-c\C-u" 'comint-kill-input)
    (define-key keymap "\C-c\C-c" 'comint-interrupt-subjob)
    (define-key keymap "\C-c\C-z" 'comint-stop-subjob)
    (define-key keymap "\C-c\C-\\" 'comint-quit--subjob)
    (define-key keymap "\C-c\C-o" 'comint-kill-output)
    (define-key keymap "\C-c\C-r" 'comint-show-output) 
    keymap)
  "Keymap used in Smalltalk interactor mode.")

;;;###autoload
(defun gst (args)
  "Invoke GNU Smalltalk"
  (interactive (list (if (null current-prefix-arg)
			 gst-args
		       (read-smalltalk-args))))
  (setq gst-args args)
  (if (not (eq major-mode 'gst-mode))
      (switch-to-buffer-other-window
       (apply 'make-gst "gst" gst-args))
    ;; invoked from a Smalltalk interactor window, so stay there
    (apply 'make-gst "gst" gst-args))
  (setq *smalltalk-process* (get-buffer-process (current-buffer))))

(defun read-smalltalk-args ()
  "Reads the arguments to pass to Smalltalk as a string, returns a list."
  (let (str args args-str result-args start end)
    (setq args gst-args)
    (setq args-str "")
    (while args
      (setq args-str (concat args-str " " (car args)))
      (setq args (cdr args)))
    (setq str (read-string "Invoke Smalltalk: " args-str))
    (while (setq start (string-match "[^ ]" str))
      (setq end (or (string-match " " str start) (length str)))
      (setq result-args (cons (substring str start end) result-args))
      (setq str (substring str end)))
    (reverse result-args)))

(defun make-gst (name &rest switches)
  (let ((buffer (get-buffer-create (concat "*" name "*")))
	proc status size)
    (setq proc (get-buffer-process buffer))
    (if proc (setq status (process-status proc)))
    (save-excursion
      (set-buffer buffer)
      ;;    (setq size (buffer-size))
      (if (memq status '(run stop))
	  nil
	(if proc (delete-process proc))
	(setq proc (apply  'start-process
			   name buffer
			   "env"
			   ;; I'm choosing to leave these here
			   ;;"-"
			   (format "TERMCAP=emacs:co#%d:tc=unknown:"
				   (frame-width))
			   "TERM=emacs"
			   "EMACS=t"
			   "gst"
			   switches))
	(setq name (process-name proc)))
      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
      (set-process-filter proc 'gst-filter)
      (gst-mode))
    buffer))

(defun gst-filter (process string)
  "Make sure that the window continues to show the most recently output
text."
  (let (where ch command-str)
    (setq where 0)			;fake to get through the gate
    (while (and string where)
      (if smalltalk-command-string
	  (setq string (smalltalk-accum-command string)))
      (if (and string
	       (setq where (string-match "\C-a\\|\C-b" string)))
	  (progn
	    (setq ch (aref string where))
	    (cond ((= ch ?\C-a)		;strip these out
		   (setq string (concat (substring string 0 where)
					(substring string (1+ where)))))
		  ((= ch ?\C-b)		;start of command
		   (setq smalltalk-command-string "") ;start this off
		   (setq string (substring string (1+ where))))))))
    (save-excursion
      (set-buffer (process-buffer process))
      (goto-char (point-max))
      (and string
	   (setq mode-status "idle")
	   (insert string))
      (if (process-mark process)
	  (set-marker (process-mark process) (point-max)))))
  ;;  (if (eq (process-buffer process)
  ;;	  (current-buffer))
  ;;      (goto-char (point-max)))
					;  (save-excursion
					;      (set-buffer (process-buffer process))
					;      (goto-char (point-max))
  ;;      (set-window-dot (get-buffer-window (current-buffer)) (point-max))
					;      (sit-for 0))
  (let ((buf (current-buffer)))
    (set-buffer (process-buffer process))
    (goto-char (point-max)) (sit-for 0)
    (set-window-dot (get-buffer-window (current-buffer)) (point-max))
    (set-buffer buf)))

(defun smalltalk-accum-command (string)
  (let (where)
    (setq where (string-match "\C-a" string))
    (setq smalltalk-command-string
	  (concat smalltalk-command-string (substring string 0 where)))
    (if where
	(progn
	  (unwind-protect		;found the delimiter...do it
	      (smalltalk-handle-command smalltalk-command-string)
	    (setq smalltalk-command-string nil))
	  ;; return the remainder
	  (substring string where))
      ;; we ate it all and didn't do anything with it
      nil)))

(defun smalltalk-handle-command (str)
  (eval (read str)))

;;;###autoload
(defun gst-mode ()
  "Major mode for interacting Smalltalk subprocesses.

The following commands imitate the usual Unix interrupt and
editing control characters:
\\{smalltalk-mode-map}

Entry to this mode calls the value of gst-mode-hook with no arguments,
if that value is non-nil.  Likewise with the value of shell-mode-hook.
gst-mode-hook is called after shell-mode-hook."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'gst-mode)
  (setq mode-name "GST")
  (setq mode-line-format
	'("" mode-line-modified mode-line-buffer-identification "   "
	  global-mode-string "   %[(" mode-name ": " mode-status
	  "%n" mode-line-process ")%]----" (-3 . "%p") "-%-"))

  (require 'comint)
  (comint-mode)
  (setq comint-prompt-regexp smalltalk-prompt-pattern)
  (use-local-map comint-mode-map)
  (make-local-variable 'mode-status)
  (make-local-variable 'smalltalk-command-string)
  (setq smalltalk-command-string nil)
  (setq mode-status "starting-up")
  (run-hooks 'comint-mode-hook 'gst-mode-hook))


(defun smalltalk-eval-region (start end &optional label)
  "Evaluate START to END as a Smalltalk expression in Smalltalk window.
If the expression does not end with an exclamation point, one will be
added (at no charge)."
  (interactive "r")
  (let (str filename line pos)
    (setq str (buffer-substring start end))
    (save-excursion
      (save-restriction 
	(goto-char (max start end))
	(smalltalk-backward-whitespace)
	(if (/= (preceding-char) ?!)	;canonicalize
	    (setq str (concat str "!")))
	;; unrelated, but reusing save-excursion
	(goto-char (min start end))
	(setq pos (point))
	(setq filename (buffer-file-name))
	(widen)
	(setq line (1+ (count-lines 1 (point))))))
    (send-to-smalltalk str (or label "eval")
		       (list line filename pos))))

(defun smalltalk-reeval-region (remember)
  (interactive "P")
  (and remember
       (let (rgn start end)
	 (setq rgn (smalltalk-bound-expr))
	 (setq start (car rgn)
	       end (cdr rgn))
	 (setq smalltalk-eval-data
	     (smalltalk-get-eval-region-data start end "re-doIt"))))
  (apply 'send-to-smalltalk smalltalk-eval-data))

(defun smalltalk-get-eval-region-data (start end &optional label)
  (interactive "r")
  (let (str filename line pos)
    (setq str (buffer-substring start end))
    (save-excursion
      (save-restriction 
	(goto-char (max start end))
	(smalltalk-backward-whitespace)
	(if (/= (preceding-char) ?!)	;canonicalize
	    (setq str (concat str "!")))
	;; unrelated, but reusing save-excursion
	(goto-char (min start end))
	(setq pos (point))
	(setq filename (buffer-file-name))
	(widen)
	(setq line (1+ (count-lines 1 (point))))))
    ;; certainly not perfect, should probably use markers to bound the region
    (list str (or label "eval")
	  (list line filename pos))))

(defun smalltalk-eval-region-with-memory (start end &optional label)
  "Evaluate START to END as a Smalltalk expression in Smalltalk window.
If the expression does not end with an exclamation point, one will be
added (at no charge)."
  (interactive "r")

;  (let (str filename line pos)
;    (setq str (buffer-substring start end))
;    (save-excursion
;      (save-restriction 
;	(goto-char (max start end))
;	(smalltalk-backward-whitespace)
;	(if (/= (preceding-char) ?!)	;canonicalize
;	    (setq str (concat str "!")))
;	;; unrelated, but reusing save-excursion
;	(goto-char (min start end))
;	(setq pos (point))
;	(setq filename (buffer-file-name))
;	(widen)
;	(setq line (1+ (count-lines 1 (point))))
;	)
;      )
;    ;; certainly not perfect, should probably use markers to bound the region
;    (setq smalltalk-eval-data
;	  (list str (or label "eval")
;		(list line filename pos)))
  (setq smalltalk-eval-data (smalltalk-get-eval-region-data start end label))
  (smalltalk-reeval-region 't)) ;)

(defun smalltalk-doit (use-region)
  (interactive "P")
  (let (start end rgn)
    (if use-region
	(progn
	  (setq start (min (mark) (point)))
	  (setq end (max (mark) (point))))
      (setq rgn (smalltalk-bound-expr))
      (setq start (car rgn)
	    end (cdr rgn)))
    (smalltalk-eval-region start end "doIt")))

(defun smalltalk-bound-expr ()
  "Returns a cons of the region of the buffer that contains a smalltalk expression.
It's pretty dumb right now...looks for a line that starts with ! at the end and
a non-white-space line at the beginning, but this should handle the typical
cases nicely."
  (let (start end here)
    (save-excursion
      (setq here (point))
      (re-search-forward "^!")
      (setq end (point))
      (beginning-of-line)
      (if (looking-at "^[^ \t\"]")
	  (progn
	    (goto-char here)
	    (re-search-backward "^[^ \t\"]")
	    (while (looking-at "^$") ;this is a hack to get around a bug
	      (re-search-backward "^[^ \t\"]")))) ;with GNU Emacs's regexp system
      (setq start (point))
      (cons start end))))

(defun smalltalk-compile (use-region)
  (interactive "P")
  (let (str start end rgn filename line pos header classname category)
    (if use-region
	(progn
	  (setq start (min (point) (mark)))
	  (setq end (max (point) (mark)))
	  (setq str (buffer-substring start end))
	  (save-excursion
	    (goto-char end)
	    (smalltalk-backward-whitespace)
	    (if (/= (preceding-char) ?!) ;canonicalize
		(setq str (concat str "!"))))
	  (send-to-smalltalk str "compile"))
      (setq rgn (smalltalk-bound-method))
      (setq str (buffer-substring (car rgn) (cdr rgn)))
      (setq filename (buffer-file-name))
      (setq pos (car rgn))
      (save-excursion
	(save-restriction
	  (widen)
	  (setq line (1+ (count-lines 1 (car rgn))))))
      (if (buffer-file-name)
	  (progn 
	    (save-excursion
	      (re-search-backward "^![ \t]*[A-Za-z]")
	      (setq start (point))
	      (forward-char 1)
	      (search-forward "!")
	      (setq end (point))
	      (setq line (- line (1- (count-lines start end))))
	      ;; extra -1 here to compensate for emacs positions being 1 based,
	      ;; and smalltalk's (really ftell & friends) being 0 based.
	      (setq pos (- pos (- end start) 1)))
	    (setq str (concat (buffer-substring start end) "\n\n" str "!"))
	    (send-to-smalltalk str "compile"
		       ;-2 accounts for num lines and num chars extra
			       (list (- line 2) filename (- pos 2))))
	(save-excursion
	  (re-search-backward "^!\\(.*\\) methodsFor: \\(.*\\)!")
	  (setq classname (buffer-substring
			   (match-beginning 1) (match-end 1)))
	  (setq category (buffer-substring
			  (match-beginning 2) (match-end 2)))
	  (goto-char (match-end 0))
	  (setq str (smalltalk-quote-strings str))
	  (setq str (format "%s compile: '%s' classified: %s!\n"
			    classname (substring str 0 -1) category))
	  (save-excursion (set-buffer (get-buffer-create "junk"))
			  (erase-buffer)
			  (insert str))
	  (send-to-smalltalk str "compile"
			     (list line nil 0)))))))

(defun smalltalk-bound-method ()
  (let (start end)
    (save-excursion
      (re-search-forward "^!")
      (setq end (point)))
    (save-excursion
      (re-search-backward "^[^ \t\"]")
      (while (looking-at "^$")		  ;this is a hack to get around a bug
	(re-search-backward "^[^ \t\"]")) ;with GNU Emacs's regexp system
      (setq start (point)))
    (cons start end)))

(defun smalltalk-quote-strings (str)
  (let (new-str)
    (save-excursion
      (set-buffer (get-buffer-create " st-dummy "))
      (erase-buffer)
      (insert str)
      (goto-char 1)
      (while (and (not (eobp))
		  (search-forward "'" nil 'to-end))
	(insert "'"))
      (buffer-string))))

(defun smalltalk-snapshot (&optional snapshot-name)
  (interactive (if current-prefix-arg
		   (list (setq snapshot-name 
			       (expand-file-name 
				(read-file-name "Snapshot to: "))))))
  (if snapshot-name
      (send-to-smalltalk (format "ObjectMemory snapshot: '%s'!" "Snapshot"))
  (send-to-smalltalk "ObjectMemory snapshot!" "Snapshot")))

(defun smalltalk-print (start end)
  "Evaluate the expression delimited by START and END and print the result.
Interactively, the region is used.  Printing is done in the standard Smalltalk
output window."
  (interactive "r")
  (let (str)
    (setq str (buffer-substring start end))
    (save-excursion
      (goto-char (max start end))
      (smalltalk-backward-whitespace)
      (if (= (preceding-char) ?!)	;canonicalize
	  (setq str (buffer-substring (min start end)  (point))))
      (setq str (format "(%s) printNl!" str))
      (send-to-smalltalk str "print"))))

(defun smalltalk-quit ()
  "Terminate the Smalltalk session and associated process.  Emacs remains
running."
  (interactive)
  (send-to-smalltalk "ObjectMemory quit!" "Quitting"))

(defun smalltalk-filein (filename)
  "Do a FileStream>>fileIn: on FILENAME."
  (interactive "fSmalltalk file to load: ")
  (send-to-smalltalk (format "FileStream fileIn: '%s'!"
			     (expand-file-name filename))
		     "fileIn"))


(defun smalltalk-toggle-decl-tracing ()
  (interactive)
  (send-to-smalltalk
   "Smalltalk declarationTrace:
     Smalltalk declarationTrace not!"))

(defun smalltalk-toggle-exec-tracing ()
  (interactive)
  (send-to-smalltalk 
   "Smalltalk executionTrace: Smalltalk executionTrace not!"))


(defun smalltalk-toggle-verbose-exec-tracing ()
  (interactive)
  (send-to-smalltalk
   "Smalltalk verboseTrace: Smalltalk verboseTrace not!"))

(defun test-func (arg &optional cmd-arg)
  (let ((buf (current-buffer)))
    (unwind-protect
	(progn
	  (if (not (consp (cdr arg)))
	      (progn
		(find-file-other-window (car arg))
		(goto-char (1+ (cdr arg)))
		(recenter '(0))		;hack to recenter the window without
					;redisplaying everything
		)
	    (switch-to-buffer-other-window (get-buffer-create (car arg)))
	    (smalltalk-mode)
	    (erase-buffer)
	    (insert (format "!%s methodsFor: '%s'!

%s! !" (nth 0 arg) (nth 1 arg) (nth 2 arg)))
	    (beginning-of-buffer)
	    (forward-line 2)))		;skip to start of method
      (pop-to-buffer buf))))

(defun send-to-smalltalk (str &optional mode fileinfo)
  (let (temp-file buf switch-back old-buf)
    (setq temp-file (concat "/tmp/" (make-temp-name "gst")))
    (save-excursion
      (setq buf (get-buffer-create " zap-buffer "))
      (set-buffer buf)
      (erase-buffer)
      (princ str buf)
      (write-region (point-min) (point-max) temp-file nil 'no-message)
      )
    (kill-buffer buf)
    ;; this should probably be conditional
    (save-window-excursion (gst gst-args))
;;; why is this like this?
;;    (if mode
;;	(progn
;;	  (save-excursion
;;	    (set-buffer (process-buffer *smalltalk-process*))
;;	    (setq mode-status mode))
;;	  ))
    (setq old-buf (current-buffer))
    (setq buf (process-buffer *smalltalk-process*))
    (pop-to-buffer buf)
    (if mode
	(setq mode-status mode))
    (goto-char (point-max))
    (newline)
    (pop-to-buffer old-buf)
;    (if (not (eq buf (current-buffer)))
;	(progn
;	  (switch-to-buffer-other-window buf)
;	  (setq switch-back t))
;      )
;    (if mode
;	(setq mode-status mode))
;    (goto-char (point-max))
;    (newline)
;    (and switch-back (other-window 1))
;      ;;(sit-for 0)
    (if fileinfo
	(process-send-string
	 *smalltalk-process*
	 (format
	  "FileStream fileIn: '%s' line: %d from: '%s' at: %d!\n"
	  temp-file (nth 0 fileinfo) (nth 1 fileinfo) (nth 2 fileinfo)))	
      (process-send-string *smalltalk-process*
			   (concat "FileStream fileIn: '" temp-file "'!\n")))))


(provide 'gst-mode)