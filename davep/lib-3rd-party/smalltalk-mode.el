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

;;; Incorporates Frank Caggiano's changes for Emacs 19.
;;; Updates and changes for Emacs 20 and 21 by David Forster

;; ===[ Variables and constants ]=====================================

(defvar smalltalk-name-regexp "[A-z][A-z0-9_]*"
  "A regular expression that matches a Smalltalk identifier")

(defvar smalltalk-keyword-regexp (concat smalltalk-name-regexp ":")
  "A regular expression that matches a Smalltalk keyword")

(defvar smalltalk-name-chars "A-z0-9"
  "The collection of character that can compose a Smalltalk identifier")

(defvar smalltalk-whitespace " \t\n\f")

(defconst smalltalk-indent-amount 4
  "*'Tab size'; used for simple indentation alignment.")

;; ---[ Syntax Table ]------------------------------------------------

;; This may very well be a bug, but certin chars like ?+ are set to be
;; punctuation, when in fact one might think of them as words (that
;; is, they are valid selector names).  Some functions will fail
;; however, (like smalltalk-begin-of-defun) so there punctuation.
;; Works for now...

(defvar smalltalk-mode-syntax-table 
  (let ((table (make-syntax-table)))
    (setq smalltalk-mode-syntax-table (make-syntax-table))
    ;; Make sure A-z0-9 are set to "w   " for completeness
    (let ((c 0))
      (setq c ?0)
      (while (<= c ?9)
	(setq c (1+ c))
	(modify-syntax-entry c "w   " table))
      (setq c ?A)
      (while (<= c ?Z)
	(setq c (1+ c))
	(modify-syntax-entry c "w   " table))
      (setq c ?a)
      (while (<= c ?z)
	(setq c (1+ c))
	(modify-syntax-entry c "w   " table)))
    (modify-syntax-entry ?:  ".   " table) ; Symbol-char
    (modify-syntax-entry ?_  "_   " table) ; Symbol-char
    (modify-syntax-entry ?\" "!   " table) ; Comment (generic)
    (modify-syntax-entry ?'  "\"  " table) ; String
    (modify-syntax-entry ?#  "'   " table) ; Symbol or Array constant
    (modify-syntax-entry ?\( "()  " table) ; Grouping
    (modify-syntax-entry ?\) ")(  " table) ; Grouping
    (modify-syntax-entry ?\[ "(]  " table) ; Block-open
    (modify-syntax-entry ?\] ")[  " table) ; Block-close
;;    (modify-syntax-entry ?<  "(>  " table) ; Primitive-open
;;    (modify-syntax-entry ?>  "(<  " table) ; Primitive-close
    (modify-syntax-entry ?{  "(}  " table) ; Array-open
    (modify-syntax-entry ?}  "({  " table) ; Array-close
    (modify-syntax-entry ?$  "/   " table) ; Character literal
    (modify-syntax-entry ?!  ".   " table) ; End message / Delimit defs
    (modify-syntax-entry ?\; ".   " table) ; Cascade
    (modify-syntax-entry ?|  ".   " table) ; Temporaries
    (modify-syntax-entry ?^  ".   " table) ; Return
    ;; Just to make sure these are not set to "w   "
    (modify-syntax-entry ?<  ".   " table) 
    (modify-syntax-entry ?>  ".   " table) 
    (modify-syntax-entry ?+  ".   " table) ; math
    (modify-syntax-entry ?-  ".   " table) ; math
    (modify-syntax-entry ?*  ".   " table) ; math
    (modify-syntax-entry ?/  ".   " table) ; math
    (modify-syntax-entry ?=  ".   " table) ; bool/assign
    (modify-syntax-entry ?%  ".   " table) ; valid selector
    (modify-syntax-entry ?&  ".   " table) ; boolean
    (modify-syntax-entry ?\\ ".   " table) ; ???
    (modify-syntax-entry ?~  ".   " table) ; misc. selector
    (modify-syntax-entry ?@  ".   " table) ; Point
    (modify-syntax-entry ?,  ".   " table) ; concat
    table)
  "Syntax table used by Smalltalk mode")

;; ---[ Abbrev table ]------------------------------------------------

(defvar smalltalk-mode-abbrev-table nil
  "Abbrev table in use in smalltalk-mode buffers.")
(define-abbrev-table 'smalltalk-mode-abbrev-table ())

;; ---[ Keymap ]------------------------------------------------------

(defvar smalltalk-template-map 
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "p" 'smalltalk-private-template)
    (define-key keymap "c" 'smalltalk-class-template)
    (define-key keymap "i" 'smalltalk-instance-template)
    keymap)
  "Keymap of template creation keys")

(defvar smalltalk-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\n" 	   'smalltalk-newline-and-indent)
    (define-key keymap "\C-\M-a"   'smalltalk-begin-of-defun)
    (define-key keymap "\C-\M-f"   'smalltalk-forward-sexp)
    (define-key keymap "\C-\M-b"   'smalltalk-backward-sexp)
    (define-key keymap "!" 	   'smalltalk-bang)
    (define-key keymap ":"	   'smalltalk-colon)
    (define-key keymap "\C-ct"      smalltalk-template-map)

    ;; -----

    (define-key keymap "\C-cc"     'smalltalk-compile)
    (define-key keymap "\C-cd"     'smalltalk-doit)
    (define-key keymap "\C-ce"     'smalltalk-eval-region)
    (define-key keymap "\C-cf"     'smalltalk-filein)
    (define-key keymap "\C-cm"     'gst)
    (define-key keymap "\C-cp"     'smalltalk-print)
    (define-key keymap "\C-cq"     'smalltalk-quit)
    (define-key keymap "\C-cr"     'smalltalk-reeval-region)
    (define-key keymap "\C-cs"     'smalltalk-snapshot)
    (define-key keymap "\C-c\C-s"  'smalltalk-browse-selectors)
;;    (define-key keymap "\C-c\C-t"   smalltalk-ctl-t-map)
;;    (define-key keymap "\C-c\C-b"   smalltalk-ctl-b-map)
    
    keymap)  
  "Keymap for Smalltalk mode")

(defconst smalltalk-binsel "\\([-+*/~,<>=&?]\\{1,2\\}\\|:=\\|||\\)"
  "Smalltalk binary selectors")

(defconst smalltalk-font-lock-keywords
  (list
   '("#[A-z][A-z0-9_]*" . font-lock-constant-face)
   '("\\<[A-z][A-z0-9_]*:" . font-lock-function-name-face)
   (cons smalltalk-binsel 'font-lock-function-name-face)
;   '("\\^" . font-lock-keyword-face)
   '("\\$." . font-lock-string-face) ;; Chars
   '("\\<[A-Z]\\sw*\\>" . font-lock-type-face))  
  "Basic Smalltalk keywords font-locking")

(defconst smalltalk-font-lock-keywords-1
  smalltalk-font-lock-keywords	   
  "Level 1 Smalltalk font-locking keywords")

(defconst smalltalk-font-lock-keywords-2
  (append smalltalk-font-lock-keywords-1
	  (list 
	   '("\\<\\(true\\|flase\\|nil\\|self\\|super\\)\\>" 
	     . font-lock-builtin-face)
	   '(":[a-z][A-z0-9_]*" . font-lock-variable-name-face)
	   '(" |" . font-lock-type-face)
	   '("<.*>" . font-lock-builtin-face)))
  
  "Level 2 Smalltalk font-locking keywords")

;; ---[ Interactive functions ]---------------------------------------

(defun smalltalk-mode ()
  "Major mode for editing Smalltalk code."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'smalltalk-mode)
  (setq mode-name "Smalltalk")

  (use-local-map smalltalk-mode-map)
  (set-syntax-table smalltalk-mode-syntax-table)
  (setq local-abbrev-table smalltalk-mode-abbrev-table)
  
  ;; Buffer locals

  (set (make-local-variable 'paragraph-start)
       (concat "^$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate)
       paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'indent-line-function)
       'smalltalk-indent-line)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'comment-start) "\"")
  (set (make-local-variable 'comment-end) "\"")
  (set (make-local-variable 'comment-column) 32)
  (set (make-local-variable 'comment-start-skip) "\" *")
  ;; Doesn't seem useful...?
  (set (make-local-variable 'comment-indent-function)
       'smalltalk-comment-indent)
  ;; For interactive f-b sexp
  (set (make-local-variable 'parse-sexp-ignore-comments) t)

  ;; font-locking
  (set (make-local-variable 'font-lock-defaults)  
       '((smalltalk-font-lock-keywords
	  smalltalk-font-lock-keywords-1
	  smalltalk-font-lock-keywords-2)
	 nil nil nil nil))

  ;; Run hooks, must be last
  (run-hooks 'smalltalk-mode-hook))

(defun smalltalk-tab ()
  (interactive)
  (let (col)
    ;; round up, with overflow
    (setq col (* (/ (+ (current-column) smalltalk-indent-amount)
		    smalltalk-indent-amount)
		 smalltalk-indent-amount))
    (indent-to-column col)))

(defun smalltalk-begin-of-defun ()
  "Skips to the beginning of the current method.  If already at
the beginning of a method, skips to the beginning of the previous
one."
  (interactive)
  (let ((parse-sexp-ignore-comments t) here delim start)
    (setq here (point))
    (while (and (search-backward "!" nil 'to-end)
		(setq delim (smalltalk-in-string)))
      (search-backward delim))
    (setq start (point))
    (if (looking-at "!")
	(forward-char 1))
    (smalltalk-forward-whitespace)
    ;; check to see if we were already at the start of a method
    ;; in which case, the semantics are to go to the one preceeding
    ;; this one
    (if (and (= here (point))
	     (/= start (point-min)))
	(progn
	  (goto-char start)
	  (smalltalk-backward-whitespace) ;may be at ! "foo" !
	  (if (= (preceding-char) ?!)
	      (backward-char 1))
	  (smalltalk-begin-of-defun)))))  ;and go to the next one

(defun smalltalk-forward-sexp (n)
  (interactive "p")
  (let (i)
    (cond ((< n 0)
	   (smalltalk-backward-sexp (- n)))
	  ((null parse-sexp-ignore-comments)
	   (forward-sexp n))
	  (t
	   (while (> n 0)
	     (smalltalk-forward-whitespace)
	     (forward-sexp 1)
	     (setq n (1- n)))))))

(defun smalltalk-backward-sexp (n)
  (interactive "p")
  (let (i)
    (cond ((< n 0)
	   (smalltalk-forward-sexp (- n)))
	  ((null parse-sexp-ignore-comments)
	   (backward-sexp n))
	  (t
	   (while (> n 0)
	     (smalltalk-backward-whitespace)
	     (backward-sexp 1)
	     (setq n (1- n)))))))

(defun smalltalk-reindent ()
  (interactive)
  ;; Loses if at first charcter on line
  (smalltalk-indent-line))

(defun smalltalk-newline-and-indent (levels)
  "Called basically to do newline and indent.  Sees if the current line is a
new statement, in which case the indentation is the same as the previous
statement (if there is one), or is determined by context; or, if the current
line is not the start of a new statement, in which case the start of the
previous line is used, except if that is the start of a new line in which case
it indents by smalltalk-indent-amount."
  (interactive "p")
  (newline)
  (smalltalk-indent-line))

(defun smalltalk-colon ()
  "Possibly reindents a line when a colon is typed.
If the colon appears on a keyword that's at the start of the line (ignoring
whitespace, of course), then the previous line is examined to see if there
is a colon on that line, in which case this colon should be aligned with the
left most character of that keyword.  This function is not fooled by nested
expressions."
  (interactive)
  (let (needs-indent state (parse-sexp-ignore-comments t))
    (setq state (parse-partial-sexp (point-min) (point)))

    (if (null (nth 3 state))		;we're not in string or comment
	(progn
	  (save-excursion
      	    (skip-chars-backward "A-z0-9_")
	    (if (and (looking-at smalltalk-name-regexp)
		     (not (smalltalk-at-method-begin)))
		(setq needs-indent (smalltalk-white-to-bolp))))
	  (and needs-indent
	       (smalltalk-indent-for-colon))))
    ;; out temporarily
    ;;    (expand-abbrev)			;I don't think this is the "correct"
    ;;					;way to do this...I suspect that
    ;;					;some flavor of "call interactively"
    ;;					;is better.
    (self-insert-command 1)))

(defun smalltalk-bang ()
  (interactive)
  (insert "!")
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^[ \t]+!")
	(delete-horizontal-space))))

(defun smalltalk-instance-template (class-name category-name)
  (interactive
   (list (read-string "Class: " (smalltalk-backward-find-class-name))
	 (read-string "Category: ")))
  (insert (format "!%s methodsFor: '%s'!\n\n" class-name category-name))
  (save-excursion
    (insert "\n! !\n")))

(defun smalltalk-private-template (class-name)
  (interactive
   (list (read-string "Class: " (smalltalk-backward-find-class-name))))
  (insert (format "!%s methodsFor: 'private'!\n\n" class-name))
  (save-excursion
    (insert "\n! !\n")))

(defun smalltalk-class-template (class-name category-name)
  (interactive
   (list (read-string "Class: " (smalltalk-backward-find-class-name))
	 (read-string "Category: " "instance creation")))
  (insert (format "!%s class methodsFor: '%s'!\n\n" class-name category-name))
  (save-excursion
    (insert "\n! !\n")))

;; ---[ Non-interactive functions ]-----------------------------------

;; This is used by indent-for-comment
;; to decide how much to indent a comment in Smalltalk code
;; based on its context.
(defun smalltalk-comment-indent ()
  (if (looking-at "^\"")
      0				;Existing comment at bol stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max (1+ (current-column))	;Else indent at comment column
	   comment-column))))	; except leave at least one space.

(defun smalltalk-indent-line ()
  (if (smalltalk-in-comment)
      (save-restriction
	(let (start end)
	  (save-excursion (setq start (search-backward "\"")))
	  (save-excursion (setq end (search-forward "\"")))
	  (narrow-to-region start end))
	(indent-relative))
    (let (indent-amount is-keyword)
      (save-excursion
	(beginning-of-line)
	(smalltalk-forward-whitespace)
	(if (looking-at "[A-z][A-z0-9_]*:") ;indent for colon
	    (let ((parse-sexp-ignore-comments t))
	      (beginning-of-line)
	      (smalltalk-backward-whitespace)
	      (if (not (memq (preceding-char) '(?\;)))
		  (setq is-keyword t)))))
      (if is-keyword
	  (smalltalk-indent-for-colon)
	(setq indent-amount (calculate-smalltalk-indent))
	(smalltalk-indent-to-column indent-amount)))))
  
(defun calculate-smalltalk-indent ()
  (let (needs-indent indent-amount done c state orig start-of-line
		     (parse-sexp-ignore-comments t))
    (save-excursion
      (save-restriction
	(widen)
	(narrow-to-region (point-min) (point)) ;only care about what's before
	(setq state (parse-partial-sexp (point-min) (point)))
	(cond ((equal (nth 3 state) ?\") ;in a comment
	       (save-excursion
		 (smalltalk-backward-comment)
		 (setq indent-amount (1+ (current-column)))))
	      ((equal (nth 3 state) ?')	;in a string
	       (setq indent-amount 0))
	      (t
	       (save-excursion
		 (smalltalk-backward-whitespace)
		 (if (or (bobp)
			 (= (preceding-char) ?!))
		     (setq indent-amount 0)))))
	(if (null indent-amount)
	    (progn
	      (smalltalk-narrow-to-method)
	      (beginning-of-line)
	      (setq state (parse-partial-sexp (point-min) (point)))
	      (narrow-to-paren state)
	      (smalltalk-backward-whitespace)
	      (cond ((bobp)		;must be first statment in block or exp
		     (if (nth 1 state)	;we're in a paren exp
			 (if (looking-at "$")
			     ;; block with no statements, indent by 4
			     (setq indent-amount (+ (smalltalk-current-indent)
						    smalltalk-indent-amount))

			     ;; block with statements, indent to first non-whitespace
			     (setq indent-amount (smalltalk-current-column)))

		       ;; we're top level
		       (setq indent-amount smalltalk-indent-amount)))
		    ((= (preceding-char) ?.) ;at end of statement
		     (smalltalk-find-statement-begin)
		     (setq indent-amount (smalltalk-current-column)))
		    ((= (preceding-char) ?:)
		     (beginning-of-line)
		     (smalltalk-forward-whitespace)
		     (setq indent-amount (+ (smalltalk-current-column)
					    smalltalk-indent-amount)))
		    ((= (preceding-char) ?>) ;maybe <primitive: xxx>
		     (setq orig (point))
		     (backward-char 1)
		     (smalltalk-backward-whitespace)
		     (skip-chars-backward "0-9")
		     (smalltalk-backward-whitespace)
		     (if (= (preceding-char) ?:)
			 (progn
			   (backward-char 1)
			   (skip-chars-backward "a-zA-Z_")
			   (if (looking-at "primitive:")
			       (progn
				 (smalltalk-backward-whitespace)
				 (if (= (preceding-char) ?<)
				     (setq indent-amount (1- (smalltalk-current-column))))))))
		     (if (null indent-amount)
			 (progn
			   (goto-char orig)
			   (smalltalk-find-statement-begin)
			   (setq indent-amount (+ (smalltalk-current-column)
						  smalltalk-indent-amount)))))
		    (t			;must be a statement continuation
		     (save-excursion
		       (beginning-of-line)
		       (setq start-of-line (point)))
		     (smalltalk-find-statement-begin)
		     (setq indent-amount (+ (smalltalk-current-column)
					    smalltalk-indent-amount))))))
	indent-amount))))


(defun smalltalk-previous-nonblank-line ()
  (forward-line -1)
  (while (and (not (bobp))
	      (looking-at "^[ \t]*$"))
    (forward-line -1)))

(defun smalltalk-in-string ()
  "Returns non-nil delimiter as a string if the current location is
actually inside a string or string like context."
  (let (state)
    (setq state (parse-partial-sexp (point-min) (point)))
    (and (nth 3 state)
	 (char-to-string (nth 3 state)))))

(defun smalltalk-in-comment ()
  "Returns non-nil if the current location is inside a comment"
  (let (state)
    (setq state (parse-partial-sexp (point-min) (point)))
    (nth 4 state)))

(defun smalltalk-forward-whitespace ()
  "Skip white space and comments forward, stopping at end of buffer
or non-white space, non-comment character"
  (while (looking-at (concat "[" smalltalk-whitespace "]"))
    (skip-chars-forward smalltalk-whitespace)
    (if (= (following-char) ?\")
	(forward-comment 1))))

;; (defun smalltalk-forward-whitespace ()
;;   "Skip white space and comments forward, stopping at end of buffer
;; or non-white space, non-comment character"
;;   (forward-comment 1)
;;   (if (= (following-char) ?\n)
;;       (forward-char)))

(defun smalltalk-backward-whitespace ()
  "Like forward whitespace only going towards the start of the buffer"
  (while (progn (skip-chars-backward smalltalk-whitespace)
		(= (preceding-char) ?\"))
    (search-backward "\"" nil t 2)))
	

(defun smalltalk-current-column ()
  "Returns the current column of the given line, regardless of narrowed buffer."
  (save-restriction
    (widen)
    (current-column)))			;this changed in 18.56

(defun smalltalk-current-indent ()
  "Returns the indentation of the given line, regardless of narrowed buffer."
  (save-restriction
    (widen)
    (beginning-of-line)
    (skip-chars-forward smalltalk-whitespace)
    (current-column)))

(defun smalltalk-find-statement-begin ()
  "Leaves the point at the first non-blank, non-comment character of a new
statement.  If begininning of buffer is reached, then the point is left there.
This routine only will return with the point pointing at the first non-blank
on a line; it won't be fooled by multiple statements on a line into stopping
prematurely.  Also, goes to start of method if we started in the method
selector."
  (let (start ch)
    (if (= (preceding-char) ?.)		;if we start at eos
	(backward-char 1))		;we find the begin of THAT stmt
    (while (and (null start) (not (bobp)))
      (smalltalk-backward-whitespace)
      (cond ((= (setq ch (preceding-char)) ?.)
	     (let (saved-point)
	       (setq saved-point (point))
	       (smalltalk-forward-whitespace)
	       (if (smalltalk-white-to-bolp)
		   (setq start (point))
		 (goto-char saved-point)
		 (smalltalk-backward-sexp 1))
	       ))
	    ((= ch ?^)			;HACK -- presuming that when we back
					;up into a return that we're at the
					;start of a statement
	     (backward-char 1)
	     (setq start (point)))
	    ((= ch ?!)
	     (smalltalk-forward-whitespace)
	     (setq start (point)))
	    (t
	     (smalltalk-backward-sexp 1))))
    (if (null start)
      (progn
	(goto-char (point-min))
	(smalltalk-forward-whitespace)
	(setq start (point))))
    start))

(defun narrow-to-paren (state)
  "Narrows the region to between point and the closest previous open paren.
Actually, skips over any block parameters, and skips over the whitespace
following on the same line."
  (let ((paren-addr (nth 1 state))
	start c done)
    (if (not paren-addr)
	()
      (save-excursion
	(goto-char paren-addr)
	(setq c (following-char))
	(cond ((eq c ?\()
	       (setq start (1+ (point))))
	      ((eq c ?\[)
	       (forward-char 1)

	       ;; Now skip over the block parameters, if any
	       (setq done nil)
	       (while (not done)
		 (skip-chars-forward " \t")
		 (setq c (following-char))
		 (cond ((eq c ?:)
			(smalltalk-forward-sexp 1))
		       ((eq c ?|)
			(forward-char 1) ;skip vbar
			(skip-chars-forward " \t")
			(setq done t))	;and leave
		       (t
			(setq done t))))

	       ;; Now skip over the block temporaries, if any
	       (cond ((eq (following-char) ?|)
		      (setq done nil)
		      (forward-char 1))
		     (t
		      (setq done t)))
	       
	       (while (not done)
		 (skip-chars-forward " \t")
		 (setq c (following-char))
		 (cond ((eq c ?|)
			(forward-char 1) ;skip vbar
			(skip-chars-forward " \t")
			(setq done t))	;and leave
		       (t
			(smalltalk-forward-sexp 1))))

	       (setq start (point)))))
      (narrow-to-region start (point)))))

(defun smalltalk-at-method-begin ()
  "Returns T if at the beginning of a method definition, otherwise nil"
  (let ((parse-sexp-ignore-comments t))
    (if (bolp)
	(save-excursion
	  (smalltalk-backward-whitespace)
	  (= (preceding-char) ?!)
	  ))))

(defun smalltalk-indent-for-colon ()
  (let (indent-amount c start-line state done default-amount
		     (parse-sexp-ignore-comments t))
    ;; we're called only for lines which look like "<whitespace>foo:"
    (save-excursion
      (save-restriction
	(widen)
	(smalltalk-narrow-to-method)
	(beginning-of-line)
	(setq state (parse-partial-sexp (point-min) (point)))
	(narrow-to-paren state)
	(narrow-to-region (point-min) (point))
	(setq start-line (point))
	(smalltalk-backward-whitespace)
	(cond
	 ((bobp)
	  (setq indent-amount (smalltalk-current-column)))
	 ((eq (setq c (preceding-char)) ?\;)	; cascade before, treat as stmt continuation
	  (smalltalk-find-statement-begin)
	  (setq indent-amount (+ (smalltalk-current-column)
				 smalltalk-indent-amount)))
	 ((eq c ?.)	; stmt end, indent like it (syntax error here?)
	  (smalltalk-find-statement-begin)
	  (setq indent-amount (smalltalk-current-column)))
	 (t				;could be a winner
	    (smalltalk-find-statement-begin)
	    ;; we know that since we weren't at bobp above after backing
	    ;; up over white space, and we didn't run into a ., we aren't
	    ;; at the beginning of a statement, so the default indentation
	    ;; is one level from statement begin
	    (setq default-amount
		  (+ (smalltalk-current-column) ;just in case
		     smalltalk-indent-amount))
	    ;; might be at the beginning of a method (the selector), decide
	    ;; this here
	    (if (not (looking-at smalltalk-keyword-regexp ))
		;; not a method selector
		(while (and (not done) (not (eobp)))
		  (smalltalk-forward-sexp 1) ;skip over receiver
		  (smalltalk-forward-whitespace)
		  (cond ((eq (following-char) ?\;)
			 (setq done t)
			 (setq indent-amount default-amount))
			((and (null indent-amount) ;pick up only first one
			      (looking-at smalltalk-keyword-regexp))
			 (setq indent-amount (smalltalk-current-column))))))
	    (and (null indent-amount)
		 (setq indent-amount default-amount))))))
    (if indent-amount
	(smalltalk-indent-to-column indent-amount))))

(defun smalltalk-indent-to-column (col)
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to col))
  (if (bolp)
      ;;delete horiz space may have moved us to bol instead of staying where
      ;; we were.  this fixes it up.
      (move-to-column col)))

(defun smalltalk-narrow-to-method ()
  "Narrows the buffer to the contents of the method, exclusive of the
method selector and temporaries."
  (let ((end (point))
	(parse-sexp-ignore-comments t)
	done handled)
    (save-excursion
      (smalltalk-begin-of-defun)
      (if (looking-at "[a-zA-z]")	;either unary or keyword msg
	  ;; or maybe an immediate expression...
	  (progn
	    (forward-sexp)
	    (if (= (following-char) ?:) ;keyword selector
		(progn			;parse full keyword selector
		  (backward-sexp 1)	;setup for common code
		  (smalltalk-forward-keyword-selector))
	      ;; else maybe just a unary selector or maybe not
	      ;; see if there's stuff following this guy on the same line
	      (let (here eol-point)
		(setq here (point))
		(end-of-line)
		(setq eol-point (point))
		(goto-char here)
		(smalltalk-forward-whitespace)
		(if (< (point) eol-point) ;if there is, we're not a method
					; (a heuristic guess)
		    (beginning-of-line)
		  (goto-char here)))))	;else we're a unary method (guess)
	;; this must be a binary selector, or a temporary
	(if (= (following-char) ?|)
	    (progn			;could be temporary
	      (end-of-line)
	      (smalltalk-backward-whitespace)
	      (if (= (preceding-char) ?|)
		  (progn
		    (setq handled t)))
	      (beginning-of-line)))
	(if (not handled)
	    (progn
	      (skip-chars-forward (concat "^" smalltalk-whitespace))
	      (smalltalk-forward-whitespace)
	      (skip-chars-forward smalltalk-name-chars)))) ;skip over operand
      (smalltalk-forward-whitespace)
      ;;sbb  6-Sep-93 14:58:54 attempted fix(skip-chars-forward smalltalk-whitespace)
      (if (= (following-char) ?|)	;scan for temporaries
	  (progn
	    (forward-char)		;skip over |
	    (smalltalk-forward-whitespace)
	    (while (and (not (eobp))
			(looking-at "[a-zA-Z_]"))
	      (skip-chars-forward smalltalk-name-chars)
	      (smalltalk-forward-whitespace)
	      )
	    (if (and (= (following-char) ?|) ;only if a matching | as a temp
		     (< (point) end))	;and we're after the temps
		(narrow-to-region (1+ (point)) end))) ;do we limit the buffer
	;; added "and <..." Dec 29 1991 as a test
	(and (< (point) end)
	     (narrow-to-region (point) end))))))

(defun smalltalk-forward-keyword-selector ()
  "Starting on a keyword, this function skips forward over a keyword selector.
It is typically used to skip over the actual selector for a method."
  (let (done)
    (while (not done)
      (if (not (looking-at "[a-zA-Z_]"))
	  (setq done t)
	(skip-chars-forward smalltalk-name-chars)
	(if (= (following-char) ?:)
	    (progn
	      (forward-char)
	      (smalltalk-forward-sexp 1)
	      (smalltalk-forward-whitespace))
	  (setq done t)
	  (backward-sexp 1))))))

(defun smalltalk-white-to-bolp ()
  "Returns T if from the current position to beginning of line is whitespace.
Whitespace is defined as spaces, tabs, and comments."
  (let (done is-white line-start-pos)
    (save-excursion
      (save-excursion
	(beginning-of-line)
	(setq line-start-pos (point)))
      (while (not done)
	(and (not (bolp))
	     (skip-chars-backward " \t"))
	(cond ((bolp)
	       (setq done t)
	       (setq is-white t))
	      ((= (char-after (1- (point))) ?\")
	       (backward-sexp)
	       (if (< (point) line-start-pos) ;comment is multi line
		   (setq done t)))
	      (t
	       (setq done t))))
      is-white)))


(defun smalltalk-backward-comment ()
  (search-backward "\"")		;find its start
  (while (= (preceding-char) ?\")	;skip over doubled ones
    (backward-char 1)
    (search-backward "\"")))

(defun smalltalk-collect-selector ()
  "Point is stationed inside or at the beginning of the selector in question.
This function computes the Smalltalk selector (unary, binary, or keyword) and
returns it as a string.  Point is not changed."
  (save-excursion
    (let (start selector done ch
		(parse-sexp-ignore-comments t))
      (skip-chars-backward (concat "^" "\"" smalltalk-whitespace))
      (setq start (point))
      (if (looking-at smalltalk-name-regexp)
	  (progn			;maybe unary, maybe keyword
	    (skip-chars-forward smalltalk-name-chars)
	    (if (= (following-char) ?:)	;keyword?
		(progn
		  (forward-char 1)
		  (setq selector (buffer-substring start (point)))
		  (setq start (point))
		  (while (not done)
		    (smalltalk-forward-whitespace)
		    (setq ch (following-char))
		    (cond ((memq ch '(?\; ?. ?\] ?\) ?! ))
			   (setq done t))
			  ((= ch ?:)
			   (forward-char 1)
			   (setq selector
				 (concat selector
					 (buffer-substring start (point)))))
			  (t
			   (setq start (point))
			   (smalltalk-forward-sexp 1)))))
	      (setq selector (buffer-substring start (point)))))
	(skip-chars-forward (concat "^" ?\" smalltalk-whitespace))
	(setq selector (buffer-substring start (point))))
      selector)))

(defun smalltalk-backward-find-class-name ()
  (let (first-hit-point first-hit second-hit-point second-hit)
    (save-excursion
      (if (setq first-hit-point
		(search-backward-regexp "^![ \t]*\\(\\w+\\)[ \t]+" nil t))
	  (setq first-hit (buffer-substring (match-beginning 1) (match-end 1)))))
    (save-excursion
      (if (setq second-hit-point
		(search-backward-regexp
		 "^\\w+[ \t]+\\(variable\\|variableWord\\|variableByte\\)?subclass:[ \t]+#\\(\\w+\\)" nil t))
	  (setq second-hit (buffer-substring
			    (match-beginning 2)
			    (match-end 2)))))
    (if first-hit-point
	(if (and second-hit-point (> second-hit-point first-hit-point))
	    second-hit
	  first-hit)
      (or second-hit ""))))


(provide 'smalltalk-mode)
;;; (require 'gst-mode)

