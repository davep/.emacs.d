;;; xrdb-mode.el --- mode for editing X resource database files

;; Author:        1994-1997 Barry A. Warsaw
;; Maintainer:    tools-help@python.org
;; Created:       May 1994
;; Version:       1.21
;; Last Modified: 1997/02/24 03:34:56
;; Keywords:      data languages

;; Copyright (C) 1994 Barry A. Warsaw

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; In 1994 I wrote:
;;
;; "I used to be like you.  I used to hack on X resource database files
;;  all the time, and when I did, I found this mode to be fairly
;;  useful.  It's by no means perfect.  At one time I had a collection
;;  of hacks that did some nice indentation of resource lines, but
;;  they were not organized in any way.  This mode was my attempt to
;;  congeal this mess into a proper major mode.  I release it now, not
;;  because it will change your life, but because I don't plan to do
;;  anything more with it.
;;
;;  I have since been enlightened and no longer have to cavort with
;;  mere mortal X hackers anymore.  I like my brain cells, so now I
;;  use NEXTSTEP where all is glory.  Or would you say I traded one
;;  vice for another?  Hmm...  Anyway, if you are still down in the
;;  trenches and would like to inherit this file, let me know.  I
;;  don't intend to do any work on it any more... unless I lose my
;;  place in paradise.  I promise to be good, Steve.  :-) :-)"
;;
;; I have fallen from grace and have been kicked out of paradise.  So
;; has Steve Jobs apparently :-)
;;
;; To use, put the following in your .emacs:
;;
;; (autoload 'xrdb-mode "xrdb-mode" "Mode for editing X resource files" t)
;;
;; You may also want something like:
;;
;; (setq auto-mode-alist
;;       (append '(("\\.Xdefaults$" . xrdb-mode)
;;                 ("\\.Xenvironment$" . xrdb-mode)
;;                 ("\\.Xresources$" . xrdb-mode)
;;                 )
;;               auto-mode-alist))


;; Code:


;; These variables are available for your customization
(defgroup xrdb nil
  "Mode for editing X resource database files."
  :group 'data
  :group 'languages)

(defcustom xrdb-mode-hook nil
  "*Hook to be run when `xrdb-mode' is entered."
  :type 'hook
  :group 'xrdb)

(defcustom xrdb-subdivide-by 'paragraph
  "*Extent of alignment calculations.
Can be one of `buffer', `paragraph', `page', or `line'.  Do a
\\[describe-function] xrdb-indent-buffer RET for more information."
  :type '(radio (const buffer) (const paragraph)
		(const page) (const line))
  :group 'xrdb)



;; no need to customize anything below this line
(defconst xrdb-comment-re "^[ \t]*[!]"
  "Character which starts a comment.")
(defconst xrdb-separator-char ?:
  "Character which separates resource specs from values.")


;; utilities
(defsubst xrdb-point (position)
  ;; Returns the value of point at certain commonly referenced POSITIONs.
  ;; POSITION can be one of the following symbols:
  ;; 
  ;; bol  -- beginning of line
  ;; eol  -- end of line
  ;; bod  -- beginning of defun
  ;; boi  -- back to indentation
  ;; ionl -- indentation of next line
  ;; iopl -- indentation of previous line
  ;; bonl -- beginning of next line
  ;; bopl -- beginning of previous line
  ;; bop  -- beginning of paragraph
  ;; eop  -- end of paragraph
  ;; bopg -- beginning of page
  ;; eopg -- end of page
  ;; 
  ;; This function does not modify point or mark.
  (let ((here (point)))
    (cond
     ((eq position 'bol)  (beginning-of-line))
     ((eq position 'eol)  (end-of-line))
     ((eq position 'boi)  (back-to-indentation))
     ((eq position 'bonl) (forward-line 1))
     ((eq position 'bopl) (forward-line -1))
     ((eq position 'bop)  (forward-paragraph -1))
     ((eq position 'eop)  (forward-paragraph 1))
     ((eq position 'bopg)  (forward-page -1))
     ((eq position 'eopg)  (forward-page 1))
     (t
      (error "unknown buffer position requested: %s" position)))
    (prog1
	(point)
      (goto-char here))
    ))

(defsubst xrdb-skip-to-separator ()
  ;; skip forward from the beginning of the line to the separator
  ;; character as given by xrdb-separator-char. Returns t if the
  ;; char was found, otherwise, nil.
  (beginning-of-line)
  (skip-chars-forward
   (concat "^" (char-to-string xrdb-separator-char))
   (xrdb-point 'eol))
  (= (following-char) xrdb-separator-char))



;; commands
(defun xrdb-electric-separator (arg)
  "Insert the separator character.
Re-align the line unless an argument is given."
  (interactive "P")
  ;; first insert the character
  (self-insert-command (prefix-numeric-value arg))
  ;; only do electric behavior if arg is not given
  (if (not arg)
      (xrdb-align-to (xrdb-point 'bol)
		     (xrdb-point 'bonl)
		     (save-excursion
		       (beginning-of-line)
		       (forward-comment (- (point-max)))
		       (beginning-of-line)
		       (xrdb-skip-to-separator)
		       (current-column)))))

(defun xrdb-align-to (start end goalcolumn)
  (interactive "r\nnAlign to column: ")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (beginning-of-buffer)
      (while (< (point) (point-max))
	(if (and (not (looking-at xrdb-comment-re))
		 (xrdb-skip-to-separator)
		 goalcolumn)
	    (indent-code-rigidly (xrdb-point 'bol)
				 (xrdb-point 'bonl)
				 (- goalcolumn (current-column))))
	(forward-line 1)))))

(defun xrdb-indent-line (arg)
  "Re-align current line."
  (interactive "P")
  ;; narrow to the region specified by xrdb-subdivide-by
  (save-excursion
    (save-restriction
      (widen)
      (cond
       ((eq xrdb-subdivide-by 'buffer))
       ((eq xrdb-subdivide-by 'page)
	(narrow-to-page))
       ((eq xrdb-subdivide-by 'paragraph)
	(narrow-to-region (xrdb-point 'bop) (xrdb-point 'eop)))
       (t
	(narrow-to-region (xrdb-point 'bopl) (xrdb-point 'bonl))
	))
      ;; indent line
      (xrdb-align-to (xrdb-point 'bol) (xrdb-point 'bonl)
		     (xrdb-region-goal-column))
      )))

(defun xrdb-indent-region (start end)
  "Re-align region."
  (interactive "r")
  ;; narrow to region
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (xrdb-align-to (point-min) (point-max) (xrdb-region-goal-column))
      )))

(defun xrdb-indent-page ()
  "Re-align the current page."
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-page)
      (xrdb-align-to (point-min) (point-max) (xrdb-region-goal-column))
      )))

(defun xrdb-indent-paragraph ()
  "Re-align the current paragraph."
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (xrdb-point 'bop) (xrdb-point 'eop))
      (xrdb-align-to (point-min) (point-max) (xrdb-region-goal-column))
      )))

(defun xrdb-indent-buffer (arg)
  "Re-align the entire buffer.
Alignment calculations are controlled by the variable
`xrdb-subdivide-by', which can take the values `buffer', `paragraph',
`page', or `line', with the following meanings:

 buffer - all non-comment lines are aligned with the longest line in
          the buffer.  Since every line must be scanned, this will
	  take the longest to perform.

 paragraph - alignment of lines spanning paragraphs. A paragraph is
             defined as all contiguous lines between blank or comment
	     lines.

 page - alignment of lines spanning pages (i.e. separated by
        page-delimiter, usually ^L).

 none - alignment of lines based on the previous line.

With optional \\[universal-argument], queries for alignment subdivision."
  (interactive "P")
  (let ((align-by (if (not arg)
		      xrdb-subdivide-by
		    (completing-read
		     "Align by: "
		     '(("buffer" . buffer)
		       ("paragraph" . paragraph)
		       ("page" . page)
		       ("line" . line))
		     nil t (format "%s" xrdb-subdivide-by)))))
    (message "Aligning by %s..." align-by)
    (save-excursion
      (save-restriction
	(widen)
	(cond
	 ;; by buffer
	 ((eq align-by 'buffer)
	  (xrdb-align-to (point-min) (point-max) (xrdb-region-goal-column)))
	 ;; by paragraph
	 ((eq align-by 'paragraph)
	  (beginning-of-buffer)
	  (while (< (point) (point-max))
	    (narrow-to-region (point) (xrdb-point 'eop))
	    (xrdb-align-to (point-min) (point-max) (xrdb-region-goal-column))
	    (beginning-of-buffer)
	    (widen)
	    (forward-paragraph 1)
	    ))
	 ;; by page
	 ((eq align-by 'page)
	  (beginning-of-buffer)
	  (while (< (point) (point-max))
	    (narrow-to-region (point) (xrdb-point 'eopg))
	    (xrdb-align-to (point-min) (point-max) (xrdb-region-goal-column))
	    (beginning-of-buffer)
	    (widen)
	    (forward-page 1)
	    ))
	 ;; by line
	 (t
	  (beginning-of-buffer)
	  (let ((prev-goalcol 0))
	    (while (< (point) (point-max))
	      ;; skip comments and blank lines
	      (if (not (looking-at paragraph-start))
		  (progn
		    (xrdb-align-to (xrdb-point 'bol) (xrdb-point 'bonl)
				   prev-goalcol)
		    (xrdb-skip-to-separator)
		    (setq prev-goalcol (- (point) (xrdb-point 'boi)))
		    ))
	      (forward-line 1))))
	 )))
    (message "Aligning by %s... done." align-by)
    ))


;; major-mode stuff
(defvar xrdb-mode-abbrev-table nil
  "Abbrev table used in `xrdb-mode' buffers.")
(define-abbrev-table 'xrdb-mode-abbrev-table ())


(defvar xrdb-mode-syntax-table nil
  "Syntax table used in `xrdb-mode' buffers.")
(if xrdb-mode-syntax-table
    ()
  (setq xrdb-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?!  "<" xrdb-mode-syntax-table)
  (modify-syntax-entry ?\n ">" xrdb-mode-syntax-table))


(defvar xrdb-mode-map ()
  "Keymap used in `xrdb-mode' buffers.")
(if xrdb-mode-map
    ()
  (setq xrdb-mode-map (make-sparse-keymap))
  (let ((ekey (char-to-string xrdb-separator-char)))
    ;; make the separator key electric
    (define-key xrdb-mode-map ekey 'xrdb-electric-separator)
    (define-key xrdb-mode-map "\t" 'xrdb-indent-line)
    (define-key xrdb-mode-map "\C-c\C-a" 'xrdb-indent-paragraph)
    (define-key xrdb-mode-map "\C-c\C-b" 'xrdb-submit-bug-report)
    (define-key xrdb-mode-map "\C-c\C-p" 'xrdb-indent-page)
    (define-key xrdb-mode-map "\C-c\C-r" 'xrdb-indent-region)
    (define-key xrdb-mode-map "\C-c\C-u" 'xrdb-indent-buffer)
    (define-key xrdb-mode-map "\C-c>"    'xrdb-align-to)
    ))

;;;###autoload
(defun xrdb-mode ()
  "Major mode for editing xrdb config files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table xrdb-mode-syntax-table)
  (setq major-mode 'xrdb-mode
	mode-name "xrdb"
	local-abbrev-table xrdb-mode-abbrev-table)
  (use-local-map xrdb-mode-map)
  ;; local variables
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  ;; now set their values
  (setq parse-sexp-ignore-comments t
	comment-start "! "
	comment-end "")
  (setq indent-region-function 'xrdb-indent-region
	paragraph-ignore-fill-prefix t
	paragraph-start (concat "^[ \t]*$\\|^[ \t]*[!]\\|" page-delimiter)
	paragraph-separate paragraph-start)
  (run-hooks 'xrdb-mode-hook))



;; faces and font-locking
(defvar xrdb-option-name-face 'xrdb-option-name-face
  "Face for option name on a line in an X resource db file")
(defvar xrdb-option-value-face 'xrdb-option-value-face
  "Face for option value on a line in an X resource db file")

(make-face 'xrdb-option-name-face)
(make-face 'xrdb-option-value-face)

(defun xrdb-font-lock-mode-hook ()
  (or (face-differs-from-default-p 'xrdb-option-name-face)
      (copy-face 'font-lock-keyword-face 'xrdb-option-name-face))
  (or (face-differs-from-default-p 'xrdb-option-value-face)
      (copy-face 'font-lock-string-face 'xrdb-option-value-face))
  (remove-hook 'font-lock-mode-hook 'xrdb-font-lock-mode-hook))
(add-hook 'font-lock-mode-hook 'xrdb-font-lock-mode-hook)

(defvar xrdb-font-lock-keywords
  (list '("^[ \t]*\\([^\n:]*:\\)[ \t]*\\(.*\\)$"
	  (1 xrdb-option-name-face)
	  (2 xrdb-option-value-face)))
  "Additional expressions to highlight in X resource db mode.")
(put 'xrdb-mode 'font-lock-defaults '(xrdb-font-lock-keywords))



;; commands
(defun xrdb-region-goal-column ()
  ;; Returns the goal column of the current region.  Assumes the
  ;; buffer has been narrowed to the region to scan.
  (save-excursion
    (beginning-of-buffer)
    (let ((goalcol -1)
	  linecol)
      (while (< (point) (point-max))
	;; skip any comments
	(if (and (not (looking-at xrdb-comment-re))
		 (xrdb-skip-to-separator)
		 (< goalcol (setq linecol (current-column)))
		 )
	    (setq goalcol linecol))
	(forward-line 1))
      (if (< goalcol 0)
	  nil
	goalcol))))



;; submitting bug reports

(defconst xrdb-version "1.21"
  "xrdb-mode version number.")

(defconst xrdb-mode-help-address "tools-help@python.org"
  "Address for xrdb-mode bug reports.")

(eval-when-compile
  (require 'reporter))

(defun xrdb-submit-bug-report ()
  "Submit via mail a bug report on xrdb-mode."
  (interactive)
  ;; load in reporter
  (let ((reporter-prompt-for-summary-p t)
	(varlist '(xrdb-subdivide-by
		   xrdb-mode-hook
		   )))
    (and (if (y-or-n-p "Do you want to submit a report on xrdb-mode? ")
	     t
	   (message "")
	   nil)
	 (require 'reporter)
	 (reporter-submit-bug-report
	  xrdb-mode-help-address "xrdb-mode" varlist nil nil "Dear Barry,")
	 )))


(provide 'xrdb-mode)
;; xrdb-mode.el ends here
