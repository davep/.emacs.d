;; Copyright (C) 1997 by Free Software Foundation, Inc.

;; Author: Steven L Baur <steve@xemacs.org>
;; Keywords: mail, news
;; Version: 0.20beta1-azu

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; This file provides footnote[1] support for message-mode in emacsen.
;; footnote-mode is implemented as a minor mode.

;; [1] Footnotes look something like this.  Along with some decorative
;; stuff.

;; TODO:
;; Reasonable Undo support.
;; more language styles.

;;; Change Log:

;; May-31-1998: In `Footnote-make-hole', `concat' was called with integer
;;              argument; now use `Footnote-index-to-string' and `format'
;; Apr-04-1997: Added option to narrow buffer when editing the text of
;;		a footnote.
;;		Insertion and renumbering now works.
;;		Deletion and renumbering now works.
;;		Footnote styles implemented.
;; Apr-05-1997: Added dumb version of footnote-set-style.
;;		Merged minor corrections from Hrvoje Niksic, Sudish Joseph,
;;		and David Moore.
;;		Remove absolute dependency on message-mode.
;;		Replicate letters when footnote numbers hit the end of
;;		the alphabet.
;; Apr-06-1997: Emacs portability patches from Lars Magne Ingebrigtsen.
;; Apr-18-1997:	Stricter matching of footnote tag.  (Idea from Colin Rafferty)
;; May-16-1997:	Allow customization of spacing of footnote body tag.  (Idea
;;		from Samuel Tardieu).

;;; Code:

(require 'cl)

(defgroup footnote nil
  "Support for footnotes in mail and news messages."
  :group 'message)

;;;###autoload
(defcustom footnote-mode-line-string " FN"
  "*String to display in modes section of the mode-line."
  :group 'footnote)

(defcustom footnote-mode-hook nil
  "*Hook functions run when footnote-mode is activated."
  :type 'hook
  :group 'footnote)

(defcustom footnote-narrow-to-footnotes-when-editing nil
  "*If set, narrow to footnote text body while editing a footnote."
  :type 'boolean
  :group 'footnote)

(defcustom footnote-prompt-before-deletion t
  "*If set, prompt before deleting a footnote.
There is currently no way to undo deletions."
  :type 'boolean
  :group 'footnote)

(defcustom footnote-spaced-footnotes t
  "If set true it will put a blank line between each footnote.
If nil, no blank line will be inserted."
  :type 'boolean
  :group 'footnote)

(defcustom footnote-always-blank-line-before-signature t
  "If set true, a blank line will always be inserted before signature.
This has visible effect only when `footnote-spaced-footnotes' is not true."
  :type 'boolean
  :group 'footnote)

;;; FIXME!  Is numeric-latin a reasonable name?  In what ISO 8859-* character
;;; sets are these superscripts present?  
(defcustom footnote-style 'numeric
  "*Style used for footnoting.
numeric == 1, 2, 3, ...
numeric-latin == {¹, ², ³} | {1, 2, 3, 4, ...}
english-lower == a, b, c, ...
english-upper == A, B, C, ...
roman-lower == i, ii, iii, iv, v, ...
roman-upper == I, II, III, IV, V, ...
Some versions of XEmacs and Emacs/mule may support further styles
like 'hebrew, 'greek-lower, and 'greek-upper."
  :type 'symbol
  :group 'footnote)

(defcustom footnote-use-message-mode t
  "*If non-nil assume Footnoting will be done in message-mode."
  :type 'boolean
  :group 'footnote)

(defcustom footnote-body-tag-spacing 2
  "*Number of blanks separating a footnote body tag and its text."
  :type 'integer
  :group 'footnote)

;;;###autoload
(defvar footnote-prefix [(control ?c) ?!]
  "*When not using message mode, the prefix to bind in `mode-specific-map'")

;;; Interface variables that probably shouldn't be changed

;; FIXME!  Make this customize-able?
(defvar footnote-section-tag "Footnotes: "
  "*Tag inserted at beginning of footnote section.")

;; (defconst footnote-section-tag-regexp "Footnotes\\(\\[.\\]\\)?: "
;; FIXME!  What is this                            ^^^^^^^^^^^^^^
;; doing here?
(defvar footnote-section-tag-regexp footnote-section-tag
  "*Regexp which indicates the start of a footnote section.")

;; FIXME!  Make these customize-able?
(defvar footnote-start-tag "["
  "*String used to denote start of numbered footnote.")

(defvar footnote-end-tag "]"
  "*String used to denote end of numbered footnote.")

(defvar footnote-signature-separator (if (boundp 'message-signature-separator)
					 message-signature-separator
				       "^-- $")
  "*String used to recognize .signatures.")

;;; Private variables

(defvar footnote-text-marker-alist nil
  "List of markers pointing to text of footnotes in message buffer.")
(make-variable-buffer-local 'footnote-text-marker-alist)

(defvar footnote-pointer-marker-alist nil
  "List of markers pointing to footnote pointers in message buffer.")
(make-variable-buffer-local 'footnote-pointer-marker-alist)

(defvar footnote-mouse-highlight 'highlight
  "Text property name to enable mouse over highlight.")

(defvar footnote-mode nil
  "Variable indicating whether footnote minor mode is active.")
(make-variable-buffer-local 'footnote-mode)

(defvar footnote-style-is-really-numeric-latin nil
  "Whether current numeric style is actually numeric-latin.")
(make-variable-buffer-local 'footnote-style-is-really-numeric-latin)

;;; Default styles
;;; NUMERIC-LATIN
(defconst footnote-numeric-latin-regexp "[¹²³]"
  "Regexp for Latin superscript digits.")

(defun Footnote-numeric-latin (n)
  "Numeric-latin footnote style.
Use latin superscript digits if no more than three footnotes."
  (case n
    (1 "¹")
    (2 "²")
    (3 "³")
    (t (error "This should never happen."))))

;;; NUMERIC
(defconst footnote-numeric-regexp "[0-9]"
  "Regexp for digits.")

(defun Footnote-numeric (n)
  "Numeric footnote style.
Use Arabic numerals for footnoting."
  (int-to-string n))

;;; ENGLISH UPPER
(defconst footnote-english-upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "Upper case English alphabet.")

(defconst footnote-english-upper-regexp "[A-Z]"
  "Regexp for upper case English alphabet.")

(defun Footnote-english-upper (n)
  "Upper case English footnoting.
Wrapping around the alphabet implies successive repetitions of letters."
  (let* ((ltr (mod (1- n) (length footnote-english-upper)))
	 (rep (/ (1- n) (length footnote-english-upper)))
	 (chr (char-to-string (aref footnote-english-upper ltr)))
	 rc)
    (while (>= rep 0)
      (setq rc (concat rc chr))
      (setq rep (1- rep)))
    rc))
  
;;; ENGLISH LOWER
(defconst footnote-english-lower "abcdefghijklmnopqrstuvwxyz"
  "Lower case English alphabet.")

(defconst footnote-english-lower-regexp "[a-z]"
  "Regexp of lower case English alphabet.")

(defun Footnote-english-lower (n)
  "Lower case English footnoting.
Wrapping around the alphabet implies successive repetitions of letters."
  (let* ((ltr (mod (1- n) (length footnote-english-lower)))
	 (rep (/ (1- n) (length footnote-english-lower)))
	 (chr (char-to-string (aref footnote-english-lower ltr)))
	 rc)
    (while (>= rep 0)
      (setq rc (concat rc chr))
      (setq rep (1- rep)))
    rc))

;;; ROMAN LOWER
(defconst footnote-roman-lower-list
  '((1 . "i") (5 . "v") (10 . "x")
    (50 . "l") (100 . "c") (500 . "d") (1000 . "m"))
  "List of roman numerals with their values.")

(defconst footnote-roman-lower-regexp "[ivxlcdm]"
  "Regexp of roman numerals.")

(defun Footnote-roman-lower (n)
  "Generic Roman number footnoting."
  (Footnote-roman-common n footnote-roman-lower-list))

;;; ROMAN UPPER
(defconst footnote-roman-upper-list
  '((1 . "I") (5 . "V") (10 . "X")
    (50 . "L") (100 . "C") (500 . "D") (1000 . "M"))
  "List of roman numerals with their values.")

(defconst footnote-roman-upper-regexp "[IVXLCDM]"
  "Regexp of roman numerals.  Not complete")

(defun Footnote-roman-upper (n)
  "Generic Roman number footnoting."
  (Footnote-roman-common n footnote-roman-upper-list))

(defun Footnote-roman-common (n footnote-roman-list)
  "Lower case Roman footnoting."
  (let* ((our-list footnote-roman-list)
	 (rom-lngth (length our-list))
	 (rom-high 0)
	 (rom-low 0)
	 (rom-div -1)
	 (count-high 0)
	 (count-low 0))
    ;; find surrounding numbers
    (while (and (<= count-high (1- rom-lngth))
		(>= n (car (nth count-high our-list))))
      ;; (message "Checking %d" (car (nth count-high our-list)))
      (setq count-high (1+ count-high)))
    (setq rom-high count-high)
    (setq rom-low (1- count-high))
    ;; find the appropriate divisor (if it exists)
    (while (and (= rom-div -1)
		(< count-low rom-high))
      (when (or (> n (- (car (nth rom-high our-list))
			(/ (car (nth count-low our-list))
			   2)))
		(= n (- (car (nth rom-high our-list))
			(car (nth count-low our-list)))))
	(setq rom-div count-low))
      ;; (message "Checking %d and %d in div loop" rom-high count-low)
      (setq count-low (1+ count-low)))
    ;;(message "We now have high: %d, low: %d, div: %d, n: %d"
    ;;	       rom-high rom-low (if rom-div rom-div -1) n)
    (let ((rom-low-pair (nth rom-low our-list))
	  (rom-high-pair (nth rom-high our-list))
	  (rom-div-pair (if (not (= rom-div -1)) (nth rom-div our-list) nil)))
      ;; (message "pairs are: rom-low: %S, rom-high: %S, rom-div: %S"
      ;;	  rom-low-pair rom-high-pair rom-div-pair)
      (cond
       ((< n 0) (error "Footnote-roman-common called with n < 0"))
       ((= n 0) "")
       ((= n (car rom-low-pair)) (cdr rom-low-pair))
       ((= n (car rom-high-pair)) (cdr rom-high-pair))
       ((= (car rom-low-pair) (car rom-high-pair))
	(concat (cdr rom-low-pair)
		(Footnote-roman-common
		 (- n (car rom-low-pair))
		 footnote-roman-list)))
       ((>= rom-div 0) (concat (cdr rom-div-pair) (cdr rom-high-pair)
			       (Footnote-roman-common
				(- n (- (car rom-high-pair)
					(car rom-div-pair)))
				footnote-roman-list)))
       (t (concat (cdr rom-low-pair)
		  (Footnote-roman-common
		   (- n (car rom-low-pair))
		   footnote-roman-list)))))))

;;; list of all footnote styles
(defvar footnote-style-alist
  `((numeric       Footnote-numeric       ,footnote-numeric-regexp)
    (numeric-latin Footnote-numeric-latin ,footnote-numeric-latin-regexp
                   ""                     "")
    (english-lower Footnote-english-lower ,footnote-english-lower-regexp)
    (english-upper Footnote-english-upper ,footnote-english-upper-regexp)
    (roman-lower   Footnote-roman-lower   ,footnote-roman-lower-regexp)
    (roman-upper   Footnote-roman-upper   ,footnote-roman-upper-regexp))
  "Styles of footnote tags available.
By default only boring Arabic numbers, latin superscript numbers, English
letters and Roman Numerals are available.
See footnote-han.el, footnote-greek.el and footnote-hebrew.el for more
exciting styles.")

;;; Style utilities & functions

(defun* Footnote-start-tag (&optional (style footnote-style))
  (or (fourth (assq style footnote-style-alist))
                  footnote-start-tag))

(defun* Footnote-end-tag (&optional (style footnote-style))
  (or (fifth (assq style footnote-style-alist))
      footnote-end-tag))

(defun Footnote-style-p (style)
  "Return non-nil if style is a valid style known to footnote-mode."
  (assq style footnote-style-alist))

(defun Footnote-index-to-string (index)
  "Convert a binary index into a string to display as a footnote.
Conversion is done based upon the current selected style."
  (let ((alist (if (Footnote-style-p footnote-style)
		   (assq footnote-style footnote-style-alist)
		 (first footnote-style-alist))))
    (funcall (second alist) index)))

(defun Footnote-current-regexp ()
  "Return the regexp of the index of the current style."
  (concat (third (or (assq footnote-style footnote-style-alist)
		     (first footnote-style-alist))) "+"))

(defun* Footnote-refresh-footnotes (&optional (old-style
                                               (assq footnote-style
                                                     footnote-style-alist)))
  "Redraw all footnotes.
The old footnotes are assumed to be of OLD-STYLE and the style they will
be redrawn is the current one.  You must call this or arrange to have this
called after changing footnote styles."
  (let ((old-ind-regexp (third old-style))
        (old-start-tag  (Footnote-start-tag (first old-style)))
        (old-end-tag    (Footnote-end-tag (first old-style)))
        (new-start-tag  (Footnote-start-tag))
        (new-end-tag    (Footnote-end-tag)))
    (save-excursion
      ;; Take care of the pointers first
      (loop for i from 1
            for footnote-entry in footnote-pointer-marker-alist
            do (loop for location in (cdr footnote-entry)
                     do
                     (setf (point) location)
                     (search-backward-regexp (concat (regexp-quote old-start-tag)
                                                     old-ind-regexp)
                                             nil t)
                     (when (looking-at (concat
                                        (regexp-quote old-start-tag)
                                        "\\(" old-ind-regexp "+\\)"
                                        (regexp-quote old-end-tag)))
                       (replace-match (concat
                                       new-start-tag
                                       (Footnote-index-to-string i)
                                       new-end-tag)
                                      nil "\\1"))))

      ;; Now take care of the text section
      (loop for i from 1
            for footnote-entry in footnote-text-marker-alist
            do (setf (point) (cdr footnote-entry))
               (when (looking-at (concat
                                  (regexp-quote old-start-tag)
                                  "\\(" old-ind-regexp "+\\)"
                                  (regexp-quote old-end-tag)))
                 (replace-match (concat
                                 new-start-tag
                                 (Footnote-index-to-string i)
                                 new-end-tag)
                                nil "\\1"))))))

(defun Footnote-assoc-index (key alist)
  "Give index of key in alist."
  (position key alist :key #'first))    ; Look ma, no headache!

(defun Footnote-switch-style (to)
  "Switch to another footnote style TO, refreshing the footnotes."
  (let ((old (assq footnote-style footnote-style-alist)))
    (cond ((and (eq to 'numeric-latin)
                (>= (length footnote-text-marker-alist) 4))
           (message "You won't see latin superscripts until you have less than four footnotes.")
           (Footnote-set-style 'numeric)
           (setq footnote-style-is-really-numeric-latin t))
          (t (Footnote-set-style to)))
    (Footnote-refresh-footnotes old)))

(defun* Footnote-cycle-style (&optional (arg 1))
  "Select ARG'th (from the current one) defined footnote style."
  (interactive "p")
  (Footnote-switch-style
   (first (nth (mod (+ (Footnote-assoc-index footnote-style
                                             footnote-style-alist)
                       arg)
                    (length footnote-style-alist))
               footnote-style-alist))))

(defun Footnote-set-style (&optional style)
  "Select a specific style."
  (interactive
   (list (intern (completing-read
		  "Footnote Style: "
		  obarray #'Footnote-style-p 'require-match))))
  (setq footnote-style-is-really-numeric-latin nil)
  (setq footnote-style style))

;; Internal functions
(defun Footnote-insert-numbered-footnote (arg &optional mousable)
  "Insert numbered footnote at (point)."
  (let* ((start (point))
	 (end (progn
		(insert-before-markers (concat (Footnote-start-tag)
					       (Footnote-index-to-string arg)
					       (Footnote-end-tag)))
		(point))))

    (add-text-properties start end
			 (list 'footnote-number arg))
    (when mousable
      (add-text-properties start end
			   (list footnote-mouse-highlight t)))))

(defun Footnote-renumber (from to pointer-alist text-alist)
  "Renumber a single footnote."
  (let* ((posn-list (cdr pointer-alist)))
    (setcar pointer-alist to)
    (setcar text-alist to)
    (while posn-list
      (goto-char (car posn-list))
      (search-backward (Footnote-start-tag) nil t)
      (when (looking-at (format "%s%s%s"
				(regexp-quote (Footnote-start-tag))
				(Footnote-current-regexp)
				(regexp-quote (Footnote-end-tag))))
	(add-text-properties (match-beginning 0) (match-end 0)
			     (list 'footnote-number to))
	(replace-match (format "%s%s%s"
			       (Footnote-start-tag)
			       (Footnote-index-to-string to)
			       (Footnote-end-tag))))
      (setq posn-list (cdr posn-list)))
    (goto-char (cdr text-alist))
    (when (looking-at (format "%s%s%s"
			      (regexp-quote (Footnote-start-tag))
			      (Footnote-current-regexp)
			      (regexp-quote (Footnote-end-tag))))
      (add-text-properties (match-beginning 0) (match-end 0)
			   (list 'footnote-number to))
      (replace-match (format "%s%s%s"
			     (Footnote-start-tag)
			     (Footnote-index-to-string to)
			     (Footnote-end-tag)) nil t))))

;; Not needed?
(defun Footnote-narrow-to-footnotes ()
  "Restrict text in buffer to show only text of footnotes."
  (interactive)	; testing
  (goto-char (point-max))
  (when (re-search-backward footnote-signature-separator nil t)
    (let ((end (point)))
      (when (re-search-backward (concat "^" footnote-section-tag-regexp) nil t)
	(narrow-to-region (point) end)))))

(defun Footnote-goto-char-point-max ()
  "Move to end of buffer or prior to start of .signature."
  (goto-char (point-max))
  (or (re-search-backward footnote-signature-separator nil t)
      (point)))

(defun Footnote-insert-text-marker (arg locn)
  "Insert a marker pointing to footnote arg, at buffer location locn."
  (let ((marker (make-marker)))
    (unless (assq arg footnote-text-marker-alist)
      (set-marker marker locn)
      (setq footnote-text-marker-alist
	    (acons arg marker footnote-text-marker-alist))
      (setq footnote-text-marker-alist
	    (Footnote-sort footnote-text-marker-alist)))))

(defun Footnote-insert-pointer-marker (arg locn)
  "Insert a marker pointing to footnote arg, at buffer location locn."
  (let ((marker (make-marker))
	alist)
    (set-marker marker locn)
    (if (setq alist (assq arg footnote-pointer-marker-alist))
	(setf alist
	      (cons marker (cdr alist)))
      (setq footnote-pointer-marker-alist
	    (acons arg (list marker) footnote-pointer-marker-alist))
      (setq footnote-pointer-marker-alist
	    (Footnote-sort footnote-pointer-marker-alist)))))

(defun Footnote-insert-footnote (arg)
  "Insert a footnote numbered arg, at (point)."
  (push-mark)
  (Footnote-insert-pointer-marker arg (point))
  (Footnote-insert-numbered-footnote arg t)
  (Footnote-goto-char-point-max)
  (let (insertion-before-signature)
    (if (re-search-backward (concat "^" footnote-section-tag-regexp) nil t)
        (save-restriction
          (when footnote-narrow-to-footnotes-when-editing 
            (Footnote-narrow-to-footnotes))
          (Footnote-goto-footnote (1- arg)) ; evil, FIXME (less evil now)
          (or (= arg 1)
              (when (re-search-forward (if footnote-spaced-footnotes
                                           "\n\n"
                                         (concat "\n"
                                                 (regexp-quote (Footnote-start-tag))
                                                 (Footnote-current-regexp)
                                                 (regexp-quote (Footnote-end-tag))))
                                       nil t)
                (beginning-of-line)
                t)
              (progn (Footnote-goto-char-point-max)
                     (when (and (setq insertion-before-signature
                                      (looking-at footnote-signature-separator))
                                (not footnote-spaced-footnotes)
                                footnote-always-blank-line-before-signature)
                       (forward-line -1)
                       (when (looking-at "^$")
                         (delete-char 1))))))
      ;; Need to create footnote section.
      (setq insertion-before-signature (looking-at footnote-signature-separator))
      (unless (looking-at "^$")
        (insert "\n"))
      (when (eobp)
        (insert "\n"))
      (insert footnote-section-tag "\n"))
    (let ((old-point (point)))
      (Footnote-insert-numbered-footnote arg nil)
      (Footnote-insert-text-marker arg old-point))
    insertion-before-signature))

(defun Footnote-sort (list)
  (sort list (lambda (e1 e2)
	       (< (car e1) (car e2)))))

(defun Footnote-text-under-cursor ()
  "Return the number of footnote if in footnote text.
Nil is returned if the cursor is not positioned over the text of
a footnote."
  (when (and (let ((old-point (point)))
	       (save-excursion
		 (save-restriction
		   (Footnote-narrow-to-footnotes)
		   (and (>= old-point (point-min))
			(<= old-point (point-max))))))
	     (>= (point) (cdar footnote-text-marker-alist)))
    (let ((i 1)
	  alist-txt rc)
      (while (and (setq alist-txt (nth i footnote-text-marker-alist))
		  (null rc))
	(when (< (point) (cdr alist-txt))
	  (setq rc (car (nth (1- i) footnote-text-marker-alist))))
	(setq i (1+ i)))
      (when (and (null rc)
		 (null alist-txt))
	(setq rc (car (nth (1- i) footnote-text-marker-alist))))
      rc)))

(defun Footnote-under-cursor ()
  "Return the number of the footnote underneath the cursor.
Nil is returned if the cursor is not over a footnote."
  (or (get-text-property (point) 'footnote-number)
      (Footnote-text-under-cursor)))

;;; User functions

(defun Footnote-make-hole ()
  (save-excursion
    (let ((i 0)
	  (notes (length footnote-pointer-marker-alist))
	  alist-ptr alist-txt rc)
      (while (< i notes)
	(setq alist-ptr (nth i footnote-pointer-marker-alist))
	(setq alist-txt (nth i footnote-text-marker-alist))
	(when (< (point) (- (cadr alist-ptr) 3))
	  (unless rc
	    (setq rc (car alist-ptr)))
	  (save-excursion
	    (message "Renumbering from %s to %s" 
		     (Footnote-index-to-string (car alist-ptr))
		     (Footnote-index-to-string 
		      (1+ (car alist-ptr))))
	    (Footnote-renumber (car alist-ptr)
			       (1+ (car alist-ptr))
			       alist-ptr
			       alist-txt)))
	(setq i (1+ i)))
      rc)))

;;;###autoload
(defun Footnote-add-footnote (&optional arg)
  "Add a numbered footnote.
The number the footnote receives is dependent upon the relative location
of any other previously existing footnotes.
If the variable `footnote-narrow-to-footnotes-when-editing' is set,
the buffer is narrowed to the footnote body.  The restriction is removed
by using `Footnote-back-to-message'."
  (interactive "*P")
  (when (and (eq footnote-style 'numeric-latin)
             (= 3 (length footnote-text-marker-alist)))
    (Footnote-switch-style 'numeric)
    (setq footnote-style-is-really-numeric-latin t))
  (let ((num (if footnote-text-marker-alist
                 (if (< (point) (cadar (last footnote-pointer-marker-alist)))
                     (Footnote-make-hole)
                   (1+ (caar (last footnote-text-marker-alist))))
               1)))
    (message "Adding footnote %d" num)
    (let ((insertion-before-signature (Footnote-insert-footnote num)))
      (insert-before-markers (make-string footnote-body-tag-spacing ? ))
      (let ((opoint (point)))
        (save-excursion
          (insert-before-markers
           (if (or footnote-spaced-footnotes
                   (and footnote-always-blank-line-before-signature
                        insertion-before-signature))
               "\n\n"
             "\n"))
          (when footnote-narrow-to-footnotes-when-editing
            (Footnote-narrow-to-footnotes)))
        ;; Emacs/XEmacs bug?  save-excursion doesn't restore point when using
        ;; insert-before-markers.
        (goto-char opoint)))))

(defun Footnote-delete-footnote (&optional arg)
  "Delete a numbered footnote.
With no parameter, delete the footnote under (point).  With arg specified,
delete the footnote with that number."
  (interactive "*P")
  (let ((arg (or arg (Footnote-under-cursor))))
    (when (and arg
               (or (not footnote-prompt-before-deletion)
                   (y-or-n-p (format "Really delete footnote %d?" arg))))
      (let* ((alist-ptr      (assq arg footnote-pointer-marker-alist))
             (alist-at-entry (member* arg footnote-text-marker-alist :key #'car))
             (alist-txt      (first alist-at-entry)))
        (unless (and alist-ptr alist-txt)
          (error "Can't delete footnote %d" arg))
        (loop for location in (cdr alist-ptr)
              do (save-excursion
                   (setf (point) location)
                   (kill-region (search-backward-regexp
                                 (concat (regexp-quote (Footnote-start-tag))
                                         (Footnote-current-regexp))
                                 nil t)
                                location)))
        (save-excursion
          (if (rest alist-at-entry)
              (Footnote-goto-footnote (1+ arg))
            (Footnote-goto-char-point-max)
            (unless (or (looking-at "^$")
                        footnote-spaced-footnotes
                        footnote-always-blank-line-before-signature))
            (when (and (not (looking-at "^$"))
                       (not footnote-spaced-footnotes)
                       footnote-always-blank-line-before-signature)
              (forward-line -1)))
          (kill-region (cdr alist-txt)
                       (point)))
        (setq footnote-pointer-marker-alist
              (delq alist-ptr footnote-pointer-marker-alist))
        (setq footnote-text-marker-alist
              (delq alist-txt footnote-text-marker-alist))
        (Footnote-renumber-footnotes)
        (when (and footnote-style-is-really-numeric-latin
                   (eq footnote-style 'numeric)
                   (< (length footnote-text-marker-alist) 4))
          (Footnote-switch-style 'numeric-latin))
        (when (and (null footnote-text-marker-alist)
                   (null footnote-pointer-marker-alist))
          (save-excursion
            (let ((end   (Footnote-goto-char-point-max))
                  (start (1- (re-search-backward
                              (concat "^" footnote-section-tag-regexp)
                              nil t))))
              (forward-line -1)
              (let ((end (- end (cond ((looking-at "\n")
                                       (kill-line)
                                       1)
                                      (t 0)))))
                (kill-region start (if (< end (point-max))
                                       end
                                     (point-max)))))))))))

(defun Footnote-renumber-footnotes (&optional arg)
  "Renumber footnotes, starting from 1."
  (interactive "*P")
  (save-excursion
    (let ((i 0)
	  (notes (length footnote-pointer-marker-alist))
	  alist-ptr alist-txt)
      (while (< i notes)
	(setq alist-ptr (nth i footnote-pointer-marker-alist))
	(setq alist-txt (nth i footnote-text-marker-alist))
	(unless (eq (1+ i) (car alist-ptr))
	  (Footnote-renumber (car alist-ptr) (1+ i) alist-ptr alist-txt))
	(setq i (1+ i))))))

(defun Footnote-goto-footnote (&optional arg)
  "Jump to the text of a footnote.
With no parameter, jump to the text of the footnote under (point).  With arg
specified, jump to the text of that footnote."
  (interactive "P")
  (setq zmacs-region-stays t)
  (let (footnote)
    (if arg
	(setq footnote (assq arg footnote-text-marker-alist))
      (when (setq arg (Footnote-under-cursor))
	(setq footnote (assq arg footnote-text-marker-alist))))
    (if footnote
	(goto-char (cdr footnote))
      (if (= arg 0)
	  (progn
	    (goto-char (point-max))
	    (re-search-backward (concat "^" footnote-section-tag-regexp))
	    (forward-line 1))
	(error "I don't see a footnote here.")))))

(defun Footnote-back-to-message (&optional arg)
  "Move cursor back to footnote referent.
If the cursor is not over the text of a footnote, point is not changed.
If the buffer was narrowed due to `footnote-narrow-to-footnotes-when-editing'
being set it is automatically widened."
  (interactive "P")
  (setq zmacs-region-stays t)
  (let ((note (Footnote-text-under-cursor)))
    (when note
      (when footnote-narrow-to-footnotes-when-editing
	(widen))
      (goto-char (cadr (assq note footnote-pointer-marker-alist))))))

;;;###autoload
(defvar footnote-mode-map nil
  "Keymap used for footnote minor mode.")

;; Set up our keys
;;;###autoload
(unless footnote-mode-map
  (setq footnote-mode-map (make-sparse-keymap))
  (define-key footnote-mode-map "a" 'Footnote-add-footnote)
  (define-key footnote-mode-map "b" 'Footnote-back-to-message)
  (define-key footnote-mode-map "c" 'Footnote-cycle-style)
  (define-key footnote-mode-map "d" 'Footnote-delete-footnote)
  (define-key footnote-mode-map "g" 'Footnote-goto-footnote)
  (define-key footnote-mode-map "r" 'Footnote-renumber-footnotes)
  (define-key footnote-mode-map "s" 'Footnote-set-style))

;;;###autoload
(defvar footnote-minor-mode-map nil
  "Keymap used for binding footnote minor mode.")

;;;###autoload
(unless footnote-minor-mode-map
  (define-key global-map footnote-prefix footnote-mode-map))

;;;###autoload
(defun footnote-mode (&optional arg)
  "Toggle footnote minor mode.
\\<message-mode-map>
key		binding
---		-------

\\[Footnote-renumber-footnotes]		Footnote-renumber-footnotes
\\[Footnote-goto-footnote]		Footnote-goto-footnote
\\[Footnote-delete-footnote]		Footnote-delete-footnote
\\[Footnote-cycle-style]		Footnote-cycle-style
\\[Footnote-back-to-message]		Footnote-back-to-message
\\[Footnote-add-footnote]		Footnote-add-footnote
"
  (interactive "*P")
  ;; (filladapt-mode t)
  (setq zmacs-region-stays t)
  (setq footnote-mode
	(if (null arg) (not footnote-mode)
	  (> (prefix-numeric-value arg) 0)))
  (when footnote-mode
    ;; (Footnote-setup-keybindings)
    (make-local-variable 'footnote-style)
    (if (fboundp 'force-mode-line-update)
	(force-mode-line-update)
      (set-buffer-modified-p (buffer-modified-p)))

    (when (boundp 'filladapt-token-table)
      ;; add tokens to filladapt to match footnotes
      ;; 1] xxxxxxxxxxx x x x or [1] x x x x x x x
      ;;    xxx x xx xxx xxxx	     x x x xxxxxxxxxx
      (let ((bullet-regexp (concat (regexp-quote (Footnote-start-tag))
				   "?[0-9a-zA-Z]+"
				   (regexp-quote (Footnote-end-tag))
				   "[ \t]")))
	(unless (assoc bullet-regexp filladapt-token-table)
	  (setq filladapt-token-table
		(append filladapt-token-table
			(list (list bullet-regexp 'bullet)))))))

    (run-hooks 'footnote-mode-hook)))

;; install on minor-mode-alist
;;;###autoload
(when (fboundp 'add-minor-mode)
    ;; XEmacs
    (add-minor-mode 'footnote-mode
		    footnote-mode-line-string
		    footnote-minor-mode-map))

;; Emacs -- don't autoload
(unless (assq 'footnote-mode minor-mode-alist)
  (setq minor-mode-alist
	(cons '(footnote-mode footnote-mode-line-string)
	      minor-mode-alist)))

(provide 'footnote)

;;; footnote.el ends here


