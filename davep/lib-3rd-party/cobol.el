;;; Cobol mode for GNU Emcas (version 2.00, Mar 01, 2000)
;;; There does not seem to be a maintainer for this mode, and none
;;; of the email addresses for it are good.  So, I've made it
;;; what I think it should be.  This includes adding
;;; color syntax highlighting (basically the only major change).
;;;
;;; Matthew Vanecek <linuxguy@directlink.net>
;;; Please feel free to send suggestions or improvements.
;;; 
;;; The changelog is down at the bottom.
;;;
;;; Cobol mode for GNU Emacs (version 1.01, Jun 21, 1988)
;;; Copyright (c) 1987 Free Software Foundation, Inc.
;;; Written by Robert A Sutterfield (bob@cis.ohio-state.edu) and
;;;  Paul W. Placeway (paul@tut.cis.ohio-state.edu), as changes to fortran.el
;;; Bugs to bug-cobol-mode@cis.ohio-state.edu

;;;    [0) the left column is column 1]
;;;  +  1) newline should indent to the same column as the start of
;;;        the previous line
;;;  +  2) tabs at 8 and every four thereafter (12, 16, 20, etc.)
;;;  +  3) tabs should be expanded to spaces on input
;;;  +  (3a) no tabs should appear in the buffer
;;;  no 4) right margin bell at 72 (hard to do)
;;;  +  5) (optional) flash matching parentheses
;;;  +  6) no auto-fill (WHY -- PWP) (not by default)
;;;  *  7) auto startup on .cob files
;;;	   To do this, the expression ("\\.cob$" . cobol-mode) must be
;;;	   added to loaddefs.el in the gnu-emacs lisp directory, and
;;;	   loaddefs must be re-byte-code-compiled.
;;;	   Also, an autoload must be set up for cobol-mode in loaddefs.el;
;;;	   see the loaddefs.el file in this directory.
;;;  +  8) auto indent to that of the last line (more magic than that...)
;;;  +  9) delete on a blank line should go back to LAST tab stop
;;;  + 10) C-c C-c moves cursor to ARG (or prompted) column, adding
;;;        spaces to get there if needed
;;;    11) C-c C-l does (goto-line)
;;;
;;; COBOL mode adapted from:
;;;: Fortran mode for GNU Emacs  (beta test version 1.21, Oct. 1, 1985)
;;;; Copyright (c) 1986 Free Software Foundation, Inc.
;;;; Written by Michael D. Prange (mit-eddie!mit-erl!prange).
;;;; Author acknowledges help from Stephen Gildea <mit-erl!gildea>

;; This file is not part of the GNU Emacs distribution (yet).

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; Bugs to bug-cobol-mode@cis.ohio-state.edu.
(defvar cobol-do-indent 4
  "*Extra indentation applied to `do' blocks.")

(defvar cobol-if-indent 4
  "*Extra indentation applied to `if' blocks.")

(defvar cobol-continuation-indent 6
  "*Extra indentation applied to `continuation' lines.")

(defvar cobol-pic-column 50
  "*The column that PIC clauses should be aligned to.")

(defvar cobol-indent-increment 4
  "*Amount of indentation to add to a line when it can be indented.")

(defvar cobol-comment-indent-style 'fixed
  "*nil forces comment lines not to be touched,
'fixed produces fixed comment indentation to comment-column,
and 'relative indents to current cobol indentation plus comment-column.")

(defvar cobol-comment-line-column 6
  "*Indentation for text in comment lines.")

(defvar comment-line-start "**"
  "*Delimiter inserted to start new full-line comment.")

(defvar comment-line-start-skip "^......\\*"
  "*Regexp to match the start of a full-line comment.")

(defvar cobol-minimum-statement-indent 7 	;;; this puts it in column 8
  "*Minimum indentation for cobol statements.")

(defvar cobol-minimum-area-b-indent 11 	;;; this puts it in column 12
  "*Minimum indentation for cobol statements that should be in Area B.")

;; Note that this is documented in the v18 manuals as being a string
;; of length one rather than a single character.
;; The code in this file accepts either format for compatibility.
(defvar cobol-comment-indent-char " "
  "*Character to be inserted for Cobol comment indentation.
Normally a space.")

(defvar cobol-line-number-indent 1
  "*Maximum indentation for Cobol line numbers.
6 means right-justify them within their six-column field.")

(defvar cobol-check-all-num-for-matching-do nil
  "*Non-nil causes all numbered lines to be treated as possible do-loop ends.")

(defvar cobol-continuation-char ?-
  "*Character which is inserted in column 7 by \\[cobol-split-line]
to begin a continuation line.  Normally ?-")

(defvar cobol-comment-region "      ** "
  "*String inserted by \\[cobol-comment-region] at start of each line in region.")

(defvar cobol-electric-line-number t
  "*Non-nil causes line number digits to be moved to the correct column as typed.")

(defvar cobol-startup-message t
  "*Non-nil displays a startup message when cobol-mode is first called.")

(defvar cobol-column-ruler
  (concat "0    00  1         2         3         4         5         6         7  2\n"
	  "1.../67..0..../....0..../....0..../....0..../....0..../....0..../....0..\n")
  "*String displayed above current line by \\[cobol-column-ruler].")

(defconst cobol-mode-version "2.00")

(defvar cobol-mode-syntax-table nil
  "Syntax table in use in cobol-mode buffers.")

(if cobol-mode-syntax-table ()
  (setq cobol-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\; "w" cobol-mode-syntax-table)
  (modify-syntax-entry ?+ "." cobol-mode-syntax-table)
;  (modify-syntax-entry ?- "." cobol-mode-syntax-table)
; Turn a dash into a word character
  (modify-syntax-entry ?- "w" cobol-mode-syntax-table)
;  (modify-syntax-entry ?* "." cobol-mode-syntax-table)
; We need a "*" to mark the beginning of a comment
; Apostrophe's use in comments are treated as open quotes without this
  (modify-syntax-entry ?* "<" cobol-mode-syntax-table)
  (modify-syntax-entry ?/ "." cobol-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" cobol-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" cobol-mode-syntax-table)
; '\' should not be an escape character in COBOL
  (modify-syntax-entry ?\\ "_" cobol-mode-syntax-table)
  (modify-syntax-entry ?. "w" cobol-mode-syntax-table)
  (modify-syntax-entry ?\n ">" cobol-mode-syntax-table))

(defvar cobol-mode-map () 
  "Keymap used in cobol mode.")

(if cobol-mode-map ()
  (setq cobol-mode-map (make-sparse-keymap)) ; this SHOULD be a real keymap
  (define-key cobol-mode-map ";" 'cobol-abbrev-start)
  (define-key cobol-mode-map "\C-c;" 'cobol-comment-region)
  (define-key cobol-mode-map "\e\C-a" 'beginning-of-cobol-subprogram)
  (define-key cobol-mode-map "\e\C-e" 'end-of-cobol-subprogram)
  (define-key cobol-mode-map "\e;" 'cobol-indent-comment)
  (define-key cobol-mode-map "\e\C-h" 'mark-cobol-subprogram)
  (define-key cobol-mode-map "\e\n" 'cobol-split-line)
  (define-key cobol-mode-map "\e\C-q" 'cobol-indent-subprogram)
  (define-key cobol-mode-map "\C-c\C-w" 'cobol-window-create)
  (define-key cobol-mode-map "\C-c\C-r" 'cobol-column-ruler)
  (define-key cobol-mode-map "\C-c\C-p" 'cobol-previous-statement)
  (define-key cobol-mode-map "\C-c\C-n" 'cobol-next-statement)
  (define-key cobol-mode-map "\C-c\C-c" 'cobol-goto-column)
  (define-key cobol-mode-map "\C-cc" 'cobol-goto-column) ; avoid confusion
  (define-key cobol-mode-map "\C-c\C-l" 'goto-line) ; for Sam
  (define-key cobol-mode-map "\C-cl" 'goto-line) ; avoid confusion
  (define-key cobol-mode-map "\t" 'cobol-indent-line)
  (define-key cobol-mode-map "\C-m" 'newline-and-indent) ; magic RET key
  (let ((n ?\ ))
    (while (< n 127)
      (define-key cobol-mode-map (char-to-string n) 'cobol-self-insert)
      (setq n (1+ n))))
  (define-key cobol-mode-map "\177" 'cobol-back-delete) ; magic DEL key too
;  (define-key cobol-mode-map "0" 'cobol-electric-line-number)
;  (define-key cobol-mode-map "1" 'cobol-electric-line-number)
;  (define-key cobol-mode-map "2" 'cobol-electric-line-number)
;  (define-key cobol-mode-map "3" 'cobol-electric-line-number)
;  (define-key cobol-mode-map "4" 'cobol-electric-line-number)
;  (define-key cobol-mode-map "5" 'cobol-electric-line-number)
;  (define-key cobol-mode-map "6" 'cobol-electric-line-number)
;  (define-key cobol-mode-map "7" 'cobol-electric-line-number)
;  (define-key cobol-mode-map "8" 'cobol-electric-line-number)
;  (define-key cobol-mode-map "9" 'cobol-electric-line-number)

  )

(defvar cobol-mode-abbrev-table nil)
(if cobol-mode-abbrev-table ()
  (define-abbrev-table 'cobol-mode-abbrev-table ())
  (let ((abbrevs-changed nil))
    (define-abbrev cobol-mode-abbrev-table  ";b"   "byte" nil)
    (define-abbrev cobol-mode-abbrev-table  ";ch"  "character" nil)
    (define-abbrev cobol-mode-abbrev-table  ";cl"  "close" nil)
    (define-abbrev cobol-mode-abbrev-table  ";c"   "continue" nil)
    (define-abbrev cobol-mode-abbrev-table  ";cm"  "common" nil)
    (define-abbrev cobol-mode-abbrev-table  ";cx"  "complex" nil)
    (define-abbrev cobol-mode-abbrev-table  ";di"  "dimension" nil)
    (define-abbrev cobol-mode-abbrev-table  ";do"  "double" nil)
    (define-abbrev cobol-mode-abbrev-table  ";dc"  "double complex" nil)
    (define-abbrev cobol-mode-abbrev-table  ";dp"  "double precision" nil)
    (define-abbrev cobol-mode-abbrev-table  ";dw"  "do while" nil)
    (define-abbrev cobol-mode-abbrev-table  ";e"   "else" nil)
    (define-abbrev cobol-mode-abbrev-table  ";ed"  "enddo" nil)
    (define-abbrev cobol-mode-abbrev-table  ";el"  "elseif" nil)
    (define-abbrev cobol-mode-abbrev-table  ";en"  "endif" nil)
    (define-abbrev cobol-mode-abbrev-table  ";eq"  "equivalence" nil)
    (define-abbrev cobol-mode-abbrev-table  ";ex"  "external" nil)
    (define-abbrev cobol-mode-abbrev-table  ";ey"  "entry" nil)
    (define-abbrev cobol-mode-abbrev-table  ";f"   "format" nil)
    (define-abbrev cobol-mode-abbrev-table  ";fu"  "function" nil)
    (define-abbrev cobol-mode-abbrev-table  ";g"   "goto" nil)
    (define-abbrev cobol-mode-abbrev-table  ";im"  "implicit" nil)
    (define-abbrev cobol-mode-abbrev-table  ";ib"  "implicit byte" nil)
    (define-abbrev cobol-mode-abbrev-table  ";ic"  "implicit complex" nil)
    (define-abbrev cobol-mode-abbrev-table  ";ich" "implicit character" nil)
    (define-abbrev cobol-mode-abbrev-table  ";ii"  "implicit integer" nil)
    (define-abbrev cobol-mode-abbrev-table  ";il"  "implicit logical" nil)
    (define-abbrev cobol-mode-abbrev-table  ";ir"  "implicit real" nil)
    (define-abbrev cobol-mode-abbrev-table  ";inc" "include" nil)
    (define-abbrev cobol-mode-abbrev-table  ";in"  "integer" nil)
    (define-abbrev cobol-mode-abbrev-table  ";intr" "intrinsic" nil)
    (define-abbrev cobol-mode-abbrev-table  ";l"   "logical" nil)
    (define-abbrev cobol-mode-abbrev-table  ";op"  "open" nil)
    (define-abbrev cobol-mode-abbrev-table  ";pa"  "parameter" nil)
    (define-abbrev cobol-mode-abbrev-table  ";pr"  "program" nil)
    (define-abbrev cobol-mode-abbrev-table  ";p"   "print" nil)
    (define-abbrev cobol-mode-abbrev-table  ";re"  "real" nil)
    (define-abbrev cobol-mode-abbrev-table  ";r"   "read" nil)
    (define-abbrev cobol-mode-abbrev-table  ";rt"  "return" nil)
    (define-abbrev cobol-mode-abbrev-table  ";rw"  "rewind" nil)
    (define-abbrev cobol-mode-abbrev-table  ";s"   "stop" nil)
    (define-abbrev cobol-mode-abbrev-table  ";su"  "subroutine" nil)
    (define-abbrev cobol-mode-abbrev-table  ";ty"  "type" nil)
    (define-abbrev cobol-mode-abbrev-table  ";w"   "write" nil)))

(defvar not-a-comment "^......[^\\*\\n]")

;(defvar is-a-comment "^......\\*")
(defvar is-a-comment "^ *\\*")

(defvar cobol-usage-words
  "[ \t]+\\(USAGE\\([ \t]+\\(IS\\)?\\)?\\)[ \t]+")

(defvar cobol-modifier-words
  "[ \t]+\\(\\(A\\|DE\\)SCENDING\\([ \t]+KEY\\)?\\|DYNAMIC\\|JUSTIFIED\\)")

(defvar cobol-debugging-statements
  "[ \t]+DEBUG\\(-\\(CONTENTS\\|ITEM\\|LINE\\|NAME\\|SUB\\(-\\[123]\\)\\)\\|GING\\)")

(defvar cobol-data-types
  (concat
  "[ \t]+\\<\\(ALPHA\\(\\(BET\\(IC\\(-LOWER\\|-UPPER\\)?\\)\\)?\\|NUMERIC\\(-EDITED\\)?\\)"
  "\\|BINARY\\(-\\(COB_CHAR\\|SHORT\\|LONG\\|DOUBLE\\)\\)?\\|COMP\\(\\(UTATIONAL\\)?-[0-9Xx]\\)?"
  "\\|NEGATIVE\\|PACKED-DECIMAL\\|POSITIVE\\)[^-A-Za-z0-9]"))

(defvar cobol-scope-delimiters
  (concat
   "[ \t]+\\(END-\\(ADD\\|C\\(ALL\\|OMPUTE\\)\\|D\\(ELETE\\|IVIDE\\)\\|"
   "E\\(VALUATE\\|XEC\\)\\|IF\\|MULTIPLY\\|PERFORM\\|RE\\(AD\\|CEIVE\\|TURN\\|WRITE\\)\\|"
   "S\\(EARCH\\|T\\(ART\\|RING\\)\\|UBTRACT\\)\\|UNSTRING\\|WRITE\\)\\)"))

(defvar cobol-verbs
  (concat
;   "[^-A-Za-z0-9\n]\\<
   "\\b\\(A\\(CCEPT\\|DD\\|ND\\|SSIGN\\|T\\)\\|"
   "C\\(A\\(LL\\(-CONVENTION\\)?\\|NCEL\\)\\|LOSE\\|O\\(MPUTE\\|NTAINS\\|NTINUE\\|PY\\)\\)\\|"
   "D\\(E\\(CLARE\\|ETE\\|PENDING[ \t]+ON\\|LIMITED[ \t]+BY\\)\\|I\\(SPLAY\\|VIDE\\)\\)\\|"
   "E\\(LSE\\|NTRY\\|VALUATE\\|X\\(IT\\|EC[ \t]+SQL\\)\\)\\|F\\(OR\\|ROM\\)\\|"
   "G\\(IVING\\|O\\(BACK\\)?\\)\\|"
   "I\\(F\\|N\\(ITIALIZE\\|TO\\|SPECT\\|DEX\\(ED[ \t]+BY\\)?\\)\\|S\\)\\|LESS\\|"
   "M\\(ERGE\\|OVE\\|ULTIPLY\\)\\|NEXT[ \t]SENTENCE\\|O\\(CCURS\\|PEN\\|R\\)\\|"
   "P\\(ERFORM\\|IC\\|OCOB_INTER\\)\\|"
   "R\\(E\\(AD\\|DEFINES\\|MAINDER\\|NAMES\\|PLAC\\(E\\|ING\\)\\|TURN\\(S\\|ING\\)?\\|WRITE\\)\\|"
   "OUNDED\\|UN\\)\\|"
   "S\\(E\\(ARCH\\|LECT\\|QUENTIAL\\|T\\)\\|ORT\\(-MERGE\\)?\\|PECIAL-NAMES\\|"
   "T\\(OP\\|RING\\|ART\\|ATUS\\)\\)"
   "\\|T\\(ALLYING\\|H\\(EN\\|AN\\|ROUGH\\|RU\\)\\|O\\|RAILING\\)\\|"
   "U\\(N\\(STRING\\|TIL\\)\\|PON\\|S\\(E\\|ING\\)\\)\\|VA\\(LUE\\|RYING\\)\\|"
   "W\\(HEN\\|ITH\\|RITE\\)\\)\\b" ;[ \.\n]" ;[^-_A-Za-z0-9]?"
))

(defvar cobol-divisions
    (concat not-a-comment 
	    "[ \t]*\\(IDENTIFICATION\\|DATA\\|ENVIRONMENT\\|PROCEDURE\\)[ \t]+\\(DIVISION\\)" ))

(defvar cobol-prog-info
  (concat not-a-comment 
	  "[ \t]*\\(AUTHOR\\|PROGRAM-ID\\|DATE-COMPILED\\|DATE-WRITTEN\\|SOURCE-COMPUTER\\|OBJECT-COMPUTER\\)"))

(defvar cobol-sections
  (concat not-a-comment	"[ \t]*\\([-A-Za-z0-9]+\\)[ \t]+\\(SECTION\\)\\."))

(defvar cobol-paragraphs
  (concat not-a-comment "\\([-A-Za-z0-9]+\\)\\.$"))

(defvar cobol-constants
  (concat
   "[ \t]+\\(DATE\\|FALSE\\|FILLER\\|HIGH-VALUE\\(S\\)?\\|LENGTH\\|LOW-VALUE\\(S\\)?\\|"
   "SPACE\\(S\\)?\\|TRUE\\|ZERO\\(S\\|ES\\)?\\)"
   ))

(defvar cobol-font-lock-keywords
  (list
   (list cobol-data-types '(1 font-lock-type-face nil))
   (list cobol-verbs '(1 font-lock-keyword-face nil))
   (list cobol-scope-delimiters '(1 font-lock-keyword-face nil))
   (list cobol-modifier-words '(1 font-lock-keyword-face nil))
   (list cobol-usage-words '(1 font-lock-type-face nil))
   (list cobol-divisions '(1 font-lock-constant-face nil) '(2 font-lock-string-face nil))
   (list cobol-sections '(1 font-lock-constant-face nil) '(2 font-lock-string-face nil))
   (list cobol-paragraphs '(1 font-lock-constant-face nil))
   (list cobol-constants '(1 font-lock-constant-face nil))
   (list cobol-prog-info '(1 font-lock-reference-face nil))
   '("^......\\(\\*.*\\)" 1 font-lock-comment-face t)
   ))


;;;###autoload
(defun cobol-mode ()
  "Major mode for editing cobol code.
Tab indents the current cobol line correctly. 

Type `;?' or `;\\[help-command]' to display a list of built-in abbrevs for Cobol keywords.

Variables controlling indentation style and extra features:

 comment-start
    Should allways be nil in Cobol mode.  Cobol has no in-line comments.
 cobol-do-indent
    Extra indentation within do blocks.  (default 4)
 cobol-if-indent
    Extra indentation within if blocks.  (default 4)
 cobol-continuation-indent
    Extra indentation appled to continuation statements.  (default 6)
 cobol-indent-increment
    Amount of indentation to add to a line when it can be indented (default 4)
 cobol-comment-line-column
    Amount of indentation for text within full-line comments. (default 6)
 cobol-comment-indent-style
    nil    means don't change indentation of text in full-line comments,
    fixed  means indent that text at column cobol-comment-line-column
    relative  means indent at cobol-comment-line-column beyond the
 	      indentation for a line of code.
    Default value is fixed.
 cobol-comment-indent-char
    Character to be inserted instead of space for full-line comment
    indentation.  (default SPC)
 cobol-minimum-statement-indent
    Minimum indentation for cobol statements. (default 8)
 cobol-line-number-indent
    Maximum indentation for line numbers.  A line number will get
    less than this much indentation if necessary to avoid reaching
    column 5.  (default 1)
 cobol-check-all-num-for-matching-do
    Non-nil causes all numbered lines to be treated as possible 'continue'
    statements.  (default nil)
 cobol-continuation-char
    character to be inserted in column 5 of a continuation line.
    (default is ?-)
 cobol-comment-region
    String inserted by \\[cobol-comment-region] at start of each line in 
    region.  (default \"      ** \")
 cobol-electric-line-number
    Non-nil causes line number digits to be moved to the correct column 
    as typed.  (default t)
 cobol-startup-message
    Set to nil to inhibit message first time cobol-mode is used.

Turning on Cobol mode calls the value of the variable cobol-mode-hook 
with no args, if that value is non-nil.
\\{cobol-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Cobol")
  (setq major-mode 'cobol-mode)
  (if cobol-startup-message
      (message "Emacs Cobol mode ver. %s.  Mail bugs to linux4us@home.com" cobol-mode-version))
  (setq cobol-startup-message nil)
;  (setq local-abbrev-table cobol-mode-abbrev-table)  ;; no abbrevs for now
  (set-syntax-table cobol-mode-syntax-table)
  (use-local-map cobol-mode-map)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'cobol-indent-line)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'cobol-comment-hook)
  (make-local-variable 'comment-line-start-skip)
;  (setq comment-line-start-skip "^ *\\*") ; The only way to do a comment is a * in column 7
  (setq comment-line-start-skip is-a-comment)
  (make-local-variable 'comment-line-start)
  (setq comment-line-start "** ")
;  (setq comment-line-start is-a-comment)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "![ \t]*")
  (make-local-variable 'comment-start)
  (setq comment-start nil)		; COBOL has no in-line comments
  (make-local-variable 'comment-column)
  (setq comment-column cobol-comment-line-column)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'write-file-hooks)
  (setq write-file-hooks (cons 'cobol-no-tabs-hook write-file-hooks))
  (make-local-variable 'find-file-hooks)
  (setq find-file-hooks (cons 'cobol-no-tabs-hook find-file-hooks))
  (make-local-variable 'abbrev-all-caps)
  (setq abbrev-all-caps t)
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)
  (make-local-variable 'fill-column)
  (setq fill-column 70)
  ;; Font lock support
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(cobol-font-lock-keywords nil t))
  (make-local-variable 'lazy-lock-defer-on-scrolling)
  (make-local-variable 'lazy-lock-defer-contextually)
  (make-local-variable 'lazy-lock-stealth-time)
  (make-local-variable 'lazy-lock-stealth-lines)
  (make-local-variable 'lazy-lock-stealth-verbose)
  (make-local-variable 'lazy-lock-defer-time)
  (make-local-variable 'lazy-lock-minimum-size)
  (make-local-variable 'font-lock-support-mode)
  (setq font-lock-support-mode 'lazy-lock-mode)
  (setq lazy-lock-defer-on-scrolling nil)
  (setq lazy-lock-defer-contextually nil)
  (setq lazy-lock-stealth-time 5)
  (setq lazy-lock-stealth-lines 100)
  (setq lazy-lock-minimum-size 204800)
  (setq lazy-lock-stealth-verbose t)
  (setq lazy-lock-defer-time 0.25)
  (run-hooks 'cobol-mode-hook))

(defun cobol-comment-hook ()
  cobol-comment-line-column)		; ALWAYS comment in the comment column

(defun cobol-self-insert (arg)
  "Do a self-insert-command, and check for the right margin, ringing
the bell if it is reached."
  (interactive "*p")
  (let ((column (current-column)))
    (self-insert-command arg)
    (if (and (< column fill-column)
	     (>= (current-column)
		 fill-column))
	(beep 't))))

(defun cobol-goto-column (arg)
  "Goto column ARG, counting from column 1, adding spaces to
 the end of the line if needed"
  (interactive "NGoto column: ")
  (if (> arg 0)
      (progn
	(end-of-line)
	(if (> (current-column) (- arg 1))
	    (progn
	      (beginning-of-line)
	      (forward-char (- arg 1)))
	  (insert-char ?  (- arg (current-column) 1))))))
    
(defun cobol-back-delete (arg &optional killp)
  "Slightly magic version of backward-delete-char-untabify"
  (interactive "*p\nP")
  (let (atws (column (current-column)))
    (insert-char ?\n 1)
    (forward-char -1)
    (beginning-of-line)
    (if (looking-at "[ \t]*$")
	(progn
	  (if (= (% (+ column 1) cobol-indent-increment) 0)
	      (setq column (max cobol-minimum-statement-indent
				(- column cobol-indent-increment)))
	    (setq column (max cobol-minimum-statement-indent
			      (* (/ column cobol-indent-increment)
				 cobol-indent-increment))))
	  (delete-horizontal-space)
	  (insert-char (if (stringp cobol-comment-indent-char)
			   (aref cobol-comment-indent-char 0)
			 cobol-comment-indent-char)
		       column))
      (progn
	(end-of-line)
	(backward-delete-char-untabify arg killp)))
    (end-of-line)
    (delete-char 1)))

(defun cobol-no-tabs-hook ()
  "Hook for write file that removes all tabs from the buffer.
This function must return nil so that the file will actually be written."
  (save-excursion
    ; the following code is stolen from tabify.el...
    (goto-char (point-min))
    (while (search-forward "\t" nil t)        ; faster than re-search
      (let ((start (point))
	    (column (current-column))
	    (indent-tabs-mode nil))
	(skip-chars-backward "\t")
	(delete-region start (point))
	(indent-to column))))
  nil)				; just in case to make sure file is written

(defun cobol-indent-comment ()
  "Align or create comment on current line.
Existing comments of all types are recognized and aligned.
If the line has no comment, a side-by-side comment is inserted and aligned
if the value of  comment-start  is not nil.
Otherwise, a separate-line comment is inserted, on this line
or on a new line inserted before this line if this line is not blank."
  (interactive)
  (beginning-of-line)
  ;; Recognize existing comments of either kind.
  (cond ((looking-at comment-line-start-skip)
	 (delete-horizontal-regexp " \t\\*") ; kill the old comment stuff
	 (indent-to (cobol-comment-hook))
	 (insert comment-line-start))
	;; No existing comment.
	;; Insert separate-line comment, making a new line if nec.
	(t
	 (if (looking-at "^[ \t]*$")
	     (delete-horizontal-space)
	   (beginning-of-line)
	   (insert "\n")
	   (forward-char -1))
	 (indent-to (cobol-comment-hook))
	 (insert comment-line-start)
	 )))

;;	 (insert-char (if (stringp cobol-comment-indent-char)
;;			  (aref cobol-comment-indent-char 0)
;;			  cobol-comment-indent-char)
;;		      (- (calculate-cobol-indent) (current-column))))))

(defun cobol-comment-region (beg-region end-region arg)
  "Comments every line in the region.
Puts cobol-comment-region at the beginning of every line in the region. 
BEG-REGION and END-REGION are args which specify the region boundaries. 
With non-nil ARG, uncomments the region."
  (interactive "*r\nP")
  (let ((end-region-mark (make-marker)) (save-point (point-marker)))
    (set-marker end-region-mark end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (if (not arg)			;comment the region
	(progn (insert cobol-comment-region)
	       (delete-char 7)          ; we would like to maintain the original
	                                ; alignment as closely as possible
	       (while (and  (= (forward-line 1) 0)
			    (< (point) end-region-mark))
		 (insert cobol-comment-region)
		 (delete-char 7))))
    (goto-char save-point)
    (set-marker end-region-mark nil)
    (set-marker save-point nil)))

(defun cobol-uncomment-region (beg-region end-region arg)
  "Uncomments every line in the region.
Deletes cobol-comment-region from the beginning of every line in the region. 
BEG-REGION and END-REGION are args which specify the region boundaries."
  (interactive "*r\nP")
  (let ((end-region-mark (make-marker)) (save-point (point-marker)))
    (set-marker end-region-mark end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (if (not arg)			;comment the region
	(let ( ( com ( regexp-quote cobol-comment-region ) ) ) ;uncomment the region
	  (if ( looking-at com )
	      (progn (goto-char (- (match-end 0) 1))
		     (delete-backward-char 2)))
	  (while (and  (= (forward-line 1) 0)
		       (< (point) end-region-mark))
	    (if (looking-at com)
		(progn (goto-char (- (match-end 0) 1))
		       (delete-backward-char 2))))))
    (goto-char save-point)
    (set-marker end-region-mark nil)
    (set-marker save-point nil)))

(defun cobol-abbrev-start ()
  "Typing \";\\[help-command]\" or \";?\" lists all the cobol abbrevs. 
Any other key combination is executed normally." ;\\[help-command] is just a way to print the value of the variable help-char.
  (interactive)
  (let (c)
    (insert last-command-char)
    (if (or (= (setq c (read-char)) ??)	;insert char if not equal to `?'
	    (= c help-char))
	(cobol-abbrev-help)
      (setq unread-command-events c))))

(defun cobol-abbrev-help ()
  "List the currently defined abbrevs in Cobol mode."
  (interactive)
  (message "Listing abbrev table...")
  (require 'abbrevlist)
  (list-one-abbrev-table cobol-mode-abbrev-table "*Help*")
  (message "Listing abbrev table...done"))

(defun cobol-column-ruler ()
  "Inserts a column ruler momentarily above current line, till next keystroke.
The ruler is defined by the value of cobol-column-ruler.
The key typed is executed unless it is SPC."
  (interactive)
  (momentary-string-display 
   cobol-column-ruler (save-excursion (beginning-of-line) (point))
   nil "Type SPC or any command to erase ruler."))

(defun cobol-window-create ()
  "Makes the window 72 columns wide."
  (interactive)
  (let ((window-min-width 2))
    (split-window-horizontally 73))
  (other-window 1)
  (switch-to-buffer " cobol-window-extra" t)
  (select-window (previous-window)))

(defun cobol-split-line ()
  "Break line at point and insert continuation marker and alignment."
  (interactive)
  (delete-horizontal-space)
  (if (save-excursion (beginning-of-line) (looking-at comment-line-start-skip))
      (insert ?\n comment-line-start ?\  )
      (insert ?\n cobol-continuation-char))
  (cobol-indent-line))

(defun delete-horizontal-regexp (chars)
  "Delete all characters in COB_CHARS around point.
COB_CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \."
  (interactive "*s")
  (skip-chars-backward chars)
  (delete-region (point) (progn (skip-chars-forward chars) (point))))

(defun cobol-electric-line-number (arg)
  "Self insert, but if part of a Cobol line number indent it automatically.
Auto-indent does not happen if a numeric arg is used."
  (interactive "P")
  (if (or arg (not cobol-electric-line-number))
      (self-insert-command arg)
    (if (or (save-excursion (re-search-backward "[^ \t0-9]"
						(save-excursion
						  (beginning-of-line)
						  (point))
						t)) ;not a line number
	    (looking-at "[0-9]"))		;within a line number
	(insert last-command-char)
      (skip-chars-backward " \t")
      (insert last-command-char)
      (cobol-indent-line))))

(defun beginning-of-cobol-subprogram ()
  "Moves point to the beginning of the current cobol subprogram."
  (interactive)
  (let ((case-fold-search t))
    (beginning-of-line -1)
    (re-search-backward "^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]" nil 'move)
    (if (looking-at "^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]")
	(forward-line 1))))

(defun end-of-cobol-subprogram ()
  "Moves point to the end of the current cobol subprogram."
  (interactive)
  (let ((case-fold-search t))
    (beginning-of-line 2)
    (re-search-forward "^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]" nil 'move)
    (goto-char (match-beginning 0))
    (forward-line 1)))

(defun mark-cobol-subprogram ()
  "Put mark at end of cobol subprogram, point at beginning. 
The marks are pushed."
  (interactive)
  (end-of-cobol-subprogram)
  (push-mark (point))
  (beginning-of-cobol-subprogram))
  
(defun cobol-previous-statement ()
  "Moves point to beginning of the previous cobol statement.
Returns 'first-statement if that statement is the first
non-comment Cobol statement in the file, and nil otherwise."
  (interactive)
  (let (not-first-statement continue-test)
    (beginning-of-line)
    (setq continue-test
	  (looking-at
	   (concat "      " (regexp-quote (char-to-string
					   cobol-continuation-char)))))
    (while (and (setq not-first-statement (= (forward-line -1) 0))
;;		(or (looking-at comment-line-start-skip))
		(looking-at "[ \t]*$")))
    (cond ((and continue-test
		(not not-first-statement))
	   (message "Incomplete continuation statement."))
	  (continue-test	
	   (cobol-previous-statement))
	  ((not not-first-statement)
	   'first-statement))))

(defun cobol-next-statement ()
  "Moves point to beginning of the next cobol statement.
 Returns 'last-statement if that statement is the last
 non-comment Cobol statement in the file, and nil otherwise."
  (interactive)
  (let (not-last-statement)
    (beginning-of-line)
    (while (and (setq not-last-statement (= (forward-line 1) 0))
 		(or (looking-at comment-line-start-skip)
 		    (looking-at "[ \t]*$")
		    )))
    (if (not not-last-statement)
 	'last-statement)))

(defun cobol-indent-line ()
  "Indents current cobol line based on its contents and on previous lines."
  (interactive)
  (if (or (eq last-command 'cobol-indent-line) ; if we just did a tab
	  (let (atws)
	    (insert-char ?\n 1)
	    (forward-char -1)
	    (beginning-of-line)
	    (setq atws (looking-at "[ \t]*$"))
	    (end-of-line)
	    (delete-char 1)
	    (not atws)))
      (insert-char (if (stringp cobol-comment-indent-char)
		       (aref cobol-comment-indent-char 0)
		     cobol-comment-indent-char)
		   (- cobol-indent-increment
		      (% (+ (current-column) 1) cobol-indent-increment)))
    
    (let ((do-another-tab nil)
	  (cfi (calculate-cobol-indent))
	  (cur-col (current-column))) ; we did NOT just do a tab
      (save-excursion
	(beginning-of-line)
	(if (not (= cfi (current-indentation)))
	    (cobol-indent-to-column cfi)
	  ; else the line is indented correctly; check for a comment
	  (beginning-of-line)
	  (if (re-search-forward comment-start-skip
				 (save-excursion (end-of-line) (point)) 'move)
	      (cobol-indent-comment)
	    ; else not looking at a comment; make another tab
	    (if (= cur-col cfi)
		(setq do-another-tab 't)))))
      (if do-another-tab
	  (insert-char (if (stringp cobol-comment-indent-char)
			   (aref cobol-comment-indent-char 0)
			 cobol-comment-indent-char)
		       (- cobol-indent-increment
			  (% (+ (current-column) 1)
			     cobol-indent-increment))))
      ;; Never leave point in left margin.
      (if (< (current-column) cfi)
	  (move-to-column cfi)))))

(defun cobol-indent-subprogram ()
  "Properly indents the Cobol subprogram which contains point."
  (interactive)
  (save-excursion
    (mark-cobol-subprogram)
    (message "Indenting subprogram...")
    (indent-region (point) (mark) nil))
  (message "Indenting subprogram...done."))

(defun calculate-cobol-indent ()
  "Calculates the cobol indent column based on previous lines."
  (let (icol first-statement (special-col nil) (case-fold-search t))
    (save-excursion
      (setq first-statement (cobol-previous-statement))
      (if first-statement
	    (setq icol cobol-minimum-statement-indent)
	(progn
	  (if (= (point) (point-min))
	      (setq icol cobol-minimum-statement-indent)
	    	  (setq icol (cobol-current-line-indentation)))
	  (if (looking-at "[ \t]*\\*")	; if looking a at comment
	      (setq special-col 't))
	  (skip-chars-forward " \t0-9")
	  (cond ((looking-at "if[ \t]*(")
		 (if (or (looking-at ".*)[ \t]*then\\b[ \t]*[^ \t(=a-z0-9]")
			 (let (then-test)	;multi-line if-then
			   (while (and (= (forward-line 1) 0) ;search forward for then
				       (looking-at "     [^ 0]")
				       (not (setq then-test (looking-at ".*then\\b[ \t]*[^ \t(=a-z0-9]")))))
			   then-test))
		     (setq icol (+ icol cobol-if-indent))))
		((looking-at "\\(else\\|elseif\\)\\b")
		 (setq icol (+ icol cobol-if-indent)))
		((looking-at "do\\b")
		 (setq icol (+ icol cobol-do-indent)))))))
    (save-excursion
      (beginning-of-line)
      (cond ((looking-at "[ \t]*$"))	; blank lines do nothing
	    ((looking-at comment-line-start-skip) ; junk for comments
	     (setq icol cobol-comment-line-column)
	     (setq special-col t))
	    ((looking-at (concat "      "
				 (regexp-quote (char-to-string cobol-continuation-char))))
	     (setq icol cobol-continuation-indent)
	     (setq special-col t))
	    (first-statement)		;if first in the file, don't do anything
	    ((and cobol-check-all-num-for-matching-do
		  (looking-at "[ \t]*[0-9]+")
		  (cobol-check-for-matching-do))
	     (setq icol (- icol cobol-do-indent)))
	    (t
	     (skip-chars-forward " \t")	; skip to first real stuff
	     (cond
	      ;;; The following are for special names that MUST
	      ;;; start in area A (column 8-11)
	      ((looking-at "[a-z]+ +division") ; divisions in area A
	       (setq icol cobol-minimum-statement-indent))
	      ((looking-at "[-0-9a-z]+ +section") ; sections in area A
	       (setq icol cobol-minimum-statement-indent))
	      ;; this SHOULD get paragraph names
	      ((looking-at "[-a-z0-9]+\\.") ; paragraphs
	       (if (looking-at "end-[a-z]")    ;; Cover all "end-*" scope delimiters
		   (setq icol (- icol cobol-if-indent))
		 (if (looking-at "exit\\.")
		     (setq icol (+ 4 cobol-minimum-statement-indent))
		   (setq icol cobol-minimum-statement-indent))))
	      ((looking-at "fd ")	; fd's in area A
	       (setq icol cobol-minimum-statement-indent))
	      ((looking-at "sd ")	; sd's in area A
	       (setq icol cobol-minimum-statement-indent))
	      ((looking-at "rd ")	; rd's in area A
	       (setq icol cobol-minimum-statement-indent))
	      ((looking-at "cd ")	; cd's in area A
	       (setq icol cobol-minimum-statement-indent))
	      ((looking-at "01 ")	; 01 level numbers in A too
	       (setq icol cobol-minimum-statement-indent))
	      ((looking-at "77 ")	; and 77 level numbers
	       (setq icol cobol-minimum-statement-indent))
	      ((looking-at "88 ")  ; and 88 level numbers, too
	       (if (cobol-match-last-statement "88 ")
		   (setq icol 
			 (cobol-last-statement-indent))
		 (setq icol (+ cobol-indent-increment (cobol-last-statement-indent)))))

	      ;;; the following are for end-of-block detection
	      ;; ((looking-at "end-if\\b") ;; No good if it ends with a period
	      ((looking-at "end-if")
	       (setq icol (- icol cobol-if-indent)))
	      ((looking-at "else\\b")
	       (setq icol (- icol cobol-if-indent)))
	      ((looking-at "end-[a-z]")              ;; Cover all "end-*" scope delimiters
	       (setq icol (- icol cobol-if-indent)))
	      ((and (looking-at "continue\\b")
		    (cobol-check-for-matching-do))
	       (setq icol (- icol cobol-do-indent)))
	      ((looking-at "end[ \t]*do\\b")
	       (setq icol (- icol cobol-do-indent)))
	      ((and (looking-at "end\\b[ \t]*[^ \t=(a-z]")
		    (not (= icol cobol-minimum-statement-indent)))
	       (message "Warning: `end' not in column %d.  Probably an unclosed block."
			cobol-minimum-statement-indent))
	      (t			; in the case of normal lines
	       nil)
	       ))))
    (if special-col
	icol
      (max cobol-minimum-statement-indent icol))))

(defun cobol-match-last-statement (arg)
  "Match the start of line of the last non-comment/non-blank statement.
Return `t' or `nil`."
  (interactive)
  (defvar com "      *")
  (save-excursion
    (beginning-of-line)
    (forward-line -1)
    (while (or (looking-at (regexp-quote com))
	   (looking-at "[ \t]*$"))
      (forward-line -1))
    (skip-chars-forward " \t")
    (if (looking-at arg)
	 t)))

(defun cobol-last-statement-indent ()
  "Returns the column of the last non-comment statement"
  (interactive)
  (defvar com "      *")
  (defvar col 0)
  (save-excursion
    (beginning-of-line)
    (forward-line -1)
    (while (or (looking-at (regexp-quote com))
	   (looking-at "[ \t]*$"))
      (forward-line -1))
    (skip-chars-forward " \t")
    (looking-at "A-Za-z0-9")
     (setq col (current-column)))
  (max col cobol-minimum-area-b-indent))

(defun current-line ()
  "Return the vertical position of point..."
  (interactive)
  (defvar cur-line 0)
  (save-excursion
    (setq cur-line
      (+ (count-lines (point-min) (point))
	 (if (= (current-column) 0) 1 0))))
  cur-line)

(defun cobol-current-line-indentation ()
  "Indentation of current line, ignoring Cobol line number or continuation.
This is the column position of the first non-whitespace character
aside from the line number and/or column 5 line-continuation character.
For comment lines, returns indentation of the first
non-indentation text within the comment."
  (current-indentation))
;   (save-excursion
;     (beginning-of-line)
;     (cond ((looking-at comment-line-start-skip)
; 	   (goto-char (match-end 0))
; 	   (skip-chars-forward
; 	     (if (stringp cobol-comment-indent-char)
; 		 cobol-comment-indent-char
; 	         (char-to-string cobol-comment-indent-char))))
; 	  ((looking-at "     [^ 0\n]")
; 	   (goto-char (match-end 0)))
;  	  (t
;  	   ;; Move past line number.
;  	   (move-to-column 5)))
;     ;; Move past whitespace.
; ;    (message-box "1 current line = %d" (current-line))
;     (forward-line 1)
; ;    (message-box "2 current line = %d" (current-line))
;     (beginning-of-line)
;     (skip-chars-forward " \t")
;     (current-column)))

(defun cobol-indent-to-column (col)
  "Indents current line with spaces to column COL.
notes: 1) A minus sign character in column 6 indicates a continuation
          line, and this continuation character is retained on indentation;
       2) If cobol-continuation-char is the first non-whitespace character,
          this is a continuation line;
       3) A non-continuation line which has a number as the first
          non-whitespace character is a numbered line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at comment-line-start-skip)
	(if cobol-comment-indent-style
	    (let ((char (if (stringp cobol-comment-indent-char)
			    (aref cobol-comment-indent-char 0)
			    cobol-comment-indent-char)))
	      (delete-horizontal-space)
	      (insert-char char cobol-comment-line-column)))

;;      (if (looking-at "     [^ 0\n]")
;;	  (forward-char 8)
;;	(delete-horizontal-space)
;;	;; Put line number in columns 0-4
;;	;; or put continuation character in column 5.
;;	(cond ((eobp))
;;	      ((= (following-char) cobol-continuation-char)
;;	       (indent-to 5)
;;	       (forward-char 1))
;;	      ((looking-at "[0-9]+")
;;	       (let ((extra-space (- 5 (- (match-end 0) (point)))))
;;		 (if (< extra-space 0)
;;		     (message "Warning: line number exceeds 5-digit limit.")
;;		   (indent-to (min cobol-line-number-indent extra-space))))
;;	       (skip-chars-forward "0-9"))))
      ;; Point is now after any continuation character or line number.
      ;; Put body of statement where specified.
      (delete-horizontal-space)
      (indent-to col)
      ;; Indent any comment following code on the same line.
;;      (if (re-search-forward comment-start-skip
;;			     (save-excursion (end-of-line) (point)) t)
;;	  (progn (goto-char (match-beginning 0))
;;		 (if (not (= (current-column) (cobol-comment-hook)))
;;		     (progn (delete-horizontal-space)
;;			    (indent-to (cobol-comment-hook))))))
      )))

(defun cobol-line-number-indented-correctly-p ()
  "Return t if current line's line number is correctly indente.
Do not call if there is no line number."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (and (<= (current-column) cobol-line-number-indent)
	 (or (= (current-column) cobol-line-number-indent)
	     (progn (skip-chars-forward "0-9")
		    (= (current-column) 5))))))

(defun cobol-check-for-matching-do ()
  "When called from a numbered statement, returns t
 if matching 'do' is found, and nil otherwise."
  (let (charnum
	(case-fold-search t))
    (save-excursion
      (beginning-of-line)
      (if (looking-at "[ \t]*[0-9]+")
	  (progn
	    (skip-chars-forward " \t")
	    (skip-chars-forward "0") ;skip past leading zeros
	    (setq charnum (buffer-substring (point)
					    (progn (skip-chars-forward "0-9")
						   (point))))
	    (beginning-of-line)
	    (and (re-search-backward
		  (concat "\\(^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]\\)\\|\\(^[ \t0-9]*do[ \t]*0*"
			  charnum "\\b\\)\\|\\(^[ \t]*0*" charnum "\\b\\)")
		  nil t)
		 (looking-at (concat "^[ \t0-9]*do[ \t]*0*" charnum))))))))



;;************************************************************
;; CHANGELOG-------------------------------------------------*
;;
;; 08/08/00 M Vanecek
;; - Changed a '-' in the cobol-mode-syntax table to be a word
;;   character instead of a punctuation character, because a 
;;   '-' is so often used in data item and paragraph names
;; - Changed the syntax highlighting so that successive categories
;;   do not overlay previously evaluated categories of syntax
;;
;; 08/13/00 M Vanecek <linux4us@home.com>
;; - Redefined comment-line-start-skip from nil to "^......\\*"
;; - Added (define-key cobol-mode-map "\t" 'cobol-indent-line)
;; - Redefined is-a-comment from "      \\*" to "^ *\\*"
;; - Fixed a typo in the calculate-cobol-indent function--extra 
;;   parentheses
;; - 

