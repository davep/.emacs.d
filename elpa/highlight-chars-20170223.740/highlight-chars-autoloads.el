;;; highlight-chars-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "highlight-chars" "highlight-chars.el" (22729
;;;;;;  19730 0 0))
;;; Generated autoloads from highlight-chars.el

(let ((loads (get 'Highlight-Characters 'custom-loads))) (if (member '"highlight-chars" loads) nil (put 'Highlight-Characters 'custom-loads (cons '"highlight-chars" loads))))

(defface hc-tab '((t (:background "LemonChiffon"))) "\
*Face for highlighting tab characters (`C-i') in Font-Lock mode." :group (quote Highlight-Characters) :group (quote faces))

(defface hc-trailing-whitespace '((t (:background "Gold"))) "\
*Face for highlighting whitespace at line ends in Font-Lock mode.
This includes tab, space, and hard (non-breaking) space characters." :group (quote Highlight-Characters) :group (quote faces))

(defface hc-hard-space '((t (:background "Aquamarine"))) "\
*Face for highlighting non-breaking spaces (`? ')in Font-Lock mode.
\(This is also ?\240.)" :group (quote Highlight-Characters) :group (quote faces))

(defface hc-other-char '((t (:background "HotPink"))) "\
*Face for highlighting chars in `hc-other-chars' in Font-Lock mode." :group (quote Highlight-Characters) :group (quote faces))

(defvar hc-other-chars nil "\
*Characters to highlight using face `hc-other-char'.
The characters are highlighted unless they are excluded by option
`hc-other-chars-NOT'.

A nil value means highlight *all* characters (except those excluded by
`hc-other-chars-NOT').

If non-nil, the value is a list of entries, each of which can be any
of these:
 * a string of individual characters
 * a character range, represented as a cons (FROM . TO),
   where FROM and TO are both included
 * a character class, such as [:nonascii:]
 * a character set, such as `iso-8859-1' or `lao'

The last two alternatives are available only for Emacs 22 and later.

For the first alternative, remember that you can insert any character
into the string using `C-q', and (for Emacs 23 and later) you can
insert any Unicode character using `C-x 8 RET'.

For Emacs 20, the first alternative is not well supported: Do not use
chars that are special within a regexp character alternative (i.e.,
\[...]).  In Emacs 20, the string you specify is simply wrapped with
\[...], which is not correct for all chars.")

(custom-autoload 'hc-other-chars "highlight-chars" nil)

(defvar hc-other-chars-NOT nil "\
*Chars to exclude from highlighting with face `hc-other-char'.
The possible option values are the same as for `hc-other-char'.")

(custom-autoload 'hc-other-chars-NOT "highlight-chars" nil)

(defvar hc-other-chars-font-lock-override 'append "\
*How highlighting for other chars interacts with existing highlighting.
The values correspond to the values for an OVERRIDE spec in
`font-lock-keywords'.  See (elisp) `Search-based Fontification'.

This affects commands `hc-toggle-highlight-other-chars' and
 `hc-highlight-chars', and functions `hc-highlight-other-chars' and
 `hc-dont-highlight-other-chars'.")

(custom-autoload 'hc-other-chars-font-lock-override "highlight-chars" t)

(defalias 'toggle-highlight-tabs 'hc-toggle-highlight-tabs)

(autoload 'hc-toggle-highlight-tabs "highlight-chars" "\
Toggle highlighting of TABs, using face `hc-tab'.

\(fn &optional MSGP)" t nil)

(defalias 'toggle-highlight-hard-spaces 'hc-toggle-highlight-hard-spaces)

(autoload 'hc-toggle-highlight-hard-spaces "highlight-chars" "\
Toggle highlighting of non-breaking space characters (`? ').
\(This is also ?\240.)
Uses face `hc-hard-space'.

\(fn &optional MSGP)" t nil)

(autoload 'hc-highlight-chars "highlight-chars" "\
Read a string of characters and a face name, then highlight the chars.
With a prefix arg, unhighlight the chars.

As an alternative to a string of characters, what you enter for the
characters can be any of the possibilities for a single entry in
`hc-other-chars'.  That is it can be any of the following (the last
two are only for Emacs 22+):

* A cons (C1 . C2), where C1 and C2 are characters, that is, integers,
  which you can represent using character notation.  This represents
  the range of characters from C1 through C2.

  For example, you would enter `(?a . ?g)' to mean the characters from
  `a' through `g', inclusive.  Note that you enter the parentheses and
  the dot, and you can use character read syntax (e.g., `?a' for `a').

* A character class, such as `[:digit:]'.  This matches all characters
  in the class.  You must type the brackets and colons (`:').  (This
  possibility is available only for Emacs 22 and later.)

* A character set, such as `iso-8859-1' or `lao'.  This matches all
  characters in the set.

If you mistype one of the above representations, then what you type is
interpreted as just a string of the characters to highlight.  For
example, if you mean to type `[:digit:]' but you instead type
`[:digit]' (no second colon), then the characters highlighted are
\[, :, d, g, i, t, and ].

Non-interactively:

* CHARS is a possible value for `hc-other-chars', that is, a list of
  entries such as described above.
* FACE is the face to use (e.g., a symbol).
* Non-nil OFFP non-nil means turn highlighting off.

\(fn CHARS FACE &optional OFFP MSGP)" t nil)

(defalias 'toggle-highlight-other-chars 'hc-toggle-highlight-other-chars)

(autoload 'hc-toggle-highlight-other-chars "highlight-chars" "\
Toggle highlighting chars in `hc-other-chars'
By default, face `hc-other-char' is used.
With a prefix arg you are prompted for the face to use.

\(fn &optional FACE MSGP)" t nil)

(defalias 'toggle-highlight-trailing-whitespace 'hc-toggle-highlight-trailing-whitespace)

(autoload 'hc-toggle-highlight-trailing-whitespace "highlight-chars" "\
Toggle highlighting of trailing whitespace.
This includes tab, space, and hard (non-breaking) space characters.
Uses face `hc-trailing-whitespace'.

\(fn &optional MSGP)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; highlight-chars-autoloads.el ends here
