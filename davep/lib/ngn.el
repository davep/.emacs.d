;;; ngn.el --- Quickly insert a newsgroup name into a buffer using completion.
;; Copyright 2000,2001 by Dave Pearson <davep@davep.org>
;; $Revision: 1.5 $

;; ngn.el is free software distributed under the terms of the GNU General
;; Public Licence, version 2. For details see the file COPYING.

;;; Commentary:
;;
;; ngn.el provides commands for quickly inserting a newsgroup name into a
;; buffer.
;;
;; The latest ngn.el is always available from:
;;
;;   <URL:http://www.davep.org/emacs/#ngn.el>

;;; INSTALLATION:
;;
;; o Drop ngn.el somwehere into your `load-path'. Try your site-lisp
;;   directory for example (you might also want to byte-compile the file).
;;
;; o Add the following autoload statements to your ~/.emacs file:
;;
;;   (autoload 'ngn-insert-name "ngn" "Insert a newsgroup name at `point'" t)
;;   (autoload 'ngn-insert-url  "ngn" "Insert a newsgroup URL at `point'" t)

;;; TODO:
;;
;; o Do away with `ngn-newsrc' and `ngn-merge-with-gnus-newsrc' and,
;;   instead, provide a flexible method of defining multiple sources for group
;;   names.

;;; Code:

;; Things we need:

(eval-when-compile
  (require 'cl))

;; Attempt to handle older/other emacs.

(eval-and-compile
  
  ;; If customize isn't available just use defvar instead.
  (unless (fboundp 'defgroup)
    (defmacro defgroup  (&rest rest) nil)
    (defmacro defcustom (symbol init docstring &rest rest)
      `(defvar ,symbol ,init ,docstring)))
  
  ;; If `line-beginning-position' isn't available provide one.
  (unless (fboundp 'line-beginning-position)
    (defun line-beginning-position (&optional n)
      "Return the `point' of the beginning of the current line."
      (save-excursion
        (beginning-of-line n)
        (point))))

  ;; If `line-end-position' isn't available provide one.
  (unless (fboundp 'line-end-position)
    (defun line-end-position (&optional n)
      "Return the `point' of the end of the current line."
      (save-excursion
        (end-of-line n)
        (point)))))

;; Customize options.

(defgroup ngn nil
  "Newgroup name lookup and recall."
  :group  'convenience
  :prefix "ngn-")

(defcustom ngn-newsrc "~/.newsrc"
  "*Name of your newsrc file."
  :type  'file
  :group 'ngn)

(defcustom ngn-cache-p t
  "*Should we cache the newsgroup names?"
  :type  'boolean
  :group 'ngn)

(defcustom ngn-must-exist t
  "*Must the input group name be a known group?"
  :type  'boolean
  :group 'ngn)

(defcustom ngn-url-format-function #'(lambda (group)
                                       (format "<URL:news:%s>" group))
  "*Function to format a newsgroup name as an URL."
  :type  'function
  :group 'ngn)

;; Non customising variables.

(defvar ngn-history nil
  "History list for `ngn-reader'.")

(defvar ngn-cache nil
  "Newsgroup name cache.")

;; Main code:

(defun ngn-merge-with-gnus-newsrc (groups)
  "Merge the existing group list with the Gnus group list (if available)."
  (if (boundp 'gnus-newsrc-alist)
      (append (loop for group in (mapcar #'car (symbol-value 'gnus-newsrc-alist))
                    unless (assoc group groups)
                    collect (list group))
              groups)
    groups))
            
(defun ngn-load-group-names ()
  "Load the newsgroup list for use with `completing-read'."
  (if (and ngn-cache ngn-cache-p)
      ngn-cache
    (setq ngn-cache
          (ngn-merge-with-gnus-newsrc
           (when (file-readable-p ngn-newsrc)
             (with-temp-buffer
               (save-excursion
                 (insert-file-contents-literally ngn-newsrc)
                 (while (re-search-forward "[:!].*$" nil t)
                   (replace-match "")))
               (loop until (eobp)
                     collect (list (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position)))
                     do (forward-line))))))))

(defun ngn-reader ()
  "Prompt the user for a newsgroup name, using completion."
  (let ((default (car ngn-history)))
    (completing-read (format "Newsgroup%s: " (if default
                                                 (format " (default %s)" default)
                                               ""))
                     (ngn-load-group-names)
                     nil
                     ngn-must-exist
                     ""
                     'ngn-history
                     default)))

;;;###autoload
(defun ngn-clear-cache ()
  "Clear the newsgroup name cache."
  (interactive)
  (setq ngn-cache nil))

;;;###autoload
(defun ngn-insert-name (groupname)
  "Insert GROUPNAME into the current buffer."
  (interactive (list (ngn-reader)))
  (insert groupname))

;;;###autoload
(defun ngn-insert-url (groupname)
  "Insert GROUPNAME, as an URL, into the current buffer."
  (interactive (list (ngn-reader)))
  (insert (funcall ngn-url-format-function groupname)))

;;;###autoload
(defun ngn-insert (no-format)
  "Insert a newsgroup name, formatting the name depending on the prefix.

If no prefix is provided then `ngn-insert-url' is used to insert the group
name. If a prefix is provided then `ngn-insert-name' is used."
  (interactive "P")
  (call-interactively (if no-format #'ngn-insert-name #'ngn-insert-url)))

(provide 'ngn)

;;; ngn.el ends here.
