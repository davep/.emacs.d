;;; smartsig.el --- Smart signature selection based on message content.
;; Copyright 2000,2001,2002 by Dave Pearson <davep@davep.org>
;; $Revision: 1.12 $

;; smartsig.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.

;;; Commentary:
;;
;; This code is designed for use when editing email and news articles. It
;; lets you create signatures that automatically adapt to what you're
;; writing based on words contained in the body of the text.
;;
;; smartsig allows you to do this by letting you define signatures with
;; associated keywords. The definition is done using `smartsig-add':
;;
;;   (smartsig-add ID SIGNATURE &rest KEYWORDS)
;;
;; ID is a unique ID for the signature, make it something short and
;; meaningful. SIGNATURE is a pointer to the signature file that you want
;; used when this signature becomes applicable. KEYWORDS is an open ended
;; list of keywords that will help trigger the selection of this signature.
;;
;; Because there are a number of mail/news editing modes out there some
;; parts of smartsig need to be configured before it will work. The
;; configuration items are:
;;
;;   `smartsig-start-of-body-function' : This variable should be set to
;;   point to a function that will return the `point' of the start of the
;;   body of the article (IOW the first bit of text after any headers).
;;
;;   `smartsig-end-of-body-function' : This variable should be set to point
;;   to a function that will return the `point' of the end of the body of
;;   the article (IOW the `point' before the signature).
;;
;;   `smartsig-set-signature' : This variable should be set to point to a
;;   function that will set the signature. The function should take a single
;;   parameter that is the name of the signature file.
;;
;;   `smartsig-abbrev-table' : This variable should be set to a symbol that
;;   is the name of the abbrev table you intend to use.
;;
;; Please note that smartsig needs abbrev to work. Once you've configured
;; smartsig to work with your environment you'll still need to turn on
;; `abbrev-mode' for your mail/news editing mode before anything will
;; happen.
;;
;; The latest smartsig.el is always available from:
;;
;;   <URL:http://www.davep.org/emacs/smartsig.el>

;;; INSTALLATION:
;;
;; o Drop smartsig.el somewhere into your `load-path'. Try your site-lisp
;;   directory for example (you might also want to byte-compile the file).
;;
;; o Because smartsig needs to be used with your editing mode of choice you
;;   need to create a hook for that mode. The hook would look something like:
;;
;;   (add-hook 'some-mail-editing-mode-hook
;;             (lambda ()
;;               (require 'smartsig)
;;               (setq smartsig-set-signature #'function-for-setting-signature)
;;               (setq smartsig-abbrev-table  'some-mail-editing-mode-abbrev-table)
;;               (smartsig-clear)
;;               (smartsig-add "emacs" "~/.sigs/emacs" "emacs" "xemacs" "elisp" "gnu")
;;               (smartsig-add "mutt"  "~/.sigs/mutt"  "mutt" "mail" "email" "elkins")
;;               (abbrev-mode 1)))
;;
;;   where `some-mail-editing-mode-hook' is the hook for the editing mode
;;   you use and `some-mail-editing-mode-abbrev-table' is the abbrev table
;;   you use in that editing mode.

;;; Code:

;; Things we need:

(require 'cl)

;; Attempt to handle older/other emacs.
(eval-and-compile
  ;; If customize isn't available just use defvar instead.
  (unless (fboundp 'defgroup)
    (defmacro defgroup  (&rest rest) nil)
    (defmacro defcustom (symbol init docstring &rest rest)
      `(defvar ,symbol ,init ,docstring))))

;; Customize options.

(defgroup smartsig nil
  "Smart signature selection."
  :group  'mail
  :prefix "smartsig-")

(defcustom smartsig-start-of-body-function
  #'(lambda ()
      (save-excursion
        (setf (point) (point-min))
        (if (search-forward-regexp "^$" nil t)
            (point)
          (point-min))))
  "*Function for returning the `point' of the start of the message body."
  :type  'function
  :group 'smartsig)

(defcustom smartsig-end-of-body-function
  #'(lambda ()
      (save-excursion
        (setf (point) (point-min))
        (if (search-forward-regexp "^-- $" nil t)
            (point)
          (point-max))))
  "*Function for returning the `point' of the end of the message body."
  :type  'function
  :group 'smartsig)

(defcustom smartsig-set-signature
  #'(lambda (file)
      (message "If you'd set `smartsig-set-signature' the signature would actually have been set."))
  "*Function for setting the signature.

The function should take one parameter: the name of the file that contains
the signature to be set."
  :type  'function
  :group 'smartsig)

(defcustom smartsig-abbrev-table 'global-abbrev-table
  "*The name of the abbrev table to use."
  :type  'symbol
  :group 'smartsig)

(defcustom smartsig-ignore-checks nil
  "*List of functions for checking if a smartsig check should be ignored.

If any function in the list returns a non-nil value then that smartsig check
will be disabled. Note that each function in this list is called each time a
smartsig keyword is entered in the buffer."
  :type  'hook
  :group 'smartsig)

;; Non-customize variables.

(defvar smartsig-sigs nil
  "List of signature matchers.")

;; Structure for holding signature information.

(defstruct smartsig
  (id         "")
  (keywords   nil)
  (signature  "")
  (last-count 0))

;; Main code:

;;;###autoload
(defun smartsig-clear ()
  "Clear the smartsig list."
  (setq smartsig-sigs nil))

;;;###autoload
(defun smartsig-add (id signature &rest keywords)
  "Add a smart signature."
  (unless (find id smartsig-sigs :test #'(lambda (id sig) (string= id (smartsig-id sig))))
    (push (make-smartsig :id id :keywords keywords :signature signature) smartsig-sigs)
    (loop for keyword in keywords
          do (define-abbrev (symbol-value smartsig-abbrev-table) keyword 1 `(lambda () (smartsig ,id))))))
  
(defun smartsig-wordcount (word)
  "Count how many times WORD appears in the current buffer."
  (save-excursion
    (setf (point) (point-min))
    (let ((re (format "\\b%s\\b" word)))
      (loop while (re-search-forward re nil t)
            count t
            do (incf (point))))))

(defun smartsig-rankings ()
  "Return a ranked list of signatures."
  (save-excursion
    (save-restriction
      (narrow-to-region (funcall smartsig-start-of-body-function) (funcall smartsig-end-of-body-function))
      (loop for sig in smartsig-sigs
            do (setf (smartsig-last-count sig) (loop for keyword in (smartsig-keywords sig)
                                                     sum (smartsig-wordcount keyword))))
      (sort (copy-sequence smartsig-sigs)
            #'(lambda (x y)
                (> (smartsig-last-count x) (smartsig-last-count y)))))))

(defun smartsig-top (rankings)
  "Return the most popular signature choices in RANKINGS."
  (when rankings
    (let ((top (smartsig-last-count (car rankings))))
      (loop for sig in rankings
            when (= top (smartsig-last-count sig))
            collect sig))))

(defun smartsig-tied-p (rankings)
  "Is there a tie at the top of RANKINGS?"
  (not (null (cdr (smartsig-top rankings)))))

(defun smartsig-best (id rankings)
  "Return the best signature."
  (when rankings
    (let ((sigs (smartsig-top rankings)))
      (when sigs
        ;; Was there a tie?
        (if (smartsig-tied-p sigs)
            ;; Yes. Ok, so, is ID in the list of tied signatures?
            (let ((which (position id sigs :test #'(lambda (id sig) (string= id (smartsig-id sig))))))
              (when which
                ;; Yes, it is, use that one.
                (nth which sigs)))
          ;; No, no tie, set the signature from the most popular.
          (car sigs))))))

(defun smartsig-disabled-p (id)
  "Check if this smartsig check should be disabled."
  (loop for check in smartsig-ignore-checks if (funcall check id) return t))

(defun smartsig (id)
  "Set the signature based on the content of the current buffer."
  (unless (smartsig-disabled-p id)
    (let ((best (smartsig-best id (smartsig-rankings)))
          (last (when (boundp 'smartsig-last-signature)
                  (symbol-value 'smartsig-last-signature))))
      (when (and best (not (eq best last)))
        (set (make-local-variable 'smartsig-last-signature) best)
        (funcall smartsig-set-signature (smartsig-signature best))))))

(provide 'smartsig)

;;; smartsig.el ends here
