;;; avmmf.el --- AVM Microformat tools for emacs
;; Copyright 2008,2009 by Dave Pearson <davep@davep.org>
;; $Revision: 1.1 $

;; avm.el is free software distributed under the terms of the GNU General
;; Public Licence, version 2 or (at your option) any later version. For
;; details see the file COPYING.

;;; Commentary:
;;
;; avmmf.el provides a set of functions for dealing with the AVM
;; microformat. For more information on this see:
;;
;;   <URL:http://www.strudel.org.uk/blog/astro/microformat.shtml>
;;   <URL:http://www.jodcast.net/avm/index.html>
;;
;; The latest avmmf.el is always available from:
;;
;;   <URL:http://www.davep.org/emacs/#Favmmf.el>

;;; INSTALLATION:
;;
;; o Drop avmmf.el somwehere into your `load-path'. Try your site-lisp
;;   directory for example (you might also want to byte-compile the file).
;;
;; o Add the following autoload statement to your ~/.emacs file:
;;
;;   (autoload 'avmmf-insert "avmmf" "Insert AVM microformat code into a buffer" t)

;;; Code:

(require 'url)
(eval-when-compile
  (require 'cl))

(defvar avmmf-display "compact"
  "Type of AVM microformat display to use.")

(defun avmmf-make-url (object-name)
  "Make URL for getting AVM microformat code for OBJECT-NAME."
  (format "http://www.jodcast.net/lookUP/avm/?name=%s&resultType=m&display=%s"
          (url-hexify-string object-name)
          avmmf-display))

(defun avmmf-get (object-name)
  "Get the AVM microformat code for OBJECT-NAME."
  (let ((result
         (url-retrieve-synchronously
          (avmmf-make-url object-name))))
    (with-current-buffer result
      (setf (point) (point-min))
      (when (search-forward "<span" nil t)
        (buffer-substring-no-properties
         (line-beginning-position)
         (line-end-position))))))

;;;###autoload
(defun avmmf-insert (object-name)
  "Insert AVM microformat code for OBJECT-NAME at `point'."
  (interactive "sObject: ")
  (let ((result (avmmf-get object-name)))
    (if result
        (insert result)
      (error "AVMMF: Could not get microformat code for '%s'" object-name))))

;;; avmmf.el ends here
