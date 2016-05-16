;;; lisping.el --- Commands for dealing with lisps.
;; By Dave Pearson <davep@davep.org>
;; $Revision: 1.7 $

(eval-when-compile
  (require 'dp-lib))

(defun run-lisp-split (lisp lisp-type)
  "Split the window and run the inferior lisp/scheme"
  (run-in-new-frame #'(lambda ()
                        (funcall (if (eq lisp-type 'lisp)
                                     #'run-lisp
                                   #'run-scheme)
                                 lisp))))

(defun run-lisp-split-interactive (name mode function)
  "Create two frames, one with the inferior lisp, one as a scratch pad."
  (select-frame (make-frame))
  (switch-to-buffer (get-buffer-create (format "*%s interaction*" name)))
  (funcall mode)
  (funcall function))

;;;###autoload
(defun clisp ()
  "Run clisp"
  (interactive)
  (run-lisp-split (or (getenv "CLISP_COMMAND") "clisp -I -q -a -K full") 'lisp))

;;;###autoload
(defun clisp-interaction ()
  "Open up two frames for interaction with clisp."
  (interactive)
  (run-lisp-split-interactive "clisp" #'lisp-mode #'clisp))

;;;###autoload
(defun clocc ()
  "Run clisp dumped with clocc"
  (interactive)
  (run-lisp-split (or (getenv "CLISPCLOCC_COMMAND") "clocc") 'lisp))

;;;###autoload
(defun clocc-interaction ()
  "Open up two frames for interaction with clisp-clocc."
  (interactive)
  (run-lisp-split-interactive "clocc" #'lisp-mode #'clocc))

;;;###autoload
(defun acl ()
  "Run acl"
  (interactive)
  (run-lisp-split (or (getenv "ACL_COMMAND") "alisp") 'lisp))

;;;###autoload
(defun acl-interaction ()
  "Open up two frames for interaction with acl."
  (interactive)
  (run-lisp-split-interactive "acl" #'lisp-mode #'acl))

;;;###autoload
(defun cmucl ()
  "Run cmucl"
  (interactive)
  (run-lisp-split (or (getenv "CMUCL_COMMAND") "lisp") 'lisp))

;;;###autoload
(defun cmucl-interaction ()
  "Open up two frames for interaction with cmucl."
  (interactive)
  (run-lisp-split-interactive "cmucl" #'lisp-mode #'cmucl))

;;;###autoload
(defun sbcl ()
  "Run sbcl"
  (interactive)
  (run-lisp-split (or (getenv "SBCL_COMMAND") "sbcl") 'lisp))

;;;###autoload
(defun sbcl-interaction ()
  "Open up two frames for interaction with sbcl."
  (interactive)
  (run-lisp-split-interactive "sbcl" #'lisp-mode #'sbcl))

;;;###autoload
(defun ecl ()
  "Run ecl"
  (interactive)
  (run-lisp-split (or (getenv "ECL_COMMAND") "ecl") 'lisp))

;;;###autoload
(defun ecl-interaction ()
  "Open up two frames for interaction with ecl."
  (interactive)
  (run-lisp-split-interactive "ecl" #'lisp-mode #'ecl))

;;;###autoload
(defun corman ()
  "Run Corman Common Lisp"
  (interactive)
  (run-lisp-split (or (getenv "CORMAN_COMMAND") "corman") 'lisp))

;;;###autoload
(defun corman-interaction ()
  "Open up two frames for interaction with Corman Common Lisp."
  (interactive)
  (run-lisp-split-interactive "corman" #'lisp-mode #'corman))

;;;###autoload
(defun guile ()
  "Run guile"
  (interactive)
  (run-lisp-split (or (getenv "GUILE_COMMAND") "guile") 'scheme))

;;;###autoload
(defun guile-interaction ()
  "Open up two frames for interaction with guile."
  (interactive)
  (run-lisp-split-interactive "guile" #'scheme-mode #'guile))

;;;###autoload
(defun umb-scheme ()
  "Run umb-scheme"
  (interactive)
  (run-lisp-split (or (getenv "UMB_COMMAND") "umb-scheme") 'scheme))

;;;###autoload
(defun umb-scheme-interaction ()
  "Open up two frames for interaction with umb-scheme."
  (interactive)
  (run-lisp-split-interactive "umb-scheme" #'scheme-mode #'umb-scheme))

;;;###autoload
(defun mzscheme ()
  "Run mzscheme"
  (interactive)
  (run-lisp-split (or (getenv "MZSCHEME_COMMAND") "mzscheme") 'scheme))

;;;###autoload
(defun mzscheme-interaction ()
  "Open up two frames for interaction with mzscheme."
  (interactive)
  (run-lisp-split-interactive "mzscheme" #'scheme-mode #'mzscheme))

;;;###autoload
(defun bigloo ()
  "Run bigloo"
  (interactive)
  (run-lisp-split (or (getenv "BIGLOO_COMMAND") "bigloo") 'scheme))

;;;###autoload
(defun bigloo-interaction ()
  "Open up two frames for interaction with bigloo."
  (interactive)
  (run-lisp-split-interactive "bigloo" #'scheme-mode #'bigloo))

;;;###autoload
(defun scsh ()
  "Run scsh"
  (interactive)
  (run-lisp-split (or (getenv "SCSH_COMMAND") "scsh") 'scheme))

;;;###autoload
(defun scsh-interaction ()
  "Open up two frames for interaction with scsh."
  (interactive)
  (run-lisp-split-interactive "scsh" #'scheme-mode #'scsh))

;;;###autoload
(defun rep ()
  "Run rep"
  (interactive)
  (run-lisp-split (or (getenv "REP_COMMAND") "rep") 'lisp))

;;;###autoload
(defun rep-interaction ()
  "Open up two frames for interaction with rep."
  (interactive)
  (run-lisp-split-interactive "rep" #'scheme-mode #'rep))

;;;###autoload
(defun elisp ()
  "Run ielm"
  (interactive)
  (run-in-new-frame #'ielm))

;;;###autoload
(defun chicken ()
  "Run chicken"
  (interactive)
  (run-lisp-split (or (getenv "CHICKEN_COMMAND") "csi") 'scheme))

;;;###autoload
(defun chicken-interaction ()
  "Open up two frames for interaction with chicken."
  (interactive)
  (run-lisp-split-interactive "chicken" #'scheme-mode #'chicken))

;;;###autoload
(defun clim ()
  "Run a CLIM-enabled lisp listener with SLIME."
  (interactive)
  (let ((inferior-lisp-program "clim"))
    (slime)))

(provide 'lisping)
