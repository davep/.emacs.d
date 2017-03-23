;;; parenface.el --- Provide a face for parens in lisp modes.
;; By Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.1
;; URL: https://github.com/davep/parenface.el

;;; Commentary:
;;
;; Add a paren-face to Emacs and add support for it to the various Lisp modes.
;;
;; Based on some code that Boris Schaefer <boris@uncommon-sense.net> posted
;; to comp.lang.scheme in message <87hf8g9nw5.fsf@qiwi.uncommon-sense.net>.

;;; Code:

(defvar parenface 'parenface)

(defface parenface
    '((((class color))
       (:foreground "DimGray")))
  "Face for displaying a paren."
  :group 'faces)

(defmacro parenface-add-support (keywords)
  "Generate a lambda expression for use in a hook.

KEYWORDS is the keywords to add paren support for."
  `(lambda ()
    (let* ((regexp "(\\|)")
           (match (assoc regexp ,keywords)))
      (unless (eq (cdr match) parenface)
        (setq ,keywords (append (list (cons regexp parenface)) ,keywords))))))

;; Keep the compiler quiet.
(eval-when-compile
  (defvar scheme-font-lock-keywords-2 nil)
  (defvar lisp-font-lock-keywords-2 nil))

(add-hook 'scheme-mode-hook           (parenface-add-support scheme-font-lock-keywords-2))
(add-hook 'lisp-mode-hook             (parenface-add-support lisp-font-lock-keywords-2))
(add-hook 'emacs-lisp-mode-hook       (parenface-add-support lisp-font-lock-keywords-2))
(add-hook 'lisp-interaction-mode-hook (parenface-add-support lisp-font-lock-keywords-2))

(provide 'parenface)

;;; parenface.el ends here
