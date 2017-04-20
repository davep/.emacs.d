;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This code is the last big hangover from my original ~/.emacs config. I
;; still need to have a proper clean-out of what's in here. Some of these
;; languages are seldom used by me any more and others likely have better
;; methods of theming the values these days. I imagine a good number of
;; these can be done via `custom' too.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (require 'cc-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup a default compile command for a buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (defvar compile-command))

(defun davep-languages-setup-compile (default-command)
  "Setup the compile command for a buffer"
  (interactive "sDefault compile command: \n")
  (or (file-exists-p "GNUmakefile")
      (file-exists-p "makefile")
      (file-exists-p "Makefile")
      (progn (make-local-variable 'compile-command)
             (setq compile-command
                   (concat default-command " " buffer-file-name
                           " -o " (file-name-sans-extension
                                   (file-name-nondirectory (buffer-file-name))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language styles and modes.

(add-hook 'c-mode-hook                  ; C
          #'(lambda ()
              (c-set-style "BSD")
              (setq c-basic-offset 4)
              (c-set-offset 'case-label '+)
              (davep-languages-setup-compile "gcc -Wall -O2")
              (define-key c-mode-map "\C-m" #'newline-and-indent)))

(add-hook 'c++-mode-hook                ; C++
          #'(lambda ()
              (c-set-style "BSD")
              (setq c-basic-offset 4)
              (c-set-offset 'case-label '+)
              (c-set-offset 'inline-open 0)
              (c-set-offset 'access-label '-)
              (c-set-offset 'inclass '++)
              (davep-languages-setup-compile "g++ -Wall -O2")
              (define-key c++-mode-map "\C-m" #'newline-and-indent)))

(when davep:linux-x-p
  (add-hook 'vc-log-mode-hook #'(lambda () (flyspell-mode 1))))
(add-hook 'text-mode-hook #'(lambda()
                              (flyspell-mode 1)
                              (footnote-mode 1)))
(add-hook 'org-mode-hook #'(lambda ()
                             (auto-fill-mode)
                             (flyspell-mode 1)))

(add-hook 'slime-inferior-process-start-hook #'(lambda ()
                                                 (require 'slime-fancy)))

(provide 'davep-languages)
