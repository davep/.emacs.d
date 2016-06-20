(eval-when-compile
  (require 'cc-mode)
  (require 'pascal)
  (require 'js))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language styles and modes.

(add-hook 'c-mode-hook                  ; C
          #'(lambda ()
              (c-set-style "BSD")
              (setq c-basic-offset 4)
              (c-set-offset 'case-label '+)
              (setup-compile "gcc -Wall -O2")
              (define-key c-mode-map "\C-m" #'newline-and-indent)))

(add-hook 'c++-mode-hook                ; C++
          #'(lambda ()
              (c-set-style "BSD")
              (setq c-basic-offset 4)
              (c-set-offset 'case-label '+)
              (c-set-offset 'inline-open 0)
              (c-set-offset 'access-label '-)
              (c-set-offset 'inclass '++)
              (setup-compile "g++ -Wall -O2")
              (define-key c++-mode-map "\C-m" #'newline-and-indent)))

(add-hook 'pascal-mode-hook             ; Generic Pascal.
          #'(lambda ()
              (setq pascal-indent-level 2)
              (setq pascal-auto-newline t)
              (setq pascal-tab-always-indent t)
              (define-key pascal-mode-map "\C-m" #'newline-and-indent)))

(add-hook 'opascal-mode-hook            ; Delphi.
          #'(lambda ()
              (setq opascal-indent-level 2)
              (define-key opascal-mode-map "\C-m" #'newline-and-indent)))

;; Javascript.
(add-hook 'js-mode-hook
          #'(lambda ()
              (define-key js-mode-map "\C-m" #'newline-and-indent)))

;; .asd files are lisp files.
(push (cons "\\.asd$" 'lisp-mode) auto-mode-alist)

(defun lisp-modes-indents ()
  ;; The following apply to various lisp modes, do them all here and don't
  ;; worry about it.
  (put 'if                      'common-lisp-indent-function 2)
  (put 'with-open-file          'lisp-indent-function 1)
  (put 'print-unreadable-object 'lisp-indent-function 1)
  (put 'with-output-to-string   'lisp-indent-function 1)
  (put 'easy-menu-define        'lisp-indent-function 3)
  (put 'eval-when-compile       'lisp-indent-function 0)
  (put 'eval-and-compile        'lisp-indent-function 0)
  (put 'with-temp-buffer        'lisp-indent-function 0)
  (put 'define-derived-mode     'lisp-indent-function 3)
  (put 'with-nntp-client        'lisp-indent-function 1)
  (put 'with-text-face          'lisp-indent-function 1)
  (put 'with-drawing-options    'lisp-indent-function 1)
  (put 'with-output-as-presentation 'lisp-indent-function 1)
  (put 'with-reconnect          'lisp-indent-function 1)
  (put 'with-standard-page      'lisp-indent-function 1)
  (put 'handler-case            'lisp-indent-function 1))

(defun dp-emacs-lisp-hook ()
  ;; Indent using Common Lisp rules (make things like `flet' indent
  ;; correctly).
  (set (make-local-variable 'lisp-indent-function) 'common-lisp-indent-function)
  ;; Handle any other indents.
  (lisp-modes-indents)
  ;; Think for me...
  (eldoc-mode))

(add-hook 'emacs-lisp-mode-hook       #'dp-emacs-lisp-hook)
(add-hook 'lisp-interaction-mode-hook #'dp-emacs-lisp-hook)

(defun scheme-modes-indents ()
  ;; For bigloo
  (put 'module 'lisp-indent-function 1)
  ;; For chicken
  (put 'let-values 'scheme-indent-function 1))

(add-hook 'scheme-mode-hook #'scheme-modes-indents)

(add-hook 'lisp-mode-hook   #'daves-generic-keys)
(add-hook 'lisp-mode-hook   #'lisp-modes-indents)
(add-hook 'scheme-mode-hook #'daves-generic-keys)
(add-hook 'python-mode-hook #'daves-generic-keys)
(add-hook 'perl-mode-hook   #'daves-generic-keys)
(add-hook 'cperl-mode-hook  #'daves-generic-keys)
(when davep:linux-x-p
  (add-hook 'vc-log-mode-hook #'(lambda () (flyspell-mode 1))))
(add-hook 'message-mode-hook #'(lambda() (flyspell-mode 1)))
(add-hook 'text-mode-hook #'(lambda()
                              (flyspell-mode 1)
                              (footnote-mode 1)))
(add-hook 'markdown-mode-hook #'(lambda ()
                                  (auto-fill-mode)
                                  (flyspell-mode 1)))

(defun daves-generic-keys ()
  "davep: Set up my generic key mappings"
  (define-key (current-local-map) "\C-m" #'newline-and-indent))

(provide 'davep-languages)
