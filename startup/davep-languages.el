;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This code is the last big hangover from my original ~/.emacs config. I
;; still need to have a proper clean-out of what's in here. Some of these
;; languages are seldom used by me any more and others likely have better
;; methods of theming the values these days. I imagine a good number of
;; these can be done via `custom' too.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language styles and modes.

(add-hook 'text-mode-hook #'(lambda()
                              (flyspell-mode 1)
                              (footnote-mode 1)))
(add-hook 'org-mode-hook #'(lambda ()
                             (auto-fill-mode)
                             (flyspell-mode 1)))


(provide 'davep-languages)
