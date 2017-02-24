;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window position and size.
(let ((styles
       '(
         ("bellerophon" .
          (lambda ()
              (set-frame-size (selected-frame) 90 50)
              (set-frame-position (selected-frame) 230 60)))
         ("ariel" .
          (lambda ()
              (set-frame-size (selected-frame) 90 50)
              (set-frame-position (selected-frame) 300 200))))))
  (when window-system
    (funcall (or (cdr (assoc (car (split-string (downcase (system-name)) "\\.")) styles))
                 (lambda () nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window titles.
(let ((format (concat invocation-name "@" (system-name) " - [%b]")))
  (setq frame-title-format format
        icon-title-format  format))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ensure trailing whitespace and tabs get highlighted.
(require 'highlight-chars)
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (when (buffer-file-name)
              (hc-highlight-trailing-whitespace))))

(provide 'davep-style)
