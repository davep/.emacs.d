;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual tweaks.
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

(provide 'davep-style)
