;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual tweaks.
(when window-system
  (cond
    ((string= (system-name) "Bellerophon")
     (set-frame-size (selected-frame) 90 50)
     (set-frame-position (selected-frame) 230 60))
    ((string= (system-name) "Ariel")
     (set-frame-size (selected-frame) 90 50)
     (set-frame-position (selected-frame) 300 200))))

(provide 'davep-style)
