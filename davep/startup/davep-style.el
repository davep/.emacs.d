;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual tweaks.
(let ((styles
       '(
	 ("Bellerophon" .
	  (lambda ()
	      (set-frame-size (selected-frame) 90 50)
	      (set-frame-position (selected-frame) 230 60)))
	 ("Ariel" .
	  (lambda ()
	      (set-frame-size (selected-frame) 90 50)
	      (set-frame-position (selected-frame) 300 200))))))
  (when window-system
    (funcall (or (cdr (assoc (system-name) styles))
		 (lambda () nil)))))

(provide 'davep-style)
