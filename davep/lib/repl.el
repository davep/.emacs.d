;;;###autoload
(defun repl ()
  "Quick and dirty elisp repl."
  (while t
    (print
     (condition-case eval-error
         (eval (read))
       (error eval-error)))))