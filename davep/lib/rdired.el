(defvar rdired-hosts
  '(("new-hagbard" . "~")
    ("www.retail.aflex.net" . "~"))
  "*List of host/default directory combinarions.")

;;;###autoload
(defun rdired (host)
  (interactive (list (completing-read "Host: " rdired-hosts)))
  (let* ((host (or (assoc host rdired-hosts) (cons host "~"))))
    (dired (format "/%s:%s" (car host) (cdr host)))))
