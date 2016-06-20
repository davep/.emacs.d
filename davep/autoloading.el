;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code for handling the generation of local autoloads.

(eval-when-compile
  (require 'autoload))

(defvar davep:autoload-file (davep:user-path "davep/autoloads.el")
  "My local autoloads file.")

(defvar davep:autoload-3rd-party-file (davep:user-path "davep/autoloads-3rd-party.el")
  "My local third party library autoloads file.")

(defun update-davep-autoloads-core (file dir)
  "Core function for updating local autoload files."
  (let ((generated-autoload-file file))
    (unless (file-exists-p file)
      (with-temp-buffer
        (insert 12)
        (setf (buffer-file-name) file)
        (save-buffer)))
    (require 'autoload)
    (update-directory-autoloads dir))
  (byte-compile-file file))

(defun update-davep-autoloads ()        
  "Update my local autoloads file."
  (interactive)
  (update-davep-autoloads-core davep:autoload-file davep:lib))

(defun update-davep-3rd-party-autoloads ()
  "Update my local 3rd party lisp autoloads file."
  (interactive)
  (update-davep-autoloads-core davep:autoload-3rd-party-file davep:lib-3rd-party))

(defun load-davep-autoloads ()
  "Load (after optionally creating) local autoloads."
  (unless davep:rootp
    (unless (file-exists-p davep:autoload-file)
      (update-davep-autoloads))
    (unless (file-exists-p davep:autoload-3rd-party-file)
      (update-davep-3rd-party-autoloads))
  (unless davep:rootp
    (load (file-name-sans-extension davep:autoload-file))
    (load (file-name-sans-extension davep:autoload-3rd-party-file)))))

(provide 'autoloading)
