;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code for handling the generation of local autoloads.

(defvar davep:autoload-file (davep:user-path "davep/autoloads.el")
  "My local autoloads file.")

(defun update-davep-autoloads-core (file dir)
  "Core function for updating local autoload files."
  (let ((generated-autoload-file file))
    (cond (gnu-emacs-p
           (unless (file-exists-p file)
             (with-temp-buffer
               (insert 12)
               (setf (buffer-file-name) file)
               (save-buffer)))
           (require 'autoload)
           (update-autoloads-from-directories dir))
          (x-emacs-p
           (update-autoloads-from-directory dir)))
    (byte-compile-file file)))

(defun update-davep-autoloads ()	
  "Update my local autoloads file."
  (interactive)
  (update-davep-autoloads-core davep:autoload-file davep:lib))
