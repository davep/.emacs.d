;;; fscroll.el --- Make scroll-{up,down} go all the way.
;; Copyright 2002 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.1
;; Keywords: convenience
;; URL: https://github.com/davep/fscroll.el

;; fscroll.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.

;;; Commentary:

;; Out of the box GNU Emacs only scrolls the buffer when using `scroll-down'
;; and `scroll-up' (which most people would have bound to PgDn and PgUp).
;; Once the first or last lines are visible you'll get start/end of buffer
;; errors if you try and hit those keys more. Many other editors will, when
;; that happens, move the cursor to the start or end of the buffer. This
;; code makes GNU Emacs do that too.

;;; Code:

(defadvice scroll-down (around full-scroll-down activate)
  "Ensure that `scroll-down' goes right to the start of the buffer."
  (condition-case nil
      ad-do-it
    (beginning-of-buffer (goto-char (point-min)))))

(defadvice scroll-up (around full-scroll-up activate)
  "Ensure that `scroll-up' goes right to the end of the buffer."
  (condition-case nil
      ad-do-it
    (end-of-buffer (goto-char (point-max)))))

(provide 'fscroll)

;;; fscroll.el ends here
