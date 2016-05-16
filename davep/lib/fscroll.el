;;; fscroll.el - Make scroll-{up,down} go all the way.
;; Copyright 2002 by Dave Pearson <davep@davep.org>
;; $Revision: 1.1 $

;; fscroll.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.

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
