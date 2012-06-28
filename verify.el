;;; verify.el --- Compare current buffer with saved version
;;  $Revision: 0.2 $

;; Copyright (C) 1993/2004 by Martin V\"ath

;; Author:  Martin V\"ath <martin@mvath.de>
;; Keywords: compare verify compare-windows

;;; Commentary:

;; The purpose of this command (`verify') is to check whether and where
;; the content of a buffer has been changed since last saving.
;; The Emacs built-in check for this is not satisfactory, because
;; it is possible that one modifies a file and then undoes the
;; changes "manually" without using the undo-command.
;; Especially while editing config or code files you will be surprised,
;; how often it is the case that you don't know, whether you have
;; saved the "best" version since your last modification and would like
;; to compare the saved version with your current buffer.
;; This command helps!

;;;###autoload
(defun verify (do-show)
  "Compares the buffer with the saved version of it and marks the buffer
as unchanged, if they match. If they do not match, jumps to the first
different character.

If parameter DO-SHOW is non-nil and verify failed, displays the previously
saved file in an alternate window and jumps to the first different character.
Be careful in this case, since both buffers will have the same filename!
If parameter DO-SHOW is a nonzero number and verify failed, the two buffers
will also be compared with ediff."
  (interactive "P")
  (let ((name (buffer-file-name)))
    (and name (buffer-modified-p)
      (let (bad vmin vmax
            (obuf (current-buffer))
            (vbuf (create-file-buffer name)))
        (set-buffer vbuf)
        (insert-file-contents name t)
        (setq buffer-read-only t
              vmin (point-min) vmax (point-max))
        (set-buffer obuf)
        (save-restriction
          (widen)
          (if (eq 0 (setq bad
                      (abs (let ((case-fold-search nil))
                        (compare-buffer-substrings obuf (point-min) (point-max)
                                                   vbuf vmin vmax)))))
            (progn
              (set-buffer-modified-p nil)
              (setq do-show nil))
            (goto-char bad)))
        (if (null do-show)
            (kill-buffer vbuf)
          (switch-to-buffer-other-window vbuf)
          (goto-char bad)
          (pop-to-buffer obuf)
          (and (numberp do-show) (null (eq 0 do-show))
               (ediff-buffers obuf vbuf)))))))


