;;; kbdmacro.el --- A keyboard macro recorder.
;;  $Revision: 0.3 $

;; Copyright (C) 1994/2004 by Martin V\"ath
;; SPDX-License-Identifier: GPL-2.0-only

;; Author:  Martin V\"ath <martin@mvath.de>
;; Keywords: macro emulation convenience

;;; Commentary:

;; This is a substitute for Emacs' original macro handling.
;; If you want to define a macro, just use the command `macro-record'.
;; You are asked for a key which will later activate the macro.
;; When you are done, just use `macro-record' once more.
;; Then, whenever you press the key you entered, the macro will get executed.
;; (Of course, the macro should not use the key where it is recorded).
;; You can delete the macro from a key, restoring the old definition,
;; with the command `macro-delete'.
;; You can save all your macro definitions at once with the command
;; `macro-save'. This will save a valid ".el" file. Loading this file
;; later with `load-file' will automatically activate all saved macros.

(defvar macro-save-key-alist nil
  "The definition of keys, before they were overridden with a macro
by `macro-build' or `macro-record'.")

(defvar macro-current-key nil
  "Remembers the key in which the user wants to define a keyboard macro.")

(defun macro-get-key-to-record (k)
  "Used by `macro-record'."
  (interactive "kRecord macro to key (Ctrl-G to cancel) ")
  k)

;;;###autoload
(defun macro-build (key command)
  "Redefine key to be command.
If not already done, save previous command in macro-save-key-alist."
  (or (assoc key macro-save-key-alist)
    (setq macro-save-key-alist (cons (cons key (global-key-binding key))
                                     macro-save-key-alist)))
  (global-set-key key command))

;;;###autoload
(defun macro-record ()
  "Asks for a key and then starts recording a macro for this key;
the key is remembered in `macro-current-key'.
A `keyboard-quit' or `macro-delete' key is not allowed for macro-definition:
Choosing such a key means: Canceling the macro definition.
The old key-definition is remembered in `macro-save-key-alist'.

If the macro definition is complete, the user has to call this function again.
This is done, for example, by pressing the key, to which the macro is recorded."
  (interactive)
  (if (eq macro-current-key nil)
      (let ((k (call-interactively 'macro-get-key-to-record)))
         (if (or (eq (key-binding k) 'keyboard-quit)
                 (eq (key-binding k) 'macro-delete))
             (error "Cancel macro-definition.")
           (setq macro-current-key k)
           (start-kbd-macro nil)))
    (end-kbd-macro)
    (message "Macro stopped and saved to key %s."
             (key-description macro-current-key))
    (macro-build macro-current-key last-kbd-macro)
    (setq macro-current-key nil)))

(defun macro-delete (k)
  "Delete the macro from the key pressed by restoring the old function
of the key with `macro-save-key-alist'."
  (interactive "kDelete macro from key ")
  (let ((value (assoc k macro-save-key-alist)))
    (when value
      (global-set-key k (cdr value))
      (setq macro-save-key-alist (delq value macro-save-key-alist)))))

(if (functionp 'key-press-event-p); XEmacs
  (defun macro-events-to-keys (key)
    "Return KEY in a lisp-readable form."
    (let (new)
      (mapc (lambda (el)
              (cond ((key-press-event-p el)
                     (push (let ((mods (event-modifiers el)))
                             (if mods
                                 (append mods (list (event-key el)))
                               (event-key el)))
                           new))
                    ((or (characterp el)
                         (symbolp el)
                         (listp el))
                     (push el new))))
            key)
      (setq new (nreverse new))
      (mapvector 'identity new)))
  ;; Not XEmacs
  (defalias 'macro-events-to-keys 'identity))

(defun macro-save ()
  "Save all macros recorded with `macro-record'/`macro-build'."
  (interactive)
  (or macro-save-key-alist
    (error "No macros defined."))
  (save-excursion
    (let (k (print-level nil) (print-length nil))
      (set-buffer (generate-new-buffer "*Keyboard Macros*"))
      (dolist (m macro-save-key-alist)
        (setq k (car m))
        (insert "(macro-build ")
        (prin1 (macro-events-to-keys k) (current-buffer))
        (insert " ")
        (prin1 (macro-events-to-keys (global-key-binding k)) (current-buffer))
        (insert ")\n"))
      (let ((filename
        (read-file-name "Write macros to file: " nil nil nil "kbdmac.el")))
        (unless (or (null filename) (string-equal filename ""))
          (write-region (point-min) (point-max) filename)))
      (kill-buffer nil))))

