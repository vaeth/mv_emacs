;;; block.el --- Use Block instead of Region in Emacs
;;  $Revision: 1.5 $

;; Copyright (C) 1994/2004/2006/2009/2010/2012/2017 by Martin V\"ath

;; Author:  Martin V\"ath <martin@mvath.de>
;; Keywords: block emulation convenience

;;; Commentary:

;; This is the block package which provides a new `region' type (which
;; in the following is called `block') which differs from Emacs' original
;; region in the following two respects:

;; 1. The end of the block is the place where you explicitly set the end
;;    of the block. In particular, the block is completely independent of
;;    the current point/cursor position.

;; 2. There is only one block for all buffers. So it makes sense to put the
;;    cursor into a different buffer and copy/move the previously marked block
;;    there.

;; Apart from the above changes, the block aims to be a full replacement for
;; the region, i.e. all commands which usually refer to the region use instead
;; the block if block-mode is active.

;; Usage is very simple: After this file is loaded, you have the minor mode
;; `block-mode' which you can switch on and off as required.

;; For proper functioning it might be necessary to call
;; `block-advise-all' (C-b C-a) after certain autloads (unfortunately,
;; there is no way in Emacs to do this automatically during autoloads).
;; For some new (or personal) Lisp functions which currently respect the
;; region and which are not yet supported by this file, it might also be
;; necessary to use the `block-advice-hook' to advice these functions.
;; You can do this with the aid of `block-advice-code' and its wrapper
;; functions (documented below) which provide much more functionality than
;; what is currently used. For some simple examples how these functions are
;; called, have a look in the definition of the function `block-advise-all'
;; at the lines following the text "(run-hooks 'block-advice-hook)".


(require 'advice)

;;; The Variables
;; (the keymap and some customization for "convenience" wrappers are posponed)

(defgroup block nil
  "Use Block instead of Region."
  :prefix "block"
  :group 'editing-basics
  :group 'convenience
  :group 'emulations
  :link '(emacs-commentary-link :tag "Commentary" "block.el")
  :link '(emacs-library-link :tag "Lisp File" "block.el"))

(defcustom block-face 'highlight; better be different from 'region
  "The face that is used to display the block."
  :type 'face
  :group 'block)

(defcustom block-copy-moves t
  "Whether copying of a block moves the block-marks."
  :type 'boolean
  :group 'block)

(defcustom block-forces-transient t
  "Whether block-mode implies transient-mark-mode."
  :type 'boolean
  :group 'block)

(defvar block-transient-state transient-mark-mode
  "The saved state of `transient-mark-mode' if block-mode is left.")

(defvar block-debug nil
  "If non-nil show the generated advice-code in the current buffer.
You might want to set this temporarily in `block-advice-hook'.")

(defvar block-overlay nil
  "Currently marked block")

(defvar block-advice-hook nil
  "This hook is run if recalculation of advices is necessary.
See `block-advise-all'")

(defvar block-advice-always-hook nil
  "This hook is run if advices are possibly recalculated, no matter
whether a recalculation is necessary. See `block-advise-all'.")

(defvar block-disable-advices-hook nil
  "This hook is run when the advices are disabled. Usually you will not need
this hook, because it is not necessary to manually disable advices set
with `block-advice-code'")

(defvar block-scan-error t
  "This variable should normally be non-nil: This means that the
scan/advice functions of the block package report errors as expected.
You might want to change this variable only temporarily to advise a
large class of functions \"heuristically\" without serious testing.")

(defvar block-advice-list nil
  "This variable contains an associative list of all functions which have
been advised. The value stored to each function is either nil or a list
consisting of the following data (the list might actually be shorter)
 1. The CLASS of the advice (`around', `before' or `after', see `defadvice').
 2. The NAME of the advice (usually always `block-advice').
 3. GEN-FUNC-NAME (see `block-advice-code').
 4. The POS argument of the advice (see `defadvice').
 5. The FLAG argument of the advice (see `defadvice').
 6. Any additional data GEN-FUNC returned (see `block-advice-code').
If the value is nil, this means that the function is actually not adviced;
moreover as all functions in this list the function also does not get adviced
by `block-advice-code' unless the FORCE parameter is used.")

(defvar block-advice-list-name 'block-advice-list
  "Usually, this variable contains the symbol of `block-advice-list'.
All references to that variable are made indirectly over this name.
You can use this fact to keep separated list of advices of certain kinds
if you have a need for this.")

;;; Small helper functions

(defun block-remove-trail (list &optional n)
  "Remove all trailing `nil' values from LIST and return new LIST.
With argument N, keep at least N elements (N=0 is the same as N=nil).
This function modifies LIST unless the returned list is nil."
  (and n (eq 0 n)
       (setq n nil))
  (let ((e (and n (nthcdr (1- n) list)))); Our last "non-nil" element.
    (if (and n (null e)); if the list was not long enough:
        list; return value
      (let ((c (or e list)))
        (while c
          (if (car c)
            (setq e c))
          (setq c (cdr c))))
      (when e; Cut the list. If all elements are nil return nil
        (setcdr e nil)
        list))))

(if (functionp 'transient-mark-mode); not XEmacs
  (defun block-region-is-active ()
    "Test whether the marker is active"
    (and (mark t)
         (or (null transient-mark-mode)
             mark-active
             mark-even-if-inactive)))
  ;; XEmacs
  (defun block-region-is-active ()
    "Test whether the marker is active"
    (mark t)))

(defmacro block-save-point (&rest body)
  "Execute BODY, then restore point to the position before BODY."
  `(let ((block-point (make-marker)))
    (set-marker block-point (point))
    (progn ,@body)
    (goto-char block-point)
    (set-marker block-point nil)))

(defun block-functionp (func)
  "Return (for recursive evaluation)
 'autoload  if FUNC is the definition of an autoloaded function
 'lambda    if FUNC is the definition of a lambda function
 'byte-code if FUNC is the definition of a byte-code function
 t          if FUNC is the definition of a built-in function
 nil        otherwise."
    (cond
     ((and (symbolp func) (fboundp func)
           (condition-case nil
               (setq func (indirect-function func))
             (error nil))
           (eq (car-safe func) 'autoload)
           (null (car-safe (cdr-safe (cdr-safe (cdr-safe (cdr-safe func)))))))
      'autoload)
     ((subrp func)
      t)
     ((byte-code-function-p func)
      'byte-code)
     ((eq (car-safe func) 'lambda)
      'lambda)))

;;; The main function definitions
;;
;; We start with all functions involved with block definition/movement.

;; First the internal functions for the block access.
;; All access is done via this functions.

(when (null (functionp 'overlayp)); Older XEmacs has only extent
  (defalias 'overlayp 'extentp)
  (defalias 'overlay-start 'extent-start-position)
  (defalias 'overlay-end 'extent-end-position)
  (defalias 'overlay-buffer 'extent-object)
  (defalias 'make-overlay 'make-extent)
  (defalias 'move-overlay 'set-extent-endpoints)
  (defalias 'delete-overlay 'delete-extent)
  (defalias 'overlay-put 'set-extent-property)
  (defun block-extent-property () (set-extent-property block 'unique t)))

(defun block-start-char ()
  "Return start position of the block."
  (overlay-start block-overlay))

(defun block-end-char ()
  "Return end position of the block."
  (overlay-end block-overlay))

(defun block-buffer ()
  "Return buffer of the block."
  (overlay-buffer block-overlay))

(defun block-defined ()
  "Return
  nil  if no block is defined.
  0    if a block is defined but not in the current buffer.
  t    if a block is defined in the current buffer."
  (and (overlayp block-overlay)
       (or (eq (block-buffer) (current-buffer))
           0)))

(defun block-hide-leave-mark ()
  "As `block-hide-command', but mark is not deactivated."
  (let ((defined (block-defined)))
    (when defined
      (delete-overlay block-overlay)
      (if (numberp defined)
        (redraw-display))
      (setq block-overlay nil))))

(defun block-create (beg end &optional leave-mark no-store)
  "Create (or redefine) the block in the current buffer from BEG to END,
deactivate mark (unless LEAVE-MARK is non-nil) and use the new block
for copy-and-paste to other programs (unless NO-STORE is non-nil).
Usually, you will not want to use `block-create' directly but instead call
the higher-level function `block-define-command'."
  (interactive "r")
  (when (markerp block-overlay)
    (set-marker block-overlay nil)
    (setq block-overlay nil))
  (if (overlayp block-overlay)
      (if (< end beg)
          (move-overlay block-overlay end beg (current-buffer))
        (move-overlay block-overlay beg end (current-buffer)))
    (setq block-overlay
      (if (< end beg)
          (make-overlay end beg)
        (make-overlay beg end)))
    (overlay-put block-overlay 'face block-face)
    (if (functionp 'block-extent-property)
        (block-extent-property)))
  (unless leave-mark
    (if (functionp 'deactivate-mark); not XEmacs
      (progn
        (deactivate-mark)
        (if transient-mark-mode
            (setq deactivate-mark t)))
      ;; XEmacs
      (when (functionp 'zmacs-deactivate-region)
        (zmacs-deactivate-region)
        (zmacs-update-region))))
  (unless no-store
    (kill-new (buffer-substring (block-start-char) (block-end-char)))
    (if (functionp 'own-selection); XEmacs
      (own-selection (buffer-substring (block-start-char) (block-end-char))))
    (if (functionp 'x-store-cutbuffer); XEmacs
      (x-store-cutbuffer (buffer-substring (block-start-char)
                                           (block-end-char))))
    (if interprogram-cut-function; Not XEmacs
      (funcall interprogram-cut-function
        (buffer-substring (block-start-char) (block-end-char))))))

(defun block-start-new-block (&optional p)
  "Deactivate block and until a new block is defined, let
`block-start-marked' return the position P (defaults is point)
of the current buffer."
  (block-hide-leave-mark)
  (setq block-overlay (make-marker))
  (set-marker block-overlay (or p (point))))

(defun block-start-marked ()
  "If currently no block is defined but `block-start-new-block' was called for
the current buffer then the corresponding position is returned. In all other
cases the return value is nil."
  (and (markerp block-overlay) (marker-buffer block-overlay)
       (eq (marker-buffer block-overlay) (current-buffer))
       (marker-position block-overlay)))

(defun block-mark-to-block ()
  "Define the current mark-region as the block. This is only useful e.g. after
mouse-dragging when block.el lost control over the mark."
  (if mark-active
       (block-create (mark-marker) (point))))

;; Now the user-level block commands

(defun block-define-command (&optional mode p always)
  "Activate block, setting/moving block-end-char to point (resp. to P).
More precisely, this function does the following.

1. If the block is not active in the current buffer or if ALWAYS is non-nil,
   then the block is (re)defined as the current region in the current buffer.
   If there is no active region (in transient-mark-mode) or explicit start,
   it depends on the value of MODE what happens:
       0               Use region anyway even if mark is not set.
       positive number Use whole buffer as block.
       negative number Do not define the block.
       string          Error with message string.
                       If the string is empty, a default message is used.
       nil             Define block as everything up to point/P.
       anything else   Define block as everything from point/P.
2. If a block is active in the current buffer, then the end
   (resp. the beginning if MODE is t) of the block is moved to point/P.

If ALWAYS is a number the behaviour is slightly different: In this case,
it is expected that MODE is an integer/marker which is used as the mark
to define the region."
  (interactive "P")
  (or p (setq p (point)))
  (let
    (tmp (new-data
      (cond
       ((and (null always) (eq t (block-defined)))
        (list p (if (eq t mode) (block-end-char) (block-start-char))))
       ((numberp always)
        (list p mode))
       ((block-region-is-active)
        (list (mark t) p))
       ((setq tmp (block-start-marked))
        (list tmp p))
       ((eq 0 mode)
        (list (let ((m (mark-marker)))
                (if (and m (marker-position m))
                    m
                  (point-min)))
              p))
       ((numberp mode)
        (and (< 0 mode)
          (list (point-min) (point-max))))
       ((stringp mode)
        (if (equal "" mode)
            (error "No block is defined in current window")
          (error mode)))
       ((null mode)
        (list (point-min) p))
       (t
        (list p (point-max))))))
    (if new-data (apply 'block-create new-data))))

(defun block-redefine-start-command (&optional p always)
  "Activate block and set or move block-start-char to point (resp. to P).
This function is identical to block-define-command with MODE=t."
  (interactive)
  (block-define-command t p always))

;; The following function is not really a user level command.
;; However, it is related with `block-define-command'.
;; It is actually a frequently used helper function which is
;; essentially the same as `block-defined' but also defines the block
;; if it was "halfway" defined or if it is absolutely necessary to have a
;; block for the following code.

(defun block-define-if-possible (&optional mode same)
  "Define the block if it was not defined and then return
  nil            if no block is defined
  0              if a block is defined but not in the current buffer
  t              if a block is defined in the current buffer
The value of SAME determines what happens if the block is defined
but not in the same (current) buffer.
  nil            Nothing
  a string       Error with message string.
                 If the string is empty, a default message is used.
  anything else  Redefine the block.
If the block has to be (re)defined, the prefix argument MODE has an
analogous meaning as in `block-define-command'."
  (interactive "P")
  (let ((defined (block-defined)))
    (cond
     ((and (eq t defined))
      defined)
     ((and defined (or (null same) (stringp same)))
       (if (stringp same)
         (if (equal "" same)
             (error "Block is not in the current window")
           (error stringp)))
       defined)
     (t
      (and (null same) (equal "" mode)
        (setq mode "No block is defined"))
      (block-define-command mode nil t)
      (block-defined)))))

;; Proceeding with "real" user level block commands...

(defun block-set-mark-command (&rest args)
  "This is like `set-mark-command' with the difference that a possibly defined
block is hidden."
  (interactive "P")
  (block-start-new-block)
  (apply 'set-mark-command args))

(defun block-push-mark-command (&rest args)
  "This is like `push-mark-command' with the difference that a possibly defined
block is hidden."
  (interactive "P")
  (block-start-new-block)
  (if (functionp 'push-mark-command); Not XEmacs or older Emacs
      (apply 'push-mark-command args)
    (push-mark (nth 0 args) (nth 1 args) t)))

(defun block-hide-command ()
  "Deactivate (hide) block."
  (interactive)
  (block-hide-leave-mark)
  (setq deactivate-mark t))

(defun block-line-command ()
  "Mark current line as block and move point to beginning of next line."
  (interactive)
  (let ((goal-column 0) (p (point)) b)
    (beginning-of-line)
    (setq b (point))
    (forward-line)
    (block-define-command b nil 1)
    (goto-char p)))

(defun block-line-include-command ()
  "Include current line (including line-end) into the block and move point
to beginning of current or next line."
  (interactive)
  (let ((goal-column 0))
    (beginning-of-line)
    (if (eq t (block-defined))
      (let ((s (block-start-char)) (e (block-end-char)) (p (point)))
        (if (< e s)
          (setq e s s (block-end-char)))
        (if (< p s)
          (block-redefine-start-command)
          (forward-line)
          (unless (< p e)
            (block-define-command))))
      (block-line-command)
      (forward-line))))

(defun block-copy-command ()
  "Copy block to point in current buffer."
  (interactive "*")
  (block-define-if-possible "No block to be copied")
  (let ((goal-column 0)
        (s (block-start-char)) (e (block-end-char))
        (b (block-buffer)) sp)
    (if (< e s)
      (setq e s s (block-end-char)))
    (and (eq b (current-buffer)) (<= s (point)) (< (point) e)
      (goto-char e))
    (setq sp (point))
    (insert-buffer-substring b s e)
    (if block-copy-moves
      (block-define-command sp (point) 1)
      (push-mark))
    (goto-char sp)))

(defun block-move-command ()
  "Move block to point in current buffer (without using the kill-buffer)."
  (interactive "*")
  (block-define-if-possible "No block to be moved")
  (let ((goal-column 0)
        (s (block-start-char)) (e (block-end-char))
        (b (block-buffer)) sp
        (sm (make-marker)) (em (make-marker)))
    (if (< e s)
      (setq e s s (block-end-char)))
    (and (eq b (current-buffer)) (<= s (point)) (< (point) e)
      (goto-char e))
    (set-marker sm s b)
    (set-marker em e b)
    (setq sp (point))
    (insert-buffer-substring b s e)
    (block-define-command sp (point) 1)
    (goto-char sp)
    (save-excursion
      (set-buffer b)
      (delete-region sm em))
    (set-marker sm nil)
    (set-marker em nil)))

(defun block-goto-command (arg)
  "Jump to the block start (pushing mark).
With prefix, jump to the block end."
  (interactive "P")
  (when (block-define-if-possible "")
    (pop-to-buffer (block-buffer))
    (push-mark)
    (if arg
      (goto-char (block-end-char))
      (goto-char (block-start-char)))))

(defun block-goto-end-command ()
  "Jump to the block end (pushing mark).
This is the same as `block-goto-command' with prefix ARG."
  (interactive)
  (block-goto-command t))

;;; The advice subfunctions
;;
;; The main task is to advice all functions to respect the block instead
;; of the region. In order to simplify switching on/off all the generated
;; advices, we keep them in the `block-advice-list'.
;; To automize this, we always use `block-advice-code' to define/activate
;; the corresponding advice which automatically updates `block-advice-list'
;; and does the error-handling.
;; The crucial point is that `block-advice-code' does not directly get
;; the code of the advice but only a function generating the required code.
;; This function is only exectued if the advice really needs to be
;; (re)calculated.

(defun block-advice-code (func gen-func-name &optional gen-func arglist
  force pos flag name nostore)
  "Advise code for function FUNC and store it in `block-advice-list'.
  FORCE determines what should happen if FUNC is already contained in
`block-advice-list':
    nil            the advice is enabled and activated but not changed.
                   (if the entry in `block-advice-list' exists and is nil,
                   the advice is of course not activated).
    a number       the advice is neither changed nor enabled.
    anything else  the advice is redefined (activation depends on FLAG).
  If the advice needs to be (re)defined, then (apply GEN-FUNC ARGLIST) is
used to calculate the code. More precisely, GEN-FUNC must return a list
consisting of the following items (the list might actually be shorter):
  1. The code as a list
  2. The class of the advice (see `defadvice'). Default is `after'.
  3. The new interactive specification (requires class `before' or `around')
     (or nil if the specification should not be changed).
  4. Whatever else should be stored in `block-advice-list'.
If GEN-FUNC is nil, then the function-value of GEN-FUNC-NAME is used instead.
GEN-FUNC-NAME is also used for error messages and is stored as the
generating function in `block-advice-list'.
  POS is used as a position argument for the advice (see `defadvice').
  FLAG is the argument used for the advice (see `defadvice'). The default
is `activate'.
  If NAME is non-nil, this name is used instead of `block-advice'.
  If NOSTORE is non-nil, FUNC is not stored in `block-advice-list' if it did
did not exist there. However, if FUNC was in `block-advice-list' the
corresponding value is changed unless NOSTORE is a number.
  The return value is the entry which is stored (or should be stored)
in `block-advice-list' (resp. nil in case of an error).
  If the variable `block-scan-error' is nil, then it is not an error
to pass a FUNC which is not a function - in this case nothing happens and
nil is returned.
  The case that GEN-FUNC-NAME is nil is special: In this case the advice is
deactivated (if it was in `block-advice-list' and FORCE is nil)
and it is stored into `block-advice-list' with an empty entry (unless NOSTORE
is nonempty) - this has the effect that further redefinitions of the advice
by `block-advice-code' are ignored unless FORCE is used."
  (if (null (block-functionp func))
      (if block-scan-error
        (error "%S: %S is no function" gen-func-name func))
    (let ((newval nil)
          (entry (assq func (symbol-value block-advice-list-name))))
      (if (and entry
            (if force
                (numberp force)
              (when (setq newval (cdr entry))
                (if gen-func-name
                  (ad-enable-advice func (nth 0 newval) (nth 1 newval))
                  (ad-disable-advice func (nth 0 newval) (nth 1 newval)))
                (ad-activate func))
              t))
           entry; return value
        (when gen-func-name
          (or name (setq name 'block-advice))
          (or flag (setq flag 'activate))
          ;; call the generating function
          (let (class head code acode
                (rlist (if gen-func
                           (apply gen-func arglist)
                         (apply gen-func-name arglist))))
            ;; interpret return values.
            (setq code (nth 0 rlist))
            (setq class (or (nth 1 rlist) 'after))
            (setq head (nth 2 rlist))
            (setq rlist (nthcdr 3 rlist))
            ;; rlist is now the additional data to be stored
            ;; in `block-advice-list'
            ;; calculate and execute the `defadvice' command
            (setq acode
              (append (list 'defadvice func
                            (if pos (list class name pos flag)
                                    (list class name flag)))
                      (if head (list (list 'interactive head)))
                      code))
            (when block-debug
              (insert (format "\nAdvice function %S (%S) with:\n" func class))
              (if head (insert (format "  Interactive: %s\n"
                                         (pp-to-string head))))
              (if code (insert (format "  %s\n" (pp-to-string code)))))
            (funcall (list 'lambda '() acode))
            (setq newval (list class name gen-func-name pos flag))
            (if rlist
                (nconc newval rlist)
              (block-remove-trail newval))))
        ;; update `block-advice-list'
        (if entry
          (if (numberp nostore)
              (cons func newval); return value
            (setcdr entry newcal)
            entry); return value
          (unless nostore
            (setq entry (cons func newval))
            (set block-advice-list-name
              (cons entry (symbol-value block-advice-list-name))))
          entry))))); return value

(defun block-advice-std (func gen-func-name &rest arglist)
  "This is similarly to `block-advice-code' with the only difference that the
default arguments are all nil and the arguments ARGLIST to GEN-FUNC-NAME can
be passed directly at the end, not as a list."
  (block-advice-code func gen-func-name nil arglist))


;;; The main generating functions
;;  for advice code are `block-advice-get' and `block-advice-set'.
;;  Essentially, the former is used to define the block after a function
;;  defined a region while the latter is used to pass the block to fnuctions
;;  acting on regions. If both needs to be combined, we also use
;;  `block-advice-set'.
;;  In most cases, `block-advice-set' is not used directly, because it needs
;;  informations about the interactive specification of the adviced function:
;;  Therefore, it is more convenient to use the wrapper function
;;  `block-set-advising' which calculates this information for non-autoloaded
;;  functions.
;;  Also for `block-advice-get' a wrapper function `block-get-advising' is
;;  provided

(defun block-advice-hide ()
  "This is a generating function to be used by `block-advice-code'.
It generates code which calls `block-hide-leave-mark' if the mark is set
after the adviced functions.
No additional data is store in `block-advice-list'."
  (list '((if (block-region-is-active) (block-hide-leave-mark)))))

(defun block-advice-get (&optional save define-cmd &rest cmd-args)
  "This is a generating function to be used by `block-advice-code'.
It generates the following code for the advice:
  1. After the call of the adviced function, the current region is stored
     as the block, using DEFINE-CMD with CMD-ARGS as arguments.
     DEFINE-CMD defaults to `block-define-command'.
  2. If SAVE is non-nil, the old cursor position is restored after the
     adviced function is executed.
The additional data stored in `block-advice-list' are all arguments in
their order (DEFINE-CMD is replaced by the function actually used)."
  (setq define-cmd (nconc (list (or define-cmd 'block-define-command))
                          cmd-args))
  (let ((code (list define-cmd))
        (class nil))
    (if save
      (progn
        (setq class 'around)
        (setq code (list (nconc (list 'block-save-point 'ad-do-it) code)))))
    (nconc (list code class nil save) define-cmd)))

(defun block-get-advising (func &optional save mode)
  "Advise FUNC by `block-advice-code' with the generating function
`block-advice-get' such that the following happens:
  1. After the call of the adviced function, the current region is stored
     as the block, using `block-define-command'.
  2. If SAVE is non-nil, the old cursor position is restored."
  (block-advice-std func 'block-advice-get save nil 0 nil t))

(defun block-advice-set (newspec rpos &optional barf
  mode same active define-cmd &rest cmd-args)
  "This is a generating function to be used by `block-advice-code'.
It generates the code needed for `block-set-advising' and the meaning of the
parameters essentially corresponds to that function.
The first two/three parameters related to the interactive specification
have to be given in addition:
  NEWSPEC is the interactive specificiation returned to `block-advice-code'.
It should have \"i\\ni\" in place of the first \"r\" of the original region.
Moreover, usually you should drop any leading \"*\" in the interactive
specification string; if you have done this, you should set BARF to t
to emulate the behaviour of \"*\": If BARF is t, then the adviced function
will barf if the buffer with the block is read-only. (In contrast, \"*\"
barfs if the current buffer is read-only).
  RPOS is the position (as a number, starting from 0) of the first of the
two arguments which are used as `region' arguments in the function
to be adviced, i.e. the position where the \"r\" is as a number.
If the interactive specification string of the adviced function
contained no \"r\", then RPOS must not be a number. If in this case RPOS
is nil, it is not distinguished between interactive and noninteractive calls.
Of course, if RPOS is not a number, ACTIVE should be non-nil, because
otherwise the advice is pretty useless.
  If DEFINE-CMD is non-nil, (DEFINE-CMD CMD-ARGS) is used after the adviced
function to store the current region as the block. If DEFINE-CMD is non-nil
but no function, then the default value `block-define-command' is used.
  The additional data stored in `block-advice-list' are all arguments in
their order (all optional trailing nil arguments are removed)."
  (setq define-cmd
    (and define-cmd
      (nconc
        (list (if (block-functionp define-cmd)
                   define-cmd
                'block-define-command))
        cmd-args)))
  (let (code force-data (class 'around) (force-block '())
        (test-arg (if rpos '(interactive-p) t))
        (save-command (if active 'save-excursion 'save-current-buffer))
        (new-tail (and define-cmd (list define-cmd)))
        (block-def (list (block-remove-trail
                           (list 'block-define-if-possible mode same)))))
    (if (numberp rpos)
      (setq
        force-block
          `((let ((s (block-start-char)) (e (block-end-char)))
              (if (< e s)
                (progn
                  (ad-set-arg ,rpos e)
                  (ad-set-arg ,(1+ rpos) s))
                (ad-set-arg ,rpos s)
                (ad-set-arg ,(1+ rpos) e))))
        test-arg `(null (or (ad-get-arg ,rpos)
                            (ad-get-arg ,(1+ rpos))))))

    ;; Let force-block be code which modifies the arguments of the
    ;; adviced function and if necessary also current-buffer to
    ;; contain the block and barfs if necessary.
    ;; Moreover, in case of ACTIVE set mark and point correspondingly.
    (or same
      (setq force-block
        (append force-block '((set-buffer (block-buffer))))))
    (if barf
      (setq force-block
        (append force-block '((barf-if-buffer-read-only)))))
    (if active
      (setq force-block
        (append force-block '((set-mark (block-start-char))
                             (goto-char (block-end-char))))))

    ;; Let force-data be code which calls `block-define-if-possible'
    ;; and modifies the arguments as well as buffer, mark, and point,
    ;; if appropriate.
    (setq force-data (append block-def force-block))

    ;; Define the main code:
    ;;
    ;; (A) If active is a number:
    ;; (if test-arg
    ;;   (save-command force-data ad-do-it new-tail) (*)
    ;;   ad-do-it
    ;;   new-tail)
    ;;
    ;; (B) If active is t:
    ;; (save-command
    ;;   (when test-arg force-data) (*)
    ;;   ad-do-it
    ;;   new-tail)
    ;;
    ;; (C) If same is nil (and active is nil):
    ;; (if test-arg
    ;;   (save-command force-data ad-do-it new-tail) (*)
    ;;   ad-do-it
    ;;   new-tail)
    ;;
    ;; (D) If same is t (and active is nil):
    ;;     (when test-arg force-data) (*)
    ;;     ad-do-it
    ;;     new-tail
    ;;    [Respecively only (*) as `before'-advice if new-tail is nil]

    (setq
      ;; As an intermediate step, we save the line marked with (*) in code:
      code
        (if (or (numberp active) (and (null same) (null active)))
            ;; Case (A) or (C)
            (append (list save-command)
                    force-data '(ad-do-it) new-tail)
          ;; Case (B) or (D)
          (append (list 'when test-arg) force-data))
      ;; Now we build the actual code:
      code
        (if (and active (null (numberp active)))
            ;; Case (B)
            (list (nconc (list save-command code 'ad-do-it) new-tail))
          (if (or (numberp active) (null same))
              ;; Case (A) or (C)
              (list (nconc (list 'if test-arg code 'ad-do-it) new-tail))
            ;; Case (D)
            (if new-tail
                (list (nconc (list code 'ad-do-it) new-tail))
              (setq class 'before)
              (list code))))
      ;; calculate return value, misusing code as an intermediate value.
      code (list code class newspec newspec rpos barf mode same active))
      (if define-cmd
          (nconc code define-cmd)
        (block-remove-trail code 5))))

(defun block-set-advising (func &optional mode same active deflist spec auload
  force pos flag name nostore)
  "If FUNC contains a string interactive specification containing the
character \"r\", then FUNC is advised by `block-advice-code' with the
generating function `block-advice-set' in the following way:
  1. The interactive spec is changed so that no region is required.
  2. Instead of the region the block is used as argument.
     If necessary, the buffer with the block becomes temporarily
     the active buffer.
In addition, the name of FUNC with the corresponding class is remembered in
`block-advice-list' (if it was not already in that list).
The classname of the generated advice is `block-advice'.
The return value of `block-set-advising' is the value stored in
`block-advice-list' (resp. nil in case of an error).
The arguments modify the above behaviour in the following way.

MOVE and SAME have analogous meanings as in `block-define-if-possible'.
  If ACTIVE is non-nil, then in addition to the change of arguments, the
region is temporarily defined as the current block (when FUNC is called).
If ACTIVE is a number, this happens only if the function is called
interactively.
  If DEFLIST is non-nil, it should be a list of the form (def-command args).
In this case, this command is used after the adviced function to store the
current region as the block. If def-command is no function, the default value
`block-define-command' is used. The same happens if DEFLIST is not a list.
  If the SPEC argument is a string, this is used instead of the original
interactive spec of the function FUNC. This is useful to specify if the
interactive spec is no string or is not known yet (e.g. for autoloaded
functions).
  If AULOAD is non-nil, then autoloaded functions are loaded and adviced.
By default, autloaded functions cause an error, because `block-set-advising'
must call (interactive-form FUNC) which would load them although this is
probably undesired.
  The remaining arguments are passed to `block-advice-code', and the
return value is that of `block-advice-code'.
  If the variable `block-scan-error' is non-nil, it is an error
to pass a FUNC which is not a function or (if SPEC is not provided)
which has no interactive string spec containing an \"r\" argument. Moreover,
in this case it is also an error to pass an autoloaded function, unless
`block-scan-error' is a number."
  (let ((ftype (block-functionp func)))
    (if (null ftype)
        (if block-scan-error
          (error "block-set-advising: %S is no function" func))
      (if (and (null auload) (eq 'autoload ftype))
          (and block-scan-error (null (numberp block-scan-error))
            (error "block-set-advising: %S is autoloaded function" func))
        (let ((sp spec) (barf nil))
          (or spec (setq sp (cadr (interactive-form func))))
          ;; Now sp contains the interactive spec
          (if (null (stringp sp))
            (if spec ; if we were passed a spec explicitly as an argument:
                ;; We were passed stupid arguments: Ignore block-scan-error
                (error "block-set-advising (%S): SPEC argument is no string"
                       func)
              (if block-scan-error
                (error
                   "block-set-advising: %S has no string as interactive spec"
                   func)))
            (let ((rpos 0)
                  (newspec nil)
                  (case-fold-search nil)
                  (spw (setq sp (split-string sp "\n"))))
              ;; Now sp is a list of strings of the interactive arguments:
              ;; We assume that all these arguments are separated by "\n".
              ;; We use spw to run through the list.
              (while spw
                ;; An interactive argument may start with an optional * or @
                ;; and then a letter follows. We are interested in the type
                ;; "r" but also remember if an "*" occurs and eliminate it.
                (when (string-match
                         "^\\\([^a-zA-Z]*\\\)\\\*\\\(.*\\\)$" (car spw))
                  (setcar spw (concat (match-string 1 (car spw))
                                      (match-string 2 (car spw))))
                  (setq barf t))
                (if (string-match "^\\\([^a-zA-Z]*\\\)r" (car spw))
                  (progn
                    (setcar spw (concat (match-string 1 (car spw)) "i\ni"))
                    (setq newspec (mapconcat 'identity sp "\n"))
                    (setq spw nil)); exit loop
                  (setq rpos (1+ rpos) spw (cdr spw))))
              ;; Now newspec contains the new interacitve specification string;
              ;; rpos contains numerical position of the first region argument.
              (if newspec; of course all this requires that an "r" was found.
                (let ((cmd-list (list newspec rpos barf mode same active)))
                  (if deflist
                      ;; the test whether the first element is a function
                      ;; is done in `block-advice-set'
                      (if (listp deflist)
                          (nconc cmd-list deflist)
                        (list t)); t is neither nil nor a function -> default
                    (block-remove-trail cmd-list 2))
                  (block-advice-code func 'block-advice-set nil cmd-list
                    force pos flag name nostore))
                ;; no "r" was found in interactive spec (resp. in SPEC):
                (if block-scan-error
                  (error
                    "block-set-advising (%S): spec contains no \"r\""
                    func))))))))))

;;; Some aliases and wrappers for simpler accessing of the block functions.
;;
;; One may argue whether these functions are useful, but I left them in
;; for "historical" reasons (and my convenience):
;; They cause no harm and do not take much space.
;; Just don't use them if you don't like them.

(defcustom block-register 257
  "The default register number for `block-copy-register' and related commands."
  :type 'integer
  :group 'block)

(defun block-copy-register (n beg end)
  "Similarly to copy-to-register but with default register `block-register'
and no DELETE-FLAG. For the latter use `block-cut-register'."
  (interactive "P\nr")
  (or n (setq n block-register))
  (copy-to-register n beg end)
  (message "Block copied into register %d" n))

(defun block-cut-register (n beg end)
  "Similarly to copy-to-register with DELETE-FLAG and default register
`block-register'."
  (interactive "P\nr")
  (or n (setq n block-register))
  (copy-to-register n beg end t)
  (message "Block cut into register %d" n))

(defun block-paste (n)
  "Similarly to `insert-register' but with non-nil ARG and default register
`block-register'."
  (interactive "*P")
  (or n (setq n block-register))
  (insert-register n t))

(defun block-rectangle-copy-register (n beg end)
  "Similarly to copy-rectangle-to-register but with default register
`block-register' and no DELETE-FLAG. For the latter use `block-cut-register'."
  (interactive "P\nr")
  (or n (setq n block-register))
  (copy-rectangle-to-register n beg end)
  (message "Block-rectangle copied into register %d" n))

(defun block-rectangle-cut-register (n beg end)
  "Similarly to copy-to-register with DELETE-FLAG and default register
`block-register'."
  (interactive "P\nr")
  (or n (setq n block-register))
  (copy-rectangle-to-register n beg end t)
  (message "Block-rectangle cut into register %d" n))

(defalias 'block-rectangle-pase 'block-paste)

(defalias 'block-indent 'indent-rigidly)

(defun block-unindent (start end arg)
  "Similarly to `indent-rigidly', but with negative ARG."
  (interactive "r\np")
  (indent-rigidly start end (- arg)))

(defalias 'block-comment 'comment-region)

(defun block-uncomment (beg end arg)
  "Similarly to `comment-region', but with negative ARG."
  (interactive "r\np")
  (comment-region beg end (- arg)))

(defalias 'block-begin 'block-push-mark-command)
(defalias 'block-begin-move 'block-redefine-start-command)
(defalias 'block-end 'block-define-command)
(defalias 'block-line-begin 'block-line-command)
(defalias 'block-line-end 'block-line-include-command)
(defalias 'block-hide 'block-hide-command)
(defalias 'block-copy 'block-copy-command)
(defalias 'block-move 'block-move-command)
(defalias 'block-up 'upcase-region)
(defalias 'block-down 'downcase-region)
(defalias 'block-read 'insert-file)
(defalias 'block-write 'write-region)
(defalias 'block-sort-lines 'sort-lines)
(defalias 'block-fill 'fill-region)
(defalias 'block-fill-as-paragraph 'fill-region-as-paragraph)
(defalias 'block-ispell 'ispell-region)
(defalias 'block-flyspell 'flyspell-region)
(defalias 'block-mark-paragraph 'mark-paragraph)


;;; The functions for setting/deleting all advices at once.

(defun block-advise-all (&optional mode)
  "Advise all accessible functions to obey block instead of region.
It may be useful to call this function manually after a new function
definition (e.g. by autoload).
The value of MODE decides which advices should be recalculated:
  nil          Only advise new (e.g. autoloaded) functions generically.
  0            In addition, enable and activate all other (explicit) advices.
  other value  In addition, regenerate all advices, even those which could be
               re-enabled.
Before any advice is (re)created, `block-advice-hook' is run if the explicit
advices should be enabled/generated. In all  cases, `block-advice-always-hook'
is run before that."
  (interactive "P")
  (when (and mode (null (eq 0 mode)))
    ;; Disable the old advices and actually clear `block-advice-list'
    ;; so that all following advices really are recreated:
    (block-disable-advices)
    (set block-advice-list-name nil))

  (run-hooks 'block-advice-always-hook)

  (when mode
    ;; First, define those "explicit" advices which need a special treatment:
    ;; You may want to use something like (interactive-form 'comment-region)
    ;; to find out parameters for candidates for `block-advice-set'
    ;; which are not handled automatically (because they are e.g. autoloaded).
    ;; `block-advice-hook' is meant for you to add advices for your own
    ;; functions or for functions which are not treated by default
    ;; or for which you want to change the default advice.
    ;; All advices in this hook (which should be done using
    ;; `block-advice-code' or some of its wrapper functions)
    ;; override the subsequent defaults, because adviced functions are
    ;; not adviced again (unless the FORCE option is used).
    ;; This is the reason why the hook is run *before* setting the default
    ;; advices.
    (run-hooks 'block-advice-hook)

    ;; save-buffer gets broken in emacs-23 if we would advice write-region:
    (block-advice-code 'write-region nil); `write-region' is *not* adviced.

    ;; Advise those functions which need the block as "argument"
    ;; and which cannot be treated generically with `block-set-advising':
    (let ((block-scan-error nil))
      (block-advice-std 'tabify 'block-advice-set "i\ni" 0 nil 1)
      (block-advice-std 'untabify 'block-advice-set "i\ni" 0 nil 1)
      (block-advice-std 'ispell-region 'block-advice-set "i\ni" 0 nil 1)
      (block-advice-std 'flyspell-region 'block-advice-set "i\ni" 0 nil 1)
      (block-advice-std 'sort-lines 'block-advice-set "P\ni\ni" 1 t 1)
      (block-advice-std 'comment-region 'block-advice-set "i\ni\nP" 0 t 1)
      (block-advice-std 'fill-region 'block-advice-set
        '(list nil nil (if current-prefix-arg 'full)) 0 t 1)
      (block-advice-std 'fill-region-as-paragraph 'block-advice-set
        '(list nil nil (if current-prefix-arg 'full)) 0 t 1))

    ;; Advise those functions which might define a new block
    ;; and which do not have the name `mark-*':
    (if (block-functionp 'mouse-drag-region); not XEmacs
      (block-advice-std 'mouse-drag-region 'block-advice-get nil
         ;;nil -1 nil t);; This breaks with emacs-23, so we use instead:
         'block-mark-to-block)
      ;; XEmacs
      (defun block-mouse-track-cleanup-hook () (block-define-command -1 nil t))
      (add-hook 'mouse-track-cleanup-hook 'block-mouse-track-cleanup-hook))
    (if (block-functionp 'mouse--drag-set-mark-and-point)
      (block-get-advising 'mouse--drag-set-mark-and-point))
    (block-get-advising 'insert-register)
    (block-get-advising 'yank)
    (block-get-advising 'yank-pop)
    (block-get-advising 'yank-rectangle)
    (block-get-advising 'insert-file)

    ;; Advise those functions with the name `mark-*' which cannot be treated
    ;; generically with `block-get-advising':
    (block-advice-code 'mark-marker nil)); `mark-marker' is *not* adviced.


  ;; Now we treat essentially all symbols with `block-set-advising'
  ;; except for those functions named `mark-*' - those are treated with
  ;; `block-get-advising' (with SAVE t).
  (let ((block-scan-error nil))
    (mapatoms (lambda (s)
      ;; We must not advise functions which are named `ad-*',
      ;; because these functions are used internally by the advice package
      ;; (e.g. to store the "original" functions).
      ;; It might be sufficient to exclude only `ad-Orig-*', but we want to
      ;; be on the save side in case that name conventions changes one day:
      ;; It seems rather unlikely that one day a function of name `ad-*'
      ;; needs a region argument.
      (let ((f (format "%s" s)))
        (cond
         ((string-match "^ad-" f)
          nil)
         ((string-match "^mark-" f)
          (block-get-advising s t))
         (t
          (block-set-advising s))))))))

(defun block-disable-advices ()
  "Run block-disable-advices-hook and disable all advices of
`block-advice-list'."
  (run-hooks 'block-disable-advices-hook)
  (let (c)
    (dolist (s (symbol-value block-advice-list-name))
      (when (setq c (cdr s))
        (ad-disable-advice (car s) (nth 0 c) (nth 1 c))
        (ad-activate (car s))))))

;;; The keymap stuff

;; Personally, I prefer to use the `block-mode-prefix-map'
;; (with the "plain" keys omitted) as the `block-mode-map'
;; which is much more convenient, but which requires also
;; that some other standard Emacs commands be remapped.
;; Thus, in order to keep up with Emacs standard keymaps
;; I reserve in this package only one prefix-key for this mode.

(defvar block-mode-prefix-map (let ((m (make-sparse-keymap)))
  (define-key m "b"    'block-set-mark-command)
  (define-key m "\C-b" 'block-push-mark-command)
  (define-key m "\M-b" 'block-line-command)
  (define-key m "\M-\C-b" 'block-redefine-start-command)
  (define-key m "k"    'block-define-command)
  (define-key m "\C-k" 'block-define-command)
  (define-key m "\M-\C-k" 'block-define-command)
  (define-key m "\M-k" 'block-line-include-command)
  (define-key m "h"    'block-hide-command)
  (define-key m "\C-h" 'block-hide-command)
  (define-key m "c"    'block-copy-command)
  (define-key m "\C-c" 'block-copy-command)
  (define-key m "\M-c" 'block-copy-register)
  (define-key m "\M-\C-c" 'block-copy-register)
  (define-key m "v"    'block-move-command)
  (define-key m "\C-v" 'block-move-command)
  (define-key m "\M-\C-v" 'block-paste)
  (define-key m "\M-v" 'block-paste)
  (define-key m "m"    'block-move-command)
  (define-key m "\C-m" 'block-move-command)
  (define-key m "f"    'block-read)
  (define-key m "\C-f" 'block-read)
  (define-key m "\M-f" 'block-write)
  (define-key m "e"    'block-unindent)
  (define-key m "\C-e" 'block-unindent)
  (define-key m "\M-e" 'block-indent)
  (define-key m ","    'block-uncomment)
  (define-key m [?\C-,] 'block-uncomment)
  (define-key m [?\M-,] 'block-comment)
  (define-key m "."    'block-goto-command)
  (define-key m [?\C-.] 'block-goto-command)
  (define-key m [?\M-.] 'block-goto-end-command)
  (define-key m [?\M-\C-.] 'pop-to-mark-command)
  (define-key m "-"    'block-mark-paragraph)
  (define-key m [?\C--] 'block-mark-paragraph)
  (define-key m "y"    'block-cut-register)
  (define-key m "\C-y" 'block-cut-register)
  (define-key m "\M-y" 'block-cut-register)
  (define-key m "\M-\C-y" 'block-cut-register)
  (define-key m "a"    'block-advise-all)
  (define-key m "\C-a" 'block-advise-all)
  m))

(defvar block-mode-menu-bar-map (let ((m (make-sparse-keymap)))
  (define-key m [block] (cons "Block" (make-sparse-keymap "Block")))
  (define-key m [block block-goto-end-command]
    '("Goto (Pop) to Mark" . pop-to-mark-command))
  (define-key m [block block-goto-end-command]
    '("Goto Block End" . block-goto-end-command))
  (define-key m [block block-goto-command]
    '("Goto Block Start" . block-goto-command))
  (define-key m [block block-unindent]
    '("Unindent Block" . block-unindent))
  (define-key m [block block-indent]
    '("Indent Block" . block-indent))
  (define-key m [block block-uncomment]
    '("Uncomment Block" . block-uncomment))
  (define-key m [block block-comment]
    '("Comment Block" . block-comment))
  (define-key m [block block-write]
    '("Write Block" . block-write))
  (define-key m [block block-read]
    '("Read Block" . block-read))
  (define-key m [block yank-pop]
    '("Repaste Next from Kill Ring (yank-pop)" . yank-pop))
  (define-key m [block yank]
    '("Paste Block from Kill Ring (yank)" . yank))
  (define-key m [block kill-ring-save]
    '("Copy Block to Kill Ring" . kill-ring-save))
  (define-key m [block kill-region]
    '("Cut Block to Kill Ring" . kill-region))
  (define-key m [block block-block-paste]
    '("Paste Block from Register" . block-paste))
  (define-key m [block block-rectangle-copy-register]
    '("Copy Rectangle Block to Register" . block-rectangle-copy-register))
  (define-key m [block block-rectangle-cut-register]
    '("Cut Rectangle Block to Register" . block-rectangle-cut-register))
  (define-key m [block block-copy-register]
    '("Copy Block to Register" . block-copy-register))
  (define-key m [block block-cut-register]
    '("Cut Block to Register" . block-cut-register))
  (define-key m [block block-move-command]
    '("Move Block" . block-move-command))
  (define-key m [block block-copy-command]
    '("Copy Block" . block-copy-command))
  (define-key m [block block-hide-command]
    '("Hide Block" . block-hide-command))
  (define-key m [block block-mark-paragraph]
    '("Mark Paragraph as Block" . block-mark-paragraph))
  (define-key m [block block-line-include-command]
    '("Include Line to Block" . block-line-include-command))
  (define-key m [block block-line-command]
    '("Mark Line as Block" . block-line-command))
  (define-key m [block block-redefine-start-command]
    '("Change Block Start" . block-redefine-start-command))
  (define-key m [block block-define-command]
    '("Block End" . block-define-command))
  (define-key m [block block-push-mark-command]
    '("Block Start (Push)" . block-push-mark-command))
  (define-key m [block block-set-mark-command]
    '("Block Start" . block-set-mark-command))
  (define-key m [block block-advise-all]
    '("Recalculate Advices (call this after autoloads)" . block-advise-all))
  m))

(defcustom block-prefix-key "\C-b"
  "The face that is used to display the block."
  :type 'string
  :group 'block)

(defvar block-mode-map (let ((m (make-sparse-keymap)))
  (define-key m block-prefix-key block-mode-prefix-map)
  (define-key m [menu-bar] block-mode-menu-bar-map)
  m))

;;; Finally, the main function for the mode

;;;###autoload
(defun block-mode-update ()
  "This function should be called whenever `block-mode' has possibly changed."
  (if block-mode
    (progn
      (when block-forces-transient
        (if (functionp 'transient-mark-mode); not XEmacs
          (progn
            (setq block-transient-state transient-mark-mode)
            (transient-mark-mode 1))
          ;; XEmacs
          (setq block-transient-state zmacs-regions
                zmacs-regions t)))
      (block-advise-all 0))
    (block-disable-advices)
    (block-hide-leave-mark)
    (if block-forces-transient
      (if (functionp 'transient-mark-mode); not XEmacs
          (transient-mark-mode block-transient-state)
        ;; XEmacs
        (setq zmacs-regions block-transient-state)))))

(when (string-match "XEmacs\\|Lucid" emacs-version)
  ;; XEmacs has a completely different `define-minor-mode', so we patch:
  (if (null (functionp 'define-minor-mode))
    ;; first provide a dummy definition if necessary:
    (defun define-minor-mode ()))
  (defvar block-mode nil "Non-nil if block-mode is enabled.
Use the command `block-mode' to change this variable.")
  (defadvice define-minor-mode (around block-minor-mode first
    (block-arg-dummy block-arg-comment &rest block-arg-remainder) activate)
    (funcall
      (list 'lambda '()
        ;; Our new definition of `block-mode' as a list
        (list 'defun 'block-mode '(&optional arg) block-arg-comment
          '(interactive (list (or current-prefix-arg 'toggle)))
          '(setq block-mode (cond
            ((eq arg 'toggle) (null block-mode))
            (arg (> (prefix-numeric-value arg) 0))
            (t (null block-mode))))
          '(block-mode-update)
          '(run-hooks 'block-mode-hook (if block-mode 'block-mode-on-hook
                                                      'block-mode-off-hook))
          '(force-mode-line-update)
          'block-mode)))
    (add-to-list 'minor-mode-map-alist (cons 'block-mode block-mode-map))
    ;; The advice runs only once, i.e. it deactivates itself:
    (ad-disable-advice 'define-minor-mode 'around 'block-minor-mode)
    (ad-activate 'define-minor-mode)))

;;;###autoload
(define-minor-mode block-mode
  "Toggle Block mode.
When enabled, the block is used instead of regions.
There are a couple of new commands available, all prefixed with C-b:
  C-b C-a: (Re)activate all advices. This may be necessary after autoloads or
           after new definitions. With (nonzero) prefix arg, the reactivation
           takes much longer but might help you out of unexpected problems.
  C-b C-b: `push-mark-command', starting a new block.
  C-b b  : `set-mark-command', starting a new block.
  C-b C-k: Mark the end of the block. With prefix arg, this is the same as
  C-b M-C-b: Move the start of the block.
  C-b M-b: Mark the current line (including line-end) as the block.
  C-b M-k: Include the current line (including line-end) into the block
  C-b C-.: Mark current paragraph as the block
  C-b C-h: Deactive (hide) the block
  C-b C-c: Copy the block to point
  C-b C-m: Move the block to point
  C-b M-y: Cut  the block to register
  C-b M-c: Copy the block to register
  C-b M-v: Paste the block from register
  C-b C-f: Insert block from file
  C-b M-f: Write block to file
  C-b C-e: Unindent block
  C-b M-e: Indent block
  C-b C-,: Uncomment block
  C-b M-,: Comment block
  C-b C-.: Goto start of the block
  C-b M-.: Goto end of the block
  C-b M-C-.: `pop-to-mark-command', goto mark and pop the last one pushed
For most keys the second \"C-\" can be omitted."
  :global t
  :require 'block
  :link '(emacs-commentary-link "block.el")
  :keymap 'block-mode-map
  (block-mode-update))


(provide 'block)

;;; Typical commands needed for debugging (use with C-x C-e):
;;  (progn (block-disable-advices) (setq block-advice-list nil))
;;  (setq block-scan-error nil)
;;  (setq block-debug t)
;;  (block-advise-all t)

