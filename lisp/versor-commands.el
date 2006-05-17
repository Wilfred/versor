;;; versor-commands.el -- versatile cursor commands
;;; Time-stamp: <2006-05-10 20:23:41 jcgs>
;;
;; emacs-versor -- versatile cursors for GNUemacs
;;
;; Copyright (C) 2004, 2005, 2006  John C. G. Sturdy
;;
;; This file is part of emacs-versor.
;; 
;; emacs-versor is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; emacs-versor is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with emacs-versor; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(provide 'versor-commands)
(require 'versor-menu)
(require 'versor-trim-whitespace)

;;;;;;;;;;;;;;;;;;;
;;;; reversing ;;;;
;;;;;;;;;;;;;;;;;;;

(defvar versor-reversed nil
  "Whether we have currently reversed the navigation directions.
This is useful if you cannot use \"shift-next\" for \"previous\".
It is enabled by the variable versor-reversible, which see.")

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; more structure ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro versor-as-versor-command (&rest versor-body)
  "Run BODY as a versor command, if versor-mode is enabled,
or if not called interactively.
Otherwise, do whatever was bound to the key that caused this
command to be run."
  `(if (or versor-mode
	   (not (interactive-p)))
       (progn
	 ,@versor-body)
     (let ((old-def (lookup-key versor-original-bindings-map
			       (this-command-keys-vector))))
       (if old-def
	   (call-interactively old-def)))))

(defvar versor-pre-command-hook nil
  "Hooks to run at the start of versor commands.")

(defvar versor-post-command-hook nil
  "Hooks to run at the end of versor commands.")

(defmacro versor-as-motion-command (item-var &rest body)
  "Run BODY as a versor motion command.
Necessary pre- and post-processing get done.

The BODY may call versor-set-current-item to show us where
it has decided the item is to be; otherwise we fiddle around
with calling the end-of-item of the dimension, or the next
if there is no end-of-item."
  `(versor-as-versor-command
     (let ((,item-var (if (versor-current-item-valid)
			  (versor-get-current-item)
			(car versor-latest-items))))
       (run-hooks 'versor-pre-command-hook)
       (versor-clear-current-item-indication)
       (progn
	 ,@body)
       ;; the few commands that want to do otherwise, must re-set this
       ;; one just after using this macro
       (setq versor-extension-direction nil)
       (versor-indicate-current-item)
       (run-hooks 'versor-post-command-hook))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; commands begin here ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; meta-dimension commands

(defun versor-out ()
  "Move versor-level out a dimension in (versor-current-meta-level)."
  (interactive)
  (versor-as-motion-command current-item
    (setq versor-old-level versor-level)
    (if (and (interactive-p)
	     versor-reversed)
	(decf versor-level)
      (incf versor-level))
    (versor-trim-level)
    (versor-set-status-display
     (first (versor-current-level))
     (mapcar 'first (versor-level-names))
     t)))

(defun versor-in ()
  "Move versor-level in a dimension in (versor-current-meta-level)."
  (interactive)
  (versor-as-motion-command current-item
   (setq versor-old-level versor-level)
   (if (and (interactive-p)
	    versor-reversed)
       (incf versor-level)
     (decf versor-level))
   (versor-trim-level)
   (versor-set-status-display
    (first (versor-current-level))
    (mapcar 'first (versor-level-names))
    t)))

(defun versor-next-meta-level ()
  "Move to the next meta-level."
  (interactive)
  (versor-as-motion-command current-item
   (if (and (interactive-p)
	    versor-reversed)
       (decf versor-meta-level)
     (incf versor-meta-level))
   (versor-trim-meta-level)
   (while (not (versor-meta-dimension-valid-for-mode
		(aref (aref moves-moves versor-meta-level) 0)
		major-mode))
     (if (and (interactive-p)
	      versor-reversed)
	 (decf versor-meta-level)
       (incf versor-meta-level))
     (versor-trim-meta-level))
   (versor-trim-level)
   (versor-set-status-display
    (aref (versor-current-meta-level) 0)
    (mapcar 'first (versor-meta-level-names))
    t)))

(defun versor-prev-meta-level ()
  "Move to the previous meta-level."
  (interactive)
  (versor-as-motion-command current-item
   (if (and (interactive-p)
	    versor-reversed)
       (incf versor-meta-level)
     (decf versor-meta-level))
   (versor-trim-meta-level)
   (while (not (versor-meta-dimension-valid-for-mode
		(aref (aref moves-moves versor-meta-level) 0)
		major-mode))
     (if (and (interactive-p)
	      versor-reversed)
	 (decf versor-meta-level)
       (incf versor-meta-level))
     (versor-trim-meta-level))
   (versor-trim-level)
   (versor-set-status-display
     (aref (versor-current-meta-level) 0)
     (mapcar 'first (versor-meta-level-names))
     t)))

;;;; commands within the current dimension(s)

(defun versor-reverse ()
  "Reverse the forward and backward directions."
  (interactive)
  (versor-as-versor-command
    (if (not versor-reversible)
	(error "Reversing movement directions is not enabled; set versor-reversible to enable it")
      (setq versor-reversed (not versor-reversed))
      (versor-set-status-display))))

(defun versor-get-action (action &optional level-offset)
  "Get ACTION for the current versor level, taking modal bindings into account.
With optional LEVEL-OFFSET, add that to the level first.
Modal bindings are made by versor-bind-modal, which is in versor-modal.el"
  (let* ((level-data (versor-current-level level-offset))
	 (level-modal-data (cdr (assoc major-mode (versor-action level-data 'modal-bindings))))
	 (modal-binding (if level-modal-data (versor-action level-modal-data action) nil)))
    (if modal-binding
	modal-binding
      (versor-action level-data action))))

(defvar versor-last-vicarious-command nil
  "The last interactive command that versor executed.")

(defvar versor-commands
  '(versor-start
    versor-prev
    versor-next
    versor-end
    versor-over-start
    versor-over-prev
    versor-over-next
    versor-over-end
    versor-other-end-of-item
    versor-dwim)
  "List of versor movement commands.
If last-command is one of these, we assume we made the last move.")

(defun versor-call-interactively (command)
  "Call COMMAND, as though it had been bound and typed directly."
  (when versor-display-underlying-commands
    (message "Versor using command %S" command))
  (setq versor-last-vicarious-command command)
  (call-interactively command))

(defvar versor-point-before-move nil
  "The value of point before the latest versor move.")

(defun versor-start (&optional level-offset)
  "Move backward to the start of the current dimension.
With optional LEVEL-OFFSET, add that to the level first.
If repeated, this undoes the first move, and goes back a meta-dimension."
  (interactive)
  (versor-as-motion-command current-item
   (unless (run-hook-with-args-until-success
	    (if versor-reversed
		'versor-end-hooks
	      'versor-start-hooks))
     (if (eq last-command this-command)
	 (progn
	   (versor-prev-meta-level)
	   (goto-char versor-point-before-move))
       (progn
	 (setq versor-point-before-move (point))
	 (versor-call-interactively
	  (versor-get-action (if versor-reversed 'last 'first) level-offset)))))))

(defun versor-prev-action (&optional level-offset)
  "Internal action code to move backward within the current dimension.
With optional LEVEL-OFFSET, add that to the level first."
  (unless (run-hook-with-args-until-success 'versor-prev-hooks)
    (setq versor-point-before-move (point))
    (versor-call-interactively (versor-get-action 'previous level-offset))))

(defun versor-next-action (&optional level-offset)
  "Internal action code to move forward within the current dimension.
With optional LEVEL-OFFSET, add that to the level first."
  (unless (run-hook-with-args-until-success 'versor-next-hooks)
    (setq versor-point-before-move (point))
    (versor-call-interactively (versor-get-action 'next level-offset))))

(defun versor-prev (&optional level-offset)
  "Move backward within the current dimension.
With optional LEVEL-OFFSET, add that to the level first."
  (interactive '(nil))
  (versor-as-motion-command current-item
   (if versor-reversed
       (versor-next-action level-offset)
     (versor-prev-action level-offset))))

(defun versor-next (&optional level-offset)
  "Move forward within the current dimension.
With optional LEVEL-OFFSET, add that to the level first."
  (interactive '(nil))
  (versor-as-motion-command current-item
   (if versor-reversed
       (versor-prev-action level-offset)
     (versor-next-action level-offset))))

(defun versor-end (&optional level-offset)
  "Move forward to the end of the current dimension.
With optional LEVEL-OFFSET, add that to the level first.
If repeated, this undoes the first move, and goes back a meta-dimension."
  (interactive)
  (versor-as-motion-command current-item
   (unless (run-hook-with-args-until-success
	    (if versor-reversed
		'versor-start-hooks
	      'versor-end-hooks))
     (if (eq last-command this-command)
	 (progn
	   (versor-next-meta-level)
	   (goto-char versor-point-before-move))
       (progn
	 (setq versor-point-before-move (point))
	 (versor-call-interactively
	  (versor-get-action (if versor-reversed 'first 'last) level-offset)))))))

(defun versor-over-start ()
  "Move backward to the start of the dimension above the current one.
If repeated, this undoes the first move, and goes back a meta-dimension."
  (interactive)
  (versor-as-versor-command
    (versor-start 1)))

(defun versor-over-prev ()
  "Move backward within the dimension above the current one."
  (interactive)
  (versor-as-versor-command
    (versor-prev 1)))

(defun versor-over-next ()
  "Move forward within the dimension above the current one."
  (interactive)
  (versor-as-versor-command
    (versor-next 1)))

(defun versor-over-end ()
  "Move forward to the end of the dimension above the current one.
If repeated, this undoes the first move, and goes back a meta-dimension."
  (interactive)
  (versor-as-versor-command
    (versor-end 1)))

(defun versor-over-over-start ()
  "Move backward to the start of the dimension two above the current one.
If repeated, this undoes the first move, and goes back a meta-dimension."
  (interactive)
  (versor-as-versor-command
    (versor-start 2)))

(defun versor-over-over-prev ()
  "Move backward within the dimension two above the current one."
  (interactive)
  (versor-as-versor-command
    (versor-prev 2)))

(defun versor-over-over-next ()
  "Move forward within the dimension two above the current one."
  (interactive)
  (versor-as-versor-command
    (versor-next 2)))

(defun versor-over-over-end ()
  "Move forward to the end of the dimension two above the current one.
If repeated, this undoes the first move, and goes back a meta-dimension."
  (interactive)
  (versor-as-versor-command
    (versor-end 2)))

(defun versor-end-of-item-position ()
  "Return the end of the current item."
  (let ((result
	 (let ((mover (or (versor-get-action 'end-of-item)
			  (versor-get-action 'next))))
	   (if mover
	       (save-excursion
		 (funcall mover 1)
		 (point))
	     nil))))
    result))

(defun versor-end-of-item ()
  "Move to the end of the current item."
  ;; perhaps this should change to move among the parts of a multipart item?
  (interactive)
    (versor-as-motion-command current-item
     (goto-char
      (if current-item
	  (cdr current-item)
	(versor-end-of-item-position)))
     (make-versor-overlay (car current-item) (cdr current-item))))

(defun versor-start-of-item ()
  "Move to the start of the current item."
  (interactive)
  (versor-as-motion-command current-item
   (if current-item
       (goto-char (car current-item))
     (versor-prev-action 1))))

(defun versor-other-end-of-item (&rest junk)
  "Move to the other end of the current item."
  (interactive)
  (message "versor-other-end-of-item %S" junk)
  (versor-as-motion-command item		; todo: this is leaving the wrong item highlighted, I think... when it moves to the end of the item, the following item is highlighted; not sure whether this affects the selection as well as the highlighting, but I presume it does
     (cond
      ((= (point) (car item))
       (goto-char (cdr item)))
      ((= (point) (cdr item))
       (goto-char (car item)))
      (t
       (let ((halfway (/ (+ (car item) (cdr item)) 2)))
	 (if (< (point) halfway)
	     (goto-char (cdr item))
	   (goto-char (car item))))))))

;;;;;;;;;;;;;;;;;;;;;
;;;; extend item ;;;;
;;;;;;;;;;;;;;;;;;;;;

(defvar versor-extension-direction nil
  "Which direction versor is extending in.
Once you are extending forwards, versor-extend-item-backwards will
reduce the extension rather than extending backwards, and vice versa.")

(make-variable-buffer-local 'versor-extension-direction)

(defun versor-extend-item-forwards (&optional level-offset)
  "Add another of the current unit to the end of the item.
With optional LEVEL-OFFSET, add that to the level first."
  (interactive)
  (versor-as-versor-command
    ;; must go outside versor-as-motion-command as that sets
    ;; versor-extension-direction, and we are about to preserve that
    ;; variable to protect it from versor-as-motion-command
    (unless versor-extension-direction
      (setq versor-extension-direction 'forwards))
    (let ((direction versor-extension-direction))
      (versor-as-motion-command item
	(let* ((start (versor-overlay-start item))
	       (end (versor-overlay-end item)))
	  (versor-next level-offset)
	  (if (eq direction 'forwards)
	      (versor-set-current-item start (versor-end-of-item-position))
	    (versor-set-current-item (point) end))))
      (setq versor-extension-direction direction))))

(defun versor-extend-item-backwards (&optional level-offset)
  "Add another of the current unit to the start of the item.
With optional LEVEL-OFFSET, add that to the level first."
  (interactive)
  (versor-as-versor-command
    ;; must go outside versor-as-motion-command as that sets
    ;; versor-extension-direction, and we are about to preserve that
    ;; variable to protect it from versor-as-motion-command
    (unless versor-extension-direction
      (setq versor-extension-direction 'backwards))
    (let ((direction versor-extension-direction))
      (versor-as-motion-command item
	(let* ((start (versor-overlay-start item))
	       (end (versor-overlay-end item)))
	  (versor-prev level-offset)
	  (if (eq direction 'backwards)
	      (versor-set-current-item (point) end)
	    (versor-set-current-item start (versor-end-of-item-position)))))
      (setq versor-extension-direction direction))))

(defun versor-extend-over-item-forwards ()
  "Add another of the unit above the current one, to the end of the item."
  (interactive)
  (versor-as-versor-command
    (versor-extend-item-forwards 1)))

(defun versor-extend-over-item-backwards ()
  "Add another of the unit above the current one, to the start of the item."
  (interactive)
  (versor-as-versor-command
    (versor-extend-item-backwards 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; kill-ring operations ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun versor-copy ()
  "Copy the current item."
  (interactive)
  (versor-as-motion-command current-item
   ;; (versor-display-item-list "versor-copy" (versor-get-current-items))
   (mapcar
    (lambda (item)
      (let* ((start (versor-overlay-start item))
	     (end (versor-overlay-end item))
	     (copy-string (buffer-substring start end)))
	(when (boundp 'label-with-adjacent-whitespace)
	  (label-with-adjacent-whitespace start end copy-string))
	(kill-new copy-string)))
    (reverse (versor-get-current-items)))
   (when (fboundp 'update-shown-stacks)	; from rpn-edit.el
     (update-shown-stacks))))

(defun versor-mark ()
  "Mark the current item.
If there are multiple parts to the current item (for example, opening
and closing brackets, when in the depth dimension), they go into
successive elements of the kill ring, for maximum compatibility with
normal Emacs commands. Our corresponding insertion commands understand
this."
  (interactive)
  (versor-as-motion-command current-item
   (let ((ready-made (versor-get-action 'mark)))
     (if ready-made
	 (versor-call-interactively ready-made)
       (let* ((item (versor-get-current-item)))
	 (let ((transient-mark-mode t))
	   (set-mark (cdr item))
	   (goto-char (car item))
	   (sit-for 1)))))))

(defun abbreviate-cut-string (cut-string)
  "Return a string based on CUT-STRING but more suitable for display in the minibuffer."
  (subst-char-in-string
   ?\t ? 
   (subst-char-in-string
    ?\n ?  
    (if (< (length cut-string) (frame-width))
	cut-string
      (let ((w (/ (- (frame-width) 13 ) 2)))
	(concat (substring cut-string 0 w)
		" ... "
		(substring cut-string (- w))))
      )) t))

(defun versor-kill ()
  "Kill the current item.
If there are multiple parts to the current item (for example, opening
and closing brackets, when in the depth dimension), they go into
successive elements of the kill ring, for maximum compatibility with
normal Emacs commands. Our corresponding insertion commands understand
this."
  (interactive)
  (versor-as-motion-command current-item
   (let ((ready-made (versor-get-action 'delete)))
     (if ready-made
	 (versor-call-interactively ready-made)
       (mapcar
	(lambda (item)
	  (let* ((start (versor-overlay-start item))
		 (end (versor-overlay-end item))
		 (cut-string (buffer-substring start end)))
	    (when (boundp 'label-with-adjacent-whitespace)
	      (label-with-adjacent-whitespace start end cut-string))
	    (message "Killing %s" (abbreviate-cut-string cut-string))
	    (kill-new cut-string)
	    (delete-region start end)
	    (versor-trim-whitespace start)))
	(versor-last-item-first))))
   (if (fboundp 'update-shown-stacks)	; from rpn-edit.el
       (update-shown-stacks))))

(defun versor-yank (&optional arg)
  "Versor wrapper for yank.
Does yank, then adjusts whitespace, versor-style."
  (interactive)
  (versor-as-versor-command
    (yank arg)
    (versor-trim-whitespace (region-end))
    (versor-trim-whitespace (region-beginning))))

(defun versor-transpose ()
  "Transpose this item with the following one."
  (interactive)
  (versor-as-motion-command current-item
   (let ((ready-made (versor-get-action 'transpose)))
     (if ready-made
	 (versor-call-interactively ready-made)
       (error "using undertested general implementation of versor-transpose")
       (let ((starting (point)))
	 (versor-end-of-item)
	 (let ((latter (buffer-substring starting (point))))
	   (delete-region starting (point))
	   (goto-char starting)
	   (versor-start-of-item)
	   (insert latter)))
       )
     )))

(defvar versor-isearch-string nil
  "A string that we pass to isearch, via versor-isearch-mode-hook-function")

(defun versor-isearch-mode-hook-function ()
  "A hook to pass a search string in to isearch"
  (when (stringp versor-isearch-string)
      (isearch-yank-string versor-isearch-string)))

(defun versor-search ()
  "Search for the next occurrence of the current item."
  (interactive)
  (versor-as-motion-command item
   (let* ((start (versor-overlay-start item))
	  (end (versor-overlay-end item))
	  (versor-isearch-string (buffer-substring start end)))
     (add-hook 'isearch-mode-hook 'versor-isearch-mode-hook-function)
     (let ((result (isearch-forward)))
       (when result
	 (goto-char (min result isearch-other-end)))))))

(defvar versor-insertion-kind-alist nil
  "Alist for reading what kind of insertion to do.
Almost a keymap, but the functions it contains are not commands.")

(defvar versor-insertion-kinds-menu (make-sparse-keymap "Insertion kinds")
  "Menu for kinds of insertions.")

(defun versor-definesert (key fun &optional menu-label)
  "Bind KEY to FUN in the map for choosing kinds of insertion.
This lets us have uniform insertion commands with various origins of
things to insert. We want to this because of being able to insert
multipart things, using the insert around command, which we want to
work both with brackets and with things previously killed -- for
example, the result of doing versor-kill in the depth dimension, which
can delete an opening and closing bracket together.
FUN should take one argument, the number of items to return.
It should return a list of that many items.
For insert-around, the first thing in the list is inserted before the selection,
and the second is inserted after it.
If a MENU-LABEL is given, add that to versor-insertions-menu."
  (if (or (stringp key) (vectorp key)) (setq key (aref key 0)))
  (let ((binding (assoc key versor-insertion-kind-alist)))
    (if binding
	(rplacd binding fun)
      (setq versor-insertion-kind-alist
	    (cons
	     (cons key fun)
	     versor-insertion-kind-alist))))
  (when menu-label
    (versor-add-menu-item versor-insertion-kinds-menu
			  menu-label
			  `(lambda ()
			    (interactive)
			    (funcall ,fun n)))))

(defun versor-top-n-kills (n &optional offset)
  "Return the top N entries in the kill ring.
With optional OFFSET, return the OFFSET...OFFSET+N entries instead."
  (if (null offset) (setq offset 0))
  (let ((result nil))
    (while (> n 0)
      (decf n)
      (push (current-kill (+ n offset) t) result))
    (nreverse result)))

(defun versor-top-n-searches (n)
  "Return the N most recent searches."
  (subseq search-ring 0 n))

(versor-definesert "\d" 'versor-top-n-kills "Latest kill")
(versor-definesert [ kp-delete ] 'versor-top-n-kills)
(versor-definesert [ delete ] 'versor-top-n-kills)
(versor-definesert [ DEL ] 'versor-top-n-kills)
(versor-definesert [ backspace ] 'versor-top-n-kills)
(versor-definesert "\C-y" 'versor-top-n-kills)
(versor-definesert "1" (lambda (n) "N kills, starting 1 back" (versor-top-n-kills n 1)) "N kills, starting 1 back")
(versor-definesert "2" (lambda (n) "N kills, starting 2 back" (versor-top-n-kills n 2)) "N kills, starting 2 back")
(versor-definesert "3" (lambda (n) "N kills, starting 3 back" (versor-top-n-kills n 3)) "N kills, starting 3 back")
(versor-definesert "4" (lambda (n) "N kills, starting 4 back" (versor-top-n-kills n 4)) "N kills, starting 4 back")
(versor-definesert "5" (lambda (n) "N kills, starting 5 back" (versor-top-n-kills n 5)) "N kills, starting 5 back")
(versor-definesert "6" (lambda (n) "N kills, starting 6 back" (versor-top-n-kills n 6)) "N kills, starting 6 back")
(versor-definesert "7" (lambda (n) "N kills, starting 7 back" (versor-top-n-kills n 7)) "N kills, starting 7 back")
(versor-definesert "8" (lambda (n) "N kills, starting 8 back" (versor-top-n-kills n 8)) "N kills, starting 8 back")
(versor-definesert "9" (lambda (n) "N kills, starting 9 back" (versor-top-n-kills n 9)) "N kills, starting 9 back")
(versor-definesert "f"
		   (lambda (n)
		     "Name of file in other window"
		     (list (save-window-excursion
			     (buffer-file-name
			      (window-buffer (other-window 1))))))
		   "Name of file in other window")
(versor-definesert "\C-s" 'versor-top-n-searches)
(versor-definesert "(" (lambda (n) "parentheses" (list "(" ")")) "()")
(versor-definesert "[" (lambda (n) "square brackets" (list "[" "]")) "[]")
(versor-definesert "{" (lambda (n) "braces" (list "{" "}")) "{}")
(versor-definesert "<" (lambda (n) "angle brackets" (list "<" ">")) "<>")
(versor-definesert "?" (lambda (n) "if-then statement in programming language of current mode"
			 (versor-statement-insertion-strings 'if-then)) "if-then")
(versor-definesert ":" (lambda (n) "if-then-else statement in programming language of current mode"
			 (versor-statement-insertion-strings 'if-then-else)) "if-then-else")
(versor-definesert "@" (lambda (n) "while statement in programming language of current mode"
			 (versor-statement-insertion-strings 'while-do)) "while-do")
(versor-definesert "&" (lambda (n) "AND operator in programming language of current mode"
			 (versor-statement-insertion-strings 'and)) "and")
(versor-definesert "|" (lambda (n) "OR operator in programming language of current mode"
			 (versor-statement-insertion-strings 'or)) "or")
(versor-definesert "!" (lambda (n) "NOT operator in programming language of current mode"
			 (versor-statement-insertion-strings 'not)) "not")
(versor-definesert "=" (lambda (n) "variable declaration in programming language of current mode"
			 (versor-statement-insertion-strings 'variable-declaration)) "variable declaration")

(defun versor-statement-insertion-strings (statement)
  "Return the insertion strings for STATEMENT in the current language."
  (let ((statement-descr (cdr (assoc statement
				     (get major-mode 'statements)))))
    (cdr (or (assoc (if versor-statement-insertion-with-dummy-value
			'begin-end-with-dummy
		      'begin-end)
		    statement-descr)
	     (if versor-statement-insertion-with-dummy-value 
		 (assoc 'begin-end statement-descr)
	       nil)))))

;; todo: we could also do some using something similar to tempo-insert-template (set tempo-region-stop etc first) to insert specific constructs?

;; now some fancy ones
(versor-definesert "s" 'languide-get-statement-insertion)
;; (versor-definesert "v" 'languide-get-variable-insertion)
;; (versor-definesert "f" 'languide-get-function-insertion)
;; (versor-definesert "t" 'versor-get-tag-insertion)

(defvar versor-insertion-using-menu nil
  "Whether the insertion is being done from a menu.")

(defun versor-get-insertable (n &optional prompt)
  "Return N things to insert, having asked the user for what kind of insertion this is.
This lets us do commands such as insert-around using a common framework."
  ;; should also be available for putting things into the search string --
  ;; in which case it ought to have a different name
  (if versor-insertion-using-menu
      (tmm-prompt versor-insertion-kinds-menu)
      (let* ((key (read-event (if prompt prompt "Kind of insertion: ")))
	     (command (assoc key versor-insertion-kind-alist)))
	(if (consp command)
	    (funcall (cdr command) n)
	  (error "%S in not bound to a kind of insertion" key)))))

(defun insert-active (insertion)
  "If INSERTION is a string, insert it; if a function, call it.
Various other dwim things, too -- evaluate evaluable forms, etc."
  (cond
   ((stringp insertion) (insert insertion))
   ((functionp insertion) (funcall insertion))
   ((symbolp insertion)			; but not a function
    (insert (symbol-name insertion)))
   ((and (consp insertion)
	 (functionp (car insertion)))
    (eval insertion))
   ((consp insertion)			; but not an evaluable form
    (mapcar 'insert-active insertion))
   ((integerp insertion) (insert insertion))))

(defun versor-insert-before ()
  "Insert something before the current versor item."
  (interactive)
  (versor-as-motion-command current-item
   (let* ((new-thing (versor-get-insertable 1)))
     (goto-char (car current-item))
     (mapcar 'insert-active new-thing))))

(defun versor-insert-after ()
  "Insert something after the current versor item."
  (interactive)
  (versor-as-motion-command current-item
   (let* ((new-thing (versor-get-insertable 1)))
     (goto-char (cdr current-item))
     (mapcar 'insert-active new-thing))))

(defun versor-insert-around ()
  "Insert something around the current versor item."
  (interactive)
  (versor-as-motion-command current-item
   (let ((new-thing (versor-get-insertable 2 "Type of insertion around item: ")))
     (message "insert-around: current-item=%S new-thing=%S" current-item new-thing)
     ;; todo: clever interface to skeletons and tempo templates
     (cond
      ((eq (car new-thing) 'template)
       ;; insert a tempo template
       ;; prepare region from versor data
       ;; (tempo-insert-template (cdr new-thing) t)
       (error "insert-around from template not yet implemented")
       )
      ((eq (car new-thing) 'skeleton)
       ;; insert a skeleton
       ;; prepare interregions from versor data
       ;; (skeleton-insert (cdr new-thing) regions str)
       (error "insert-around from skeleton not yet implemented")
       )
      (t
       (goto-char (versor-overlay-end current-item))
       (insert (second new-thing))
       (let ((end (make-marker))
	     (start (versor-overlay-start current-item)))
	 (set-marker end (point))
	 (goto-char start)
	 (insert (first new-thing))
	 (versor-set-current-item start end)))))))

(defun versor-insert-within ()
  "Insert something within the current versor item."
  (interactive)
  (versor-as-motion-command current-item
   (let* ((new-thing (versor-get-insertable 1)))
     ;; not yet sure what this really means
     )))

(defun versor-command-p (command)
  "Return whether COMMAND is part of versor."
  (string-match "^versor-" (symbol-name command)))

(defun languide-command-p (command)
  "Return whether COMMAND is part of languide."
  (string-match "^languide-" (symbol-name command)))

(defun versor-languide-command-p (command)
  "Return whether COMMAND is part of the versor wrapper for languide."
  (string-match "^versor-languide-" (symbol-name command)))

;;;; end of versor-commands.el
