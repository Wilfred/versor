;;; versor-commands.el -- versatile cursor commands
;;; Time-stamp: <2004-05-24 08:55:33 john>
;;
;; emacs-versor -- versatile cursors for GNUemacs
;;
;; Copyright (C) 2004  John C. G. Sturdy
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

(require 'versor)
(provide 'versor-commands)

;;;;;;;;;;;;;;;;;;;
;;;; reversing ;;;;
;;;;;;;;;;;;;;;;;;;

(defvar versor:reversed nil
  "Whether we have currently reversed the navigation directions.
This is useful if you cannot use \"shift-next\" for \"previous\".
It is enabled by the variable versor:reversible, which see.")

(defvar versor:mode-line-begin-string " <"
  "String corresponding to bracket for beginning of versor markers according to versor:reversed.")
(defvar versor:mode-line-end-string (if versor:reversible "==>" ">")
  "String corresponding to bracket for end of versor markers according to versor:reversed.")

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; more structure ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro as-versor-motion-command (&rest body)
  "Run BODY as a versor motion command.
Necessary pre- and post-processing get done."
  `(progn
     (versor-clear-current-item-indication)
     (progn
       ,@body)
     ;; the few commands that want to do otherwise, must re-set this
     ;; one just after using this macro
     (setq versor-extension-direction nil)
     (versor-indicate-current-item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; commands begin here ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; meta-dimension commands

(defun versor:out ()
  "Move versor:level out a dimension in (versor:current-meta-level)."
  (interactive)
  (as-versor-motion-command
   (setq versor:old-level versor:level)
   (if (and (interactive-p)
	    versor:reversed)
       (decf versor:level)
     (incf versor:level))
   (versor::trim-level)
   (versor:set-status-display
    (first (versor:current-level))
    (mapcar 'first (versor:level-names))
    t)))

(defun versor:in ()
  "Move versor:level in a dimension in (versor:current-meta-level)."
  (interactive)
  (as-versor-motion-command
   (setq versor:old-level versor:level)
   (if (and (interactive-p)
	    versor:reversed)
       (incf versor:level)
     (decf versor:level))
   (versor::trim-level)
   (versor:set-status-display
    (first (versor:current-level))
    (mapcar 'first (versor:level-names))
    t)))

(defun versor:next-meta-level ()
  "Move to the next meta-level."
  (interactive)
  (as-versor-motion-command
   (if (and (interactive-p)
	    versor:reversed)
       (decf versor:meta-level)
     (incf versor:meta-level))
   (versor::trim-meta-level)
   (versor::trim-level)
   (versor:set-status-display
     (aref (versor:current-meta-level) 0)
     (mapcar 'first (versor:meta-level-names))
     t)))

(defun versor:prev-meta-level ()
  "Move to the previous meta-level."
  (interactive)
  (as-versor-motion-command
   (if (and (interactive-p)
	    versor:reversed)
       (incf versor:meta-level)
     (decf versor:meta-level))
   (versor::trim-meta-level)
   (versor::trim-level)
   (versor:set-status-display
     (aref (versor:current-meta-level) 0)
     (mapcar 'first (versor:meta-level-names))
     t)))

;;;; commands within the current dimension(s)

(defun versor:reverse ()
  "Reverse the forward and backward directions."
  (interactive)
  (setq versor:reversed (not versor:reversed))
  (versor:set-status-display))

(defun versor:get-action (action &optional level-offset)
  "Get ACTION for the current versor level, taking modal bindings into account.
With optional LEVEL-OFFSET, add that to the level first.
Modal bindings are made by versor:bind-modal, which is in versor-modal.el"
  (let* ((level-data (versor:current-level level-offset))
	 (level-modal-data (cdr (assoc major-mode (versor:action level-data 'modal-bindings))))
	 (modal-binding (if level-modal-data (versor:action level-modal-data action) nil)))
    (if modal-binding
	modal-binding
      (versor:action level-data action))))

(defvar versor:point-before-move nil
  "The value of point before the latest versor move.")

(defun versor:start (&optional level-offset)
  "Move backward to the start of the current dimension.
With optional LEVEL-OFFSET, add that to the level first.
If repeated, this undoes the first move, and goes back a meta-dimension."
  (interactive)
  (as-versor-motion-command
   (unless (run-hook-with-args-until-success
	    (if versor:reversed
		'versor:end-hooks
	      'versor:start-hooks))
     (if (eq last-command this-command)
	 (progn
	   (versor:prev-meta-level)
	   (goto-char versor:point-before-move))
       (progn
	 (setq versor:point-before-move (point))
	 (call-interactively
	  (versor:get-action (if versor:reversed 'last 'first) level-offset)))))))

(defun versor:prev-action (&optional level-offset)
  "Internal action code to move backward within the current dimension.
With optional LEVEL-OFFSET, add that to the level first."
  (unless (run-hook-with-args-until-success 'versor:prev-hooks)
    (setq versor:point-before-move (point))
    (call-interactively (versor:get-action 'previous level-offset))))

(defun versor:next-action (&optional level-offset)
  "Internal action code to move forward within the current dimension.
With optional LEVEL-OFFSET, add that to the level first."
  (unless (run-hook-with-args-until-success 'versor:next-hooks)
    (setq versor:point-before-move (point))
    (call-interactively (versor:get-action 'next level-offset))))

(defun versor:prev (&optional level-offset)
  "Move backward within the current dimension.
With optional LEVEL-OFFSET, add that to the level first."
  (interactive '(nil))
  (as-versor-motion-command
   (if versor:reversed
       (versor:next-action level-offset)
     (versor:prev-action level-offset))))

(defun versor:next (&optional level-offset)
  "Move forward within the current dimension.
With optional LEVEL-OFFSET, add that to the level first."
  (interactive '(nil))
  (as-versor-motion-command
   (if versor:reversed
       (versor:prev-action level-offset)
     (versor:next-action level-offset))))

(defun versor:end (&optional level-offset)
  "Move forward to the end of the current dimension.
With optional LEVEL-OFFSET, add that to the level first.
If repeated, this undoes the first move, and goes back a meta-dimension."
  (interactive)
  (as-versor-motion-command
   (unless (run-hook-with-args-until-success
	    (if versor:reversed
		'versor:start-hooks
	      'versor:end-hooks))
     (if (eq last-command this-command)
	 (progn
	   (versor:next-meta-level)
	   (goto-char versor:point-before-move))
       (progn
	 (setq versor:point-before-move (point))
	 (call-interactively
	  (versor:get-action (if versor:reversed 'first 'last) level-offset)))))))

(defun versor:over-start ()
  "Move backward to the start of the dimension above the current one.
If repeated, this undoes the first move, and goes back a meta-dimension."
  (interactive)
  (versor:start 1))

(defun versor:over-prev ()
  "Move backward within the dimension above the current one."
  (interactive)
  (versor:prev 1))

(defun versor:over-next ()
  "Move forward within the dimension above the current one."
  (interactive)
  (versor:next 1))

(defun versor:over-end ()
  "Move forward to the end of the dimension above the current one.
If repeated, this undoes the first move, and goes back a meta-dimension."
  (interactive)
  (versor:end 1))

(defun versor:over-over-start ()
  "Move backward to the start of the dimension two above the current one.
If repeated, this undoes the first move, and goes back a meta-dimension."
  (interactive)
  (versor:start 2))

(defun versor:over-over-prev ()
  "Move backward within the dimension two above the current one."
  (interactive)
  (versor:prev 2))

(defun versor:over-over-next ()
  "Move forward within the dimension two above the current one."
  (interactive)
  (versor:next 2))

(defun versor:over-over-end ()
  "Move forward to the end of the dimension two above the current one.
If repeated, this undoes the first move, and goes back a meta-dimension."
  (interactive)
  (versor:end 2))

(defun versor-end-of-item-position ()
  "Return the end of the current item."
  (let ((result
	 (let ((mover (or (versor:get-action 'end-of-item)
			  (versor:get-action 'next))))
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
  (let* ((items versor:latest-items)
	 (item (car items)))
    (as-versor-motion-command
     (goto-char
      (if (versor:current-item-valid)
	  (cdr (versor:get-current-item))
	(versor-end-of-item-position)))
     (make-versor-overlay (car item) (cdr item)))))

(defun versor-start-of-item ()
  "Move to the start of the current item."
  (interactive)
  (as-versor-motion-command
   (if (versor:current-item-valid)
       (goto-char (car (versor:get-current-item)))
     (versor:prev-action 1))))

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
  ;; must go outside as-versor-motion-command as that sets
  ;; versor-extension-direction, and we are about to preserve that
  ;; variable to protect it from as-versor-motion-command
  (unless versor-extension-direction
    (setq versor-extension-direction 'forwards))
  (let ((direction versor-extension-direction))
    (as-versor-motion-command
     (let* ((item (versor:get-current-item))
	    (start (versor-overlay-start item))
	    (end (versor-overlay-end item)))
       (versor:next level-offset)
       (if (eq direction 'forwards)
	   (versor:set-current-item start (versor-end-of-item-position))
	 (versor:set-current-item (point) end))))
    (setq versor-extension-direction direction)))

(defun versor-extend-item-backwards (&optional level-offset)
  "Add another of the current unit to the start of the item.
With optional LEVEL-OFFSET, add that to the level first."
  (interactive)
  ;; must go outside as-versor-motion-command as that sets
  ;; versor-extension-direction, and we are about to preserve that
  ;; variable to protect it from as-versor-motion-command
  (unless versor-extension-direction
    (setq versor-extension-direction 'backwards))
  (let ((direction versor-extension-direction))
    (as-versor-motion-command
     (let* ((item (versor:get-current-item))
	    (start (versor-overlay-start item))
	    (end (versor-overlay-end item)))
       (versor:prev level-offset)
       (if (eq direction 'backwards)
	   (versor:set-current-item (point) end)
	 (versor:set-current-item start (versor-end-of-item-position)))))
    (setq versor-extension-direction direction)))

(defun versor-extend-over-item-forwards ()
  "Add another of the unit above the current one, to the end of the item."
  (interactive)
  (versor-extend-item-forwards 1))

(defun versor-extend-over-item-backwards ()
  "Add another of the unit above the current one, to the start of the item."
  (interactive)
  (versor-extend-item-backwards 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; kill-ring operations ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun versor:copy ()
  "Copy the current item."
  (interactive)
  (as-versor-motion-command
   ;; (versor:display-item-list "versor:copy" (versor:get-current-items))
   (mapcar
    (lambda (item)
      (kill-new (buffer-substring
		 (versor-overlay-start item)
		 (versor-overlay-end item))))
    (reverse (versor:get-current-items)))))

(defun versor:mark ()
  "Mark the current item.
If there are multiple parts to the current item (for example, opening
and closing brackets, when in the depth dimension), they go into
successive elements of the kill ring, for maximum compatibility with
normal Emacs commands. Our corresponding insertion commands understand
this."
  (interactive)
  (as-versor-motion-command
   (let ((ready-made (versor:get-action 'mark)))
     (if ready-made
	 (call-interactively ready-made)
       (let* ((item (versor:get-current-item)))
	 (let ((transient-mark-mode t))
	   (set-mark (cdr item))
	   (goto-char (car item))
	   (sit-for 1)))))))

(defun versor:kill ()
  "Kill the current item.
If there are multiple parts to the current item (for example, opening
and closing brackets, when in the depth dimension), they go into
successive elements of the kill ring, for maximum compatibility with
normal Emacs commands. Our corresponding insertion commands understand
this."
  (interactive)
  (as-versor-motion-command
   (let ((ready-made (versor:get-action 'delete)))
     (if ready-made
	 (call-interactively ready-made)
       (mapcar
	(lambda (item)
	  (let ((start (versor-overlay-start item))
		(end (versor-overlay-end item)))
	    (kill-new (buffer-substring start end))
	    (delete-region start end)))
	(reverse (versor:get-current-items)))))))

(defun versor:definesert (key fun)
  "Bind KEY to FUN in the map for choosing kinds of insertion.
This lets us have uniform insertion commands with various origins of
things to insert. We want to this because of being able to insert
multipart things, using the insert around command, which we want to
work both with brackets and with things previously killed -- for
example, the result of doing versor:kill in the depth dimension, which
can delete an opening and closing bracket together."
  (if (or (stringp key) (vectorp key)) (setq key (aref key 0)))
  (let ((binding (assoc key versor:insertion-kind-alist)))
    (if binding
	(rplacd binding fun)
      (setq versor:insertion-kind-alist
	    (cons
	     (cons key fun)
	     versor:insertion-kind-alist)))))

(defun versor:top-n-kills (n)
  "Return the top N entries in the kill ring."
  (let ((result nil))
    (while (> n 0)
      (decf n)
      (push (current-kill n t) result))
    result))

(versor:definesert "\d" 'versor:top-n-kills)
(versor:definesert [ kp-delete ] 'versor:top-n-kills)
(versor:definesert [ del ] 'versor:top-n-kills)
(versor:definesert "\C-y" 'versor:top-n-kills)
(versor:definesert "(" (lambda (n) (list "(" ")")))
(versor:definesert "[" (lambda (n) (list "[" "]")))
(versor:definesert "{" (lambda (n) (list "{" "}")))
(versor:definesert "<" (lambda (n) (list "<" ">")))

(defun versor:get-insertable (n &optional prompt)
  "Return N things to insert, having asked the user for what kind of insertion this is.
This lets us do commands such as insert-around using a common framework."
  ;; should also be available for putting things into the search string --
  ;; in which case it ought to have a different name
  (let* ((key (read-char (if prompt prompt "Kind of insertion: ")))
	 (command (assoc key versor:insertion-kind-alist)))
    (if (consp command)
	(funcall (cdr command) n)
      (error "Not a valid kind of insertion"))))

(defun versor:insert-before ()
  "Insert something before the current versor item."
  (interactive)
  (as-versor-motion-command
   (let* ((new-thing (versor:get-insertable 1))
	  (current-item (versor:get-current-item)))
     (goto-char (car current-item))
     (insert new-thing))))

(defun versor:insert-after ()
  "Insert something after the current versor item."
  (interactive)
  (as-versor-motion-command
   (let* ((new-thing (versor:get-insertable 1))
	  (current-item (versor:get-current-item)))
     (goto-char (cdr current-item))
     (insert new-thing))))

(defun versor:insert-around ()
  "Insert something around to the current versor item."
  (interactive)
  (as-versor-motion-command
   (let ((current-item (versor:get-current-item))
	 (new-thing (versor:get-insertable 2 "Type of insertion around item: ")))
     (message "insert-around: current-item=%S" current-item)
     (goto-char (versor-overlay-end current-item))
     (insert (second new-thing))
     (goto-char (versor-overlay-start current-item))
     (insert (first new-thing)))))

(defun versor:insert-within ()
  "Insert something within the current versor item."
  (interactive)
  (as-versor-motion-command
   (let* ((new-thing (versor:get-insertable 1))
	  (current-item (versor:get-current-item)))
     ;; not yet sure what this really means
     )))

;;;; end of versor-commands.el
