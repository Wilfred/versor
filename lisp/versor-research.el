;;;; versor-research.el -- Count use and non-use of versor
;;; Time-stamp: <2006-03-25 14:02:29 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(provide 'versor-research)

(defvar versor-commands-obarray (make-vector 1511 nil)
  "Obarray naming all the commands in versor.")

(mapc (function (lambda (symbol) (intern (symbol-name symbol) versor-commands-obarray)))
      '(versor-out versor-in versor-next-meta-level
		   versor-prev-meta-level versor-reverse versor-start versor-prev
		   versor-next versor-end versor-over-start versor-over-prev
		   versor-over-next versor-over-end versor-over-over-start
		   versor-over-over-prev versor-over-over-next versor-over-over-end
		   versor-end-of-item versor-start-of-item versor-other-end-of-item
		   versor-extend-item-forwards versor-extend-item-backwards
		   versor-extend-over-item-forwards versor-extend-over-item-backwards
		   versor-copy versor-mark versor-kill versor-transpose versor-search
		   versor-insert-before versor-insert-after versor-insert-around
		   versor-insert-within
		   versor-dwim))

(defun versor-command-p (command)
  "Return whether COMMAND is part of versor."
  (intern-soft (symbol-name command) versor-commands-obarray))

(defvar versor-research-buffer-before-command nil
  "The buffer we were in before the current command.")

(defvar versor-research-change-happened nil
  "Whether anything changed during this command.")

(defvar versor-research-previous-command-type nil
  "The type of command before the current one.")

(defvar versor-research-previous-previous-command-type nil
  "The type of command before the one before the current one.")

(defvar versor-research-same-type-command-chain nil
  "Details of the latest series of commands of the same type.")

(defvar versor-research-previous-same-type-command-chain nil
  "Details of the previous series of commands of the same type.")

(defun versor-research-pre-command-function ()
  "Set things up to see what happens during the coming command."
  ;; (message "Resetting research vars")
  (setq versor-research-buffer-before-command (current-buffer)
	versor-research-change-happened nil))

(defvar versor-research-versor-edit-after-versor-moves nil
  "Each entry is the list of versor movement commands leading directly
to a versor edit.")

(defvar versor-research-non-versor-edit-after-versor-moves nil
  "Each entry is the list of versor movement commands leading directly to a non-versor edit.")

(defvar versor-research-non-versor-edit-after-versor-moves nil
  "Each entry is the list of non-versor movement commands leading
directly to a non-versor edit.")

(defvar versor-research-versor-edit-after-versor-moves-with-manual-adjustment nil
  "Each entry is a cons of the list of versor movement commands
leading indirectly to a versor edit, and the list of non-versor
movements that came between them.")

(defvar versor-research-versor-edit-after-non-versor-moves nil
  "Each entry is the list of non-versor movement commands leading
directly to a versor edit.")

(defvar versor-research-non-versor-edit-after-versor-moves-with-manual-adjustment nil
  "Each entry is a cons of the list of versor movement commands
leading indirectly to a non-versor edit, and the list of non-versor
movements that came between them.")

(defvar versor-research-non-versor-edit-after-non-versor-moves nil
    "Each entry is the list of non-versor movement commands leading
directly to a non-versor edit.")

(defvar versor-research-live-commentary nil
  "*Whether to make a live commentary on versor and other activities.")

(defun versor-research-post-command-function ()
  "Record what happened during this command."
  (when (and (eq (current-buffer) versor-research-buffer-before-command)
	     (not (string-match "^\\*Minibuf" (buffer-name))))
    ;; we are not interested in buffer-switching commands, as they
    ;; are not versor commands
    (condition-case error-var
	(let* ((commentary (and versor-research-live-commentary
				(not (string-match "inibu" (buffer-name)))))
	       (command-was-versor (versor-command-p this-command))
	       (command-type (if command-was-versor
				 (if versor-research-change-happened
				     (if (zerop (third versor-research-change-happened))
					 'versor-insertion
				       'versor-deletion)
				   'versor-move)
			       (if versor-research-change-happened
				   (if (zerop (third versor-research-change-happened))
				       'non-versor-insertion
				     'non-versor-deletion)
				 'non-versor-move))))
	  (when commentary
	    (message "Command was %S (previous was %S)" command-type versor-research-previous-command-type))
	  (if (eq command-type versor-research-previous-command-type)
	      (push (cons this-command versor-research-change-happened)
		    versor-research-same-type-command-chain)
	    ;; here is the interesting bit -- particularly the
	    ;; transitions between versor and non-versor commands
	    (cond
	     ((memq command-type '(versor-insertion versor-deletion))
	      (cond
	       ((eq versor-research-previous-command-type 'versor-move)
		;; this is the one we like -- a versor command after versor moves
		(when commentary (message "versor edit after versor moves"))
		(push versor-research-same-type-command-chain
		      versor-research-versor-edit-after-versor-moves)
		)
	       ((eq versor-research-previous-command-type 'non-versor-move)
		;; versor command after non-versor move
		(if (eq versor-research-previous-previous-command-type 'versor-move)
		    (progn
		      ;; non-versor move may have been an adjustment after a versor move
		      (when commentary (message "versor edit after versor moves with manual adjustment"))
		      (push (cons versor-research-same-type-command-chain
				  versor-research-previous-same-type-command-chain)
			    versor-research-versor-edit-after-versor-moves-with-manual-adjustment)

		      )
		  (when commentary (message "versor edit after non versor moves"))
		  (push versor-research-same-type-command-chain
			versor-research-versor-edit-after-non-versor-moves)

		  ))))
	     ((memq command-type '(non-versor-deletion non-versor-insertion))
	      (cond
	       ((eq versor-research-previous-command-type 'non-versor-move)
		;; old-fashioned non-versor emacs editing, but did they use versor first then adjust?
		(if (eq versor-research-previous-previous-command-type 'versor-move) 
		    (progn
		      (when commentary (message "non versor edit after versor moves with manual adjustment"))
		      (push (cons versor-research-same-type-command-chain
				  versor-research-previous-same-type-command-chain)
			    versor-research-non-versor-edit-after-versor-moves-with-manual-adjustment))
		  (when commentary (message "non versor edit after non versor moves"))
		  (push versor-research-same-type-command-chain
			versor-research-non-versor-edit-after-non-versor-moves)))
	       ((eq versor-research-previous-command-type 'versor-move)
		;; using versor to navigate but not to edit
		(when commentary (message "non versor edit after versor moves"))
		(push versor-research-same-type-command-chain
		      versor-research-non-versor-edit-after-versor-moves))
	       )))
	    (setq versor-research-previous-previous-command-type versor-research-previous-command-type
		  versor-research-previous-command-type command-type
		  versor-research-same-type-command-chain nil)))
      (error (message "error %s in  versor-research-post-command-function" error-var)))))

;; I don't think we really need this one
;; (defun versor-research-before-change-function (from to)  )

(defun versor-research-after-change-function (from to old-length)
  "Part of looking at what happened during a command"
  (when (eq (current-buffer) versor-research-buffer-before-command)
    (setq versor-research-change-happened (list from to old-length)))
  ;; (message "Change %S in %S" versor-research-change-happened (current-buffer))
  )

(defun versor-research-start ()
  "Start recording lengths of sequences of consecutive commands of different kinds."
  (interactive)
  (add-hook 'pre-command-hook 'versor-research-pre-command-function)
  (add-hook 'post-command-hook 'versor-research-post-command-function)
  ;; (add-hook 'before-change-functions 'versor-research-before-change-function)
  (add-hook 'after-change-functions 'versor-research-after-change-function))

(defun versor-research-stop ()
  (interactive)
  (remove-hook 'pre-command-hook 'versor-research-pre-command-function)
  (remove-hook 'post-command-hook 'versor-research-post-command-function)
  ;; (remove-hook 'before-change-functions 'versor-research-before-change-function)
  (remove-hook 'after-change-functions 'versor-research-after-change-function))

(defun versor-research-reset ()
  "Reset the versor research data. You probably don't want to do this."
  (interactive)				; do I really want this?
  (setq versor-research-versor-edit-after-versor-moves nil
	versor-research-versor-edit-after-versor-moves-with-manual-adjustment nil
	versor-research-versor-edit-after-non-versor-moves nil
	versor-research-non-versor-edit-after-versor-moves nil
	versor-research-non-versor-edit-after-non-versor-moves nil
	versor-research-non-versor-edit-after-versor-moves-with-manual-adjustment nil))

(defun hook-memq (fn var)
  "Check whether FN is part of VAR, allowing for the global hook mechanism."
  (or (memq fn (symbol-value var))
      (and (memq t (symbol-value var))
	   (memq fn (default-value var)))))

(defun versor-research-report ()
  (interactive)
  (with-output-to-temp-buffer "*Versor research report*"
    (princ (format "Versor edits after versor moves: %d\n"
		   (length versor-research-versor-edit-after-versor-moves)))
    (princ (format "Versor edits after versor moves with adjustments: %d\n"
		   (length versor-research-versor-edit-after-versor-moves-with-manual-adjustment)))
    (princ (format "Versor edits after non-versor moves: %d\n"
		   (length versor-research-versor-edit-after-non-versor-moves)))
    (princ (format "Non-versor edits after versor moves: %d\n"
		   (length versor-research-non-versor-edit-after-versor-moves)))
    (princ (format "Non-versor edits after non-versor moves: %d\n"
		   (length versor-research-non-versor-edit-after-non-versor-moves)))
    (princ (format "Non-versor edits after versor moves with adjustments: %d\n"
		   (length versor-research-non-versor-edit-after-versor-moves-with-manual-adjustment)))
    (let ((pre-command-ok (hook-memq 'versor-research-pre-command-function 'pre-command-hook))
	  (post-command-ok (hook-memq 'versor-research-post-command-function 'post-command-hook))
	  (after-change-ok (hook-memq 'versor-research-after-change-function 'after-change-functions)))
      (if (and pre-command-ok post-command-ok after-change-ok)
	  (princ "All hooks still in place\n")
	(unless pre-command-ok (princ "Pre-command hook missing\n"))
	(unless post-command-ok (princ "Post-command hook missing\n"))
	(unless after-change-ok (princ "After-change function missing\n"))))))

;;; end of versor-research.el
