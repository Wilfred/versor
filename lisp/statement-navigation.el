;;;; statement-navigation.el -- Statement-based navigation for languide and versor
;;; Time-stamp: <2006-03-09 14:52:36 john>

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

(provide 'statement-navigation)
(require 'cl)
(require 'modal-functions)
(require 'versor-commands) ; for versor-as-motion-command
(require 'statement-cache)
(require 'statement-parts)

;;;; models for modals

(defmodel beginning-of-statement-internal ()
  "Move to the beginning of the statement.
Do not do auxiliary stuff that might be associated with this."
  (interactive))

(defmodel end-of-statement-internal ()
  "Move to the end of the current statement.
Do not do auxiliary stuff that might be associated with this."
  (interactive))

(defmodel move-into-previous-statement ()
  "Move into the previous statement.
This need be valid only after a beginning-of-statement-internal.
It should move point back such that another beginning-of-statement-internal
will go back another statement."
  (interactive))

(defmodel move-into-next-statement ()
  "Move into the next statement.
This need be valid only after an end-of-statement-internal.
It should move point forward such that another end-of-statement-internal
will go forward another statement."
  (interactive))

(defmodel identify-statement (default)
  "Note what kind of statement we are at the start of.
Need not work if not at the start of a statement.
If the statement cannot be identified, return DEFAULT.")

(defun establish-current-statement (command &optional as-far-as)
  "Record some things about the statement at point."
  ;; After each movement that may take us into a different statement,
  ;; we update some state variables that tell us about the current
  ;; statement
  (setq languide-last-statement-selector-command command)
  (let ((type (identify-statement nil)))
    (statement-set-type type)
    (statement-remember (point) type as-far-as)
    (setq statement-latest-start (point)
	  navigated-latest-part 'whole)
    type))

(defun beginning-of-statement ()
  "Move to the beginning of the statement.
Argument 1 means the current statement, 2 the next, etc."
  (interactive)
  (beginning-of-statement-internal)
  (establish-current-statement 'beginning-of-statement))

;; this is for testing
;; (global-set-key [ kp-subtract ] 'beginning-of-statement-internal)

(defun end-of-statement ()
  "Move to the end of the current statement.
Argument 1 means the current statement, 2 the next, etc."
  (interactive)
  (beginning-of-statement-internal)
  (establish-current-statement 'end-of-statement)
  (end-of-statement-internal))

(defun previous-statement-internal (n)
  "Move to the NTH previous statement.
If calling this from a program, other than inside statement navigation,
you should possibly use next-statement instead.
There an element of DWIM to this:
  If not at the beginning of the statement, move to the beginning of it.
  If already at the beginning, move to the beginning of the previous one.
Then, if N is greater than 1, move back N-1 more statements."
  (let ((starting-point (point))
	(previous-end nil))
    (languide:debug-message 'previous-statement "Going back %d statements" n)
    (beginning-of-statement-internal)
    (when (= (point) starting-point) ; this means we were already at the start, so go back another
      (languide:debug-message 'previous-statement "Was already at start, going back an extra one")
      (incf n))
    (while (> n 1)
      (languide:debug-message 'previous-statement "  %d statements left to go back over" n)
      (setq previous-end (point))
      (languide:debug-message 'previous-statement "  moving into previous statement")
      (move-into-previous-statement)
      (languide:debug-message 'previous-statement "  that gets us to %d" (point))
      (beginning-of-statement-internal)
      (languide:debug-message 'previous-statement "Now gone back another statement, to %d" (point))
      (decf n))
    (languide:debug-message 'previous-statement "final move to beginning of statement")
    (languide:debug-message 'previous-statement "that gets us to %d" (point))
    (let ((end (if (and versor-statement-up-to-next previous-end)
		   previous-end
		 (save-excursion
		   (let ((start (point)))
		     (condition-case nil
			 (end-of-statement-internal)
		       (error (message "Could not find end of statement")))
		     (point))))))
      end)))

(defun previous-statement (n)
  "Move to the NTH previous statement.
There an element of DWIM to this:
  If not at the beginning of the statement, move to the beginning of it.
  If already at the beginning, move to the beginning of the previous one.
Then, if N is greater than 1, move back N-1 more statements."
  (interactive "p")
  (versor-as-motion-command
   (let ((end (previous-statement-internal n)))
     (versor-set-current-item (point) end)
     (establish-current-statement 'previous-statement end))))

(defun next-statement-internal (n)
  "Move to the NTH next statement.
If calling this from a program, other than inside statement navigation,
you should possibly use next-statement instead."
  (languide:debug-message 'next-statement-internal "next-statement(%d) starting from %d" n (point))
  (let ((first t)
	(starting (point)))
    (while (> n 0)
      ;; todo: this does the seemingly wrong thing if called before the start of the first statement -- it ends up taking us to the second statement
      (languide:debug-message 'next-statement "at %d (\"%s\"); %d statements left; going to end of current statement" (point) (buffer-substring-no-properties (point) (+ 24 (point))) n)
      (end-of-statement-internal)
      (languide:debug-message 'next-statement "at end of current statement, %d" (point))
      ;; not sure what the point of this next bit was... seems to do better without it!
      (when nil first ; go back to see if we were before the statement
	    (let ((first-end (point)))
	      (languide:debug-message 'next-statement "Going back to find start of statement where next-statement was called")
	      (beginning-of-statement-internal)
	      (if (> (point) starting)
		  ;; if the beginning of the first statement is past
		  ;; where we started, i.e. we started before the
		  ;; statement, then no more iterations
		  (progn
		    (languide:debug-message 'next-statement "we started before the statement")
		    (setq n 0))
		(goto-char first-end))))
      (languide:debug-message 'next-statement "at %d; skipping to code at start of next statement" (point))
      (skip-to-actual-code)
      (languide:debug-message 'next-statement "that gets us to %d" (point))
      (when (> n 1)
	(languide:debug-message 'next-statement "moving into next statement")
	(move-into-next-statement)
	(languide:debug-message 'next-statement "that gets us to %d" (point)))
      (decf n)
      (setq first nil)))
  ;; move over any comment
  (skip-to-actual-code))

(defun next-statement (n)
  "Move to the NTH next statement, and set up the statement variables."
  (interactive "p")
  (versor-as-motion-command
   (next-statement-internal n)
   (languide:debug-message 'next-statement
			   "Finding both ends of the new current statement, currently at start=%d"
			   (point))
   (let* ((start (point))
	  (end (save-excursion (end-of-statement-internal) (point))))
     (languide:debug-message 'next-statement "Going to end gets us %d..%d" start end)
     (versor-set-current-item start end)
     (establish-current-statement 'next-statement end))))

(defvar statement-navigation-type 'comment
  "The current type of statement to navigate to parts of.
Initialized to 'comment as pretty well every language should have a definition of this.")

(defvar statement-navigation-type-string "comment"
  "The name of the current type of statement to navigate to parts of.
Initialized to \"comment\" as pretty well every language should have a definition of this.")

(defun statement-set-type (type)
  "Set the statement type to TYPE.
If TYPE is nil, do nothing (might change this to say \"unknown\").
This also updates the mode line display of it."
  (interactive
   (list
    (choose-in-steps "Statement type: "
		     (mapcar
		      (function
		       (lambda (statement)
			 (symbol-name (car statement))))
		      (get major-mode 'statements)))))
  (message "%S statement" type)
  (if type
      (progn
	(setq statement-navigation-type (if (symbolp type) type (intern type))
	      statement-navigation-type-string (if (symbolp type) (symbol-name statement-navigation-type) type))
	(force-mode-line-update t)
	t)
    (progn
      (message "No type given")
      (setq statement-navigation-type 'unknown
	    statement-navigation-type-string "unknown")
      (force-mode-line-update t)
      nil)))

(defun statement-type-at-point ()
  "Return the type of statement at point."
  (save-excursion
    (beginning-of-statement 1)
    (identify-statement nil)))

;;; end of statement-navigation.el
