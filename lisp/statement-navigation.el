;;;; statement-navigation.el -- Statement-based navigation for languide and versor
;;; Time-stamp: <2005-08-12 10:16:09 jcgs>

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
(require 'versor-commands) ; for versor:as-motion-command

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
	  navigated-latest-part 'whole)))

(defun beginning-of-statement ()
  "Move to the beginning of the statement.
Argument 1 means the current statement, 2 the next, etc."
  (interactive)
  (beginning-of-statement-internal)
  (establish-current-statement 'beginning-of-statement))

;; this is for testing
(global-set-key [ kp-subtract ] 'beginning-of-statement-internal)

(defun end-of-statement ()
  "Move to the end of the current statement.
Argument 1 means the current statement, 2 the next, etc."
  (interactive)
  (beginning-of-statement-internal)
  (establish-current-statement 'end-of-statement)
  (end-of-statement-internal))

(defun previous-statement (n)
  "Move to the NTH previous statement.
There an element of DWIM to this:
  If not at the beginning of the statement, move to the beginning of it.
  If already at the beginning, move to the beginning of the previous one.
Then, if N is greater than 1, move back N-1 more statements."
  (interactive "p")
  (versor:as-motion-command
   (let ((starting-point (point))
	 (previous-end nil))
     (message "Going back %d statements" n)
     (beginning-of-statement-internal)
     (when (= (point) starting-point) ; this means we were already at the start, so go back another
       (languide:debug-message 'previous-statement "Was already at start, going back an extra one")
       (incf n))
     (while (> n 1)
       (message "  %d statements left to go back over" n)
       (setq previous-end (point))
       (message "  moving into previous statement")
       (move-into-previous-statement)
       (message "  that gets us to %d" (point))
       (beginning-of-statement-internal)
       (message "Now gone back another statement, to %d" (point))
       (decf n))
     (message "final move to beginning of statement")
     (message "that gets us to %d" (point))
     (let ((end (if (and versor:statement-up-to-next previous-end)
		    previous-end
		  (save-excursion
		    (let ((start (point)))
		      (condition-case nil
			  (end-of-statement-internal)
			(error (message "Could not find end of statement")))
		      (point))))))
       (versor:set-current-item (point) end)
       (establish-current-statement 'previous-statement end)))))

(defun next-statement (n)
  "Move to the NTH next statement."
  (interactive "p")
  (versor:as-motion-command
   (let ((first t)
	 (starting (point)))
     (while (> n 0)
     ;; todo: this does the seemingly wrong thing if called before the start of the first statement -- it ends up taking us to the second statement
     (message "at %d (\"%s\"); %d statements left; going to end of current statement" (point) (buffer-substring-no-properties (point) (+ 24 (point))) n)
     (end-of-statement-internal)
     (when first			; go back to see if we were before the statement
       (let ((first-end (point)))
	 (beginning-of-statement-internal)
	 (if (> (point) starting)
	     (setq n 0)
	   (goto-char first-end))))
     (message "at %d; skipping to code at start of next statement" (point))
     (skip-to-actual-code)
     (message "that gets us to %d" (point))
     (when (> n 1)
       (message "moving into next statement")
       (move-into-next-statement)
       (message "that gets us to %d" (point)))
     (decf n)
     (setq first nil)))
   (let* ((start (point))
	  (end (save-excursion (end-of-statement-internal) (point))))
     (versor:set-current-item start end)
     (establish-current-statement 'next-statement end))))

(defun navigate-this-whole ()
  "Navigate to the whole of the current statement."
  (interactive)
  (navigate-to 'whole))

(defun navigate-this-head ()
  "Navigate to the head of the current statement."
  (interactive)
  (navigate-to 'head)) 

(defun navigate-this-body ()
  "Navigate to the body of the current statement."
  (interactive)
  (navigate-to 'body))

(defun navigate-this-tail ()
  "Navigate to the tail of the current statement."
  (interactive)
  (navigate-to 'tail))

(defun navigate-this-container ()
  "Navigate to the container of the current statement."
  (interactive)
  (versor:as-motion-command
   (navigate-this-whole)		; in case of assumptions made when moving outwards
   (let ((end (statement-container)))
     (setq navigated-latest-part 'container)
     (versor:set-current-item (point) mark-candidate)
     (versor:display-highlighted-choice "container" languide-parts))))

(defvar statement-navigate-parts-cyclic nil
  "*Whether to step forwards from body (or tail if present) back round to head.")

(defvar statement-navigate-parts-include-container t
  "*Whether to step forwards from body (or tail if present) or back from head, to container.")

(defun get-statement-part (type part)
  "For the major mode, get statement description of TYPE, PART thereof."
  (let ((modal-statements (get major-mode 'statements)))
    (if modal-statements
	(let ((statement (cdr (assoc type modal-statements))))
	  (if statement
	      (let ((description (cdr (assoc part statement))))
		(if description
		    description
		  nil))
	    nil))
      nil)))

(defun statement-navigate-parts-next (&optional ignore)
  "Navigate to the next part of the statement."
  (interactive)
  (versor:as-motion-command
   (case navigated-latest-part
     ('container (navigate-this-head))
     ('whole (navigate-this-head))
     ('head (navigate-this-body))
     ('body (if (get-statement-part statement-navigation-type 'tail)
		(navigate-this-tail)
	      (navigate-this-head)))
     ('tail
      (cond
       (statement-navigate-parts-include-container (navigate-this-container))
       (statement-navigate-parts-cyclic (navigate-this-head))
       (t (navigate-this-whole)))))))

(defun statement-navigate-parts-previous (&optional ignore)
  "Navigate to the previous part of the statement."
  (interactive)
  (versor:as-motion-command
   (case navigated-latest-part
     ('whole (navigate-this-body))
     ('container (navigate-this-head))
     ('head
      (cond ((get-statement-part statement-navigation-type 'tail) (navigate-this-tail))
	    (statement-navigate-parts-include-container (navigate-this-container))
	    (statement-navigate-parts-cyclic (navigate-this-body))
	    (t (navigate-this-whole))))
     ('body (navigate-this-head))
     ('tail (navigate-this-body)))))

(defvar navigated-latest-part nil
  "The latest part of a statement that we navigated to.")

(defvar statements-known nil
  "List of known statement positions (buffer-local).
Each element is
  (startpos endpos type (part start . end) (part start . end) ...)
and the list is sorted by starting position.
Whenever we change something in the buffer, we invalidate
all the data referring to things further down the buffer.")

(defvar latest-statement-known nil
  "The latest-used entry in statements-known.
This is kept as a handy cache.")

(mapcar 'make-variable-buffer-local
	'(navigated-latest-part
	  statements-known
	  latest-statement-known))

(defun languide-after-change-function (start end length)
  "After a change, some of languide's cached information may be wrong, so throw it away."
  (let ((statements-prev statements-known)
	(statements statements-known))
    (while statements
      (let* ((statement (car statements))
	     (stastart (first statement)))
	(when (>= stastart start)
	  (rplacd statements-prev nil)
	  (setq statements nil)))
      (setq statements-prev statements
	    statements (cdr statements)))))

(defun statement-remember (start type &optional end)
  "Remember that there is a statement starting at START, of TYPE.
See the variable statements-known."
  (message "remembering %S at %S..%S" type start end)
  (if statements-known
      (if (< start (first (car statements-known)))
	  ;; I think I can do the rest more clearly if I get this
	  ;; case out of the way first
	  (push (setq latest-statement-known (list start end type))
		statements-known)
	(let ((statements-prev statements-known)
	      (statements (cdr statements-known))
	      (done nil))
	  (while (and statements
		      (not done))
	    (let* ((statement (car statements))
		   (stastart (first statement)))
	      (cond
	       ((= start stastart)
		(rplacd statement (list end type))
		(setq latest-statement-known statement
		      done t))
	       ((> stastart start)
		(rplacd statements-prev
			(cons (setq latest-statement-known (list start end type))
			      statements))
		(setq done t))
	       (t
		)))
	    (setq statements-prev statements
		  statements (cdr statements)))
	  (unless done
	    (rplacd statements-prev
		    (list
		     (setq latest-statement-known (list start end type)))))))
    (setq statements-known
	  (list
	   (setq latest-statement-known (list start end type)))))
  latest-statement-known)

(defun statement-find (start)
  "Find the statement starting at START."
  (if (and (consp latest-statement-known)
	   (= start (car latest-statement-known)))
      latest-statement-known
    (assoc start statements-known)))

(defun statement-remember-part (statement-start type part start end)
  "Remember that for the statement starting at STATEMENT-START and of TYPE, the PART runs from START to END."
  (when part
    (let ((statement (statement-find statement-start)))
      (when (null statement)
	(setq statement (statement-remember statement-start type)))
      (when (null statement)
	(error "Null statement in statement-remember-part"))
      (let ((part (assoc part (cdddr statement))))
	(message "statement-remember-part got statement=%S part=%S" statement part)
	(if part
	    (rplacd part (cons part (cons start end)))
	  (rplacd (cddr statement)
		  (cons (cons part (cons start end))
			(cdddr statement))))))))

(defun statement-find-part (start part)
  "For the statement at START, find cached PART as a cons of (start . end)."
  (message "statement-find-part %S %S" start part)
  (let ((statement (statement-find start)))
    (message "statement-find-part got statement %S" statement)
    (if statement
	(if (eq part 'whole)
	    (cons (car statement) (cadr statement))
	  (cdr (assoc part (cdddr statement))))
      nil)))

(defun statement-display-cache ()
  "Display the cached statement data. Meant for debugging."
  (interactive)
  (with-output-to-temp-buffer (format "*Cached statement data for %s*" (buffer-name))
    (dolist (statement statements-known)
      (princ (format "%5S-%5s: %-30S%s\n"
		     (car statement)
		     (if (numberp (cadr statement))
			 (int-to-string (cadr statement))
		       "?")
		     (caddr statement)
		     (if (cdddr statement)
			 (format " %S" (cdddr statement))
		       "")
		     )))))

(defvar languide-parts '("container" "whole" "head" "body" "tail")
  "The parts we can navigate to.")

(defvar languide-last-statement-selector-command nil
  "The last command to call establish-current-statement.")

(defvar languide-safe-commands
  '(statement-navigate-parts-next
    statement-navigate-parts-previous)
  "Commands that leave things as languide would like to find them.")

(defun latest-move-was-languide ()
  "Return whether the latest move was by languide."
  (or (eq last-command languide-last-statement-selector-command)
      (memq last-command languide-safe-commands)
      (and (memq last-command versor:commands)
	   (memq versor:last-vicarious-command languide-safe-commands))))

(defun navigate-to (part)
  "Navigate to PART of the current statement."
  (setq navigated-latest-part part)
  (add-hook 'after-change-functions 'languide-after-change-function nil t) ; to invalidate cache as needed
  (let* ((old-position (point))		; in case we give up
	 ;; see which statement we are now on; if it is the same as the last
	 ;; time we did any navigation, we can use cached navigation data
	 (prelocated (latest-move-was-languide))
	 (statement-start (if prelocated
			      (progn
				(languide:debug-message 'navigate-to "re-using statement position from latest-statement-known=%S" latest-statement-known)
				(car latest-statement-known))
			    (progn
			      (beginning-of-statement-internal)
			      (languide:debug-message 'navigate-to "navigate-to %S found statement begins \"%s\""
						      part (buffer-substring (point) (+ 20 (point))))
			      (point))))
	 (remembered (statement-find-part statement-start part)))
    (if remembered
	(progn				; we have cached data for this
	  (message "using cached statement position %d..%d" (car remembered) (cdr remembered))
	  (set-mark-candidate (cdr remembered))
	  (goto-char (car remembered))
	  (versor:set-current-item (car remembered) (cdr remembered))
	  (versor:display-highlighted-choice (symbol-name part) languide-parts))
      (let* ((type (if prelocated
		       (third latest-statement-known)
		     (identify-statement nil)))
	     (directions (get-statement-part type part)))
	;; no cached data, really do the navigation
;;;;;;;;;;;;;;;; type is coming through as nil if we are prelocated but had not cached this part
	(message "not cached; navigate-to %S %S got %S" type part directions)
	(if directions
	    (if (eq (car directions) 'statement-navigate)
		(progn
		  (goto-char statement-start)
		  (statement-navigate (cdr directions))
		  ;; cache the data in case we want it again;
		  (message "caching statement position %d..%S" (point) mark-candidate)
		  (statement-remember-part statement-start type
					   part (point) mark-candidate)
		  (versor:set-current-item (point) mark-candidate)
		  (versor:display-highlighted-choice (symbol-name part) languide-parts))
	      (goto-char old-position)
	      (error "Don't know how to handle directions like \"%S\"" directions))
	  (goto-char old-position)
	  (error "No %S defined for \"%S\" for %S" part type major-mode))))))

(defvar mark-candidate nil
  "Where the mark will be set at the end of statement-navigate.
We set this (using set-mark-candidate) instead of setting the mark,
at each stage of navigation, to avoid polluting the mark-ring.")

(defun latest-statement-navigation-end (&optional junk)
  "Return the end of the latest statement navigation result.
Optional argument for compatibility with other things that get the versor
package to the end of an item."
  mark-candidate)

(defvar transient-transient-mark-mode 120
  "*Whether to turn transient-mark-mode on briefly after each statement navigation.
if this is a number, is used as the number of seconds for which to do this.
If you make this as long as you're likely to wait, it will in effect sit there
until you do something else.")

(defun statement-navigate (directions)
  "Take DIRECTIONS for navigating around a statement.
Leave point at the start of the selected section."
  (setq mark-candidate nil)
  (dolist (direction directions)
    ;; (message " statement-navigate: %S" direction)
    (cond
     ((stringp direction)
      ;; (message "Searching for %s" direction)
      (re-search-forward direction (point-max) t))
     ((consp direction)
      ;; (message "Evaluating direction %S" direction)
      (eval direction))
     (t (error "unknown navigation element %S" direction))
     )
    (set-mark mark-candidate) ; todo: not sure if this is needed or appropriate
    ;; (message "Mark candidate now %S" mark-candidate)
    )
  (set-mark mark-candidate))

(defun set-mark-candidate (m)
  "Set the mark candidate to M.
At the end of statement-navigate, this is used to set the mark.
This avoids polluting the mark ring with intermediate marks."
  (setq mark-candidate m))

(defun expression (&optional n)
  "Move forward over an expression.
For use in statement-navigate."
  (interactive)
  (set-mark-candidate (point))
  (forward-sexp (if n n 1))
  (if (interactive-p) (set-mark mark-candidate)))

(defun expression-contents (&optional n)
  "Select the contents of an expression (omitting initial and final whitespace).
Intended for use from statement-navigate."
  (interactive)
  (if (null n) (setq n 1))
  (message "expression-contents starting from %d (\"%s\")" (point) (buffer-substring (point) (+ (point) 8)))
  (forward-sexp n)
  (let ((after-end (point)))
    (forward-sexp (- n))
    (if (looking-at "\\s(")
	(progn
	  (forward-char 1) ; or (down-list 1) would do, but this is probably quicker
	  (skip-syntax-forward " ")
	  (let ((start (point)))
	    (goto-char (1- after-end))
	    (skip-syntax-backward " ")
	    (set-mark-candidate (point))
	    (goto-char start)))
      (progn
	(set-mark-candidate after-end))))
  (if (interactive-p) (set-mark mark-candidate)))

(defun expressions ()
  "Select as many expressions as possible, stopping on hitting a non-balanced closing bracket."
  (interactive)
  (let ((start (point)))
    (goto-char (scan-lists start 1 1))
    (let ((after-end (point)))
      (goto-char (1- after-end))
      (skip-syntax-backward " ")
      (set-mark-candidate (point))
      (goto-char start)))
  (if (interactive-p) (set-mark mark-candidate)))

(defun upto (pattern)
  "Select up to the start of PATTERN."
  (let ((start (point)))
    (if (re-search-forward pattern (point-max) t)
	(set-mark-candidate (match-beginning 0))
      (error "No %s found" pattern))
    (goto-char start))
  (if (interactive-p) (set-mark mark-candidate)))

(defun from-start-of-statement ()
  "Move to the start of the statement."
  (set-mark-candidate statement-latest-start))

(defun start-of-match ()
  "Move point to the start of the match just done (for statement navigation)."
  (goto-char (match-beginning 0)))

(defun preceding-expression ()
  "Select the preceding expression (for statement navigation)."
  (backward-sexp 1)
  (expression))

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
