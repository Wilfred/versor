;;;; languide.el -- language-guided editing
;;; Time-stamp: <2004-05-20 11:37:10 john>
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

(require 'cl)
(require 'modal-functions)
(provide 'languide)

;; To use languide, you need to:
;;   add the directory containing this elisp file, and its companions, to your load-path
;;   load this file
;; This is designed to work with versor, so you may want to have loaded that too.
;; It is also designed with vr-mode in mind.

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

(defun establish-current-statement ()
  "Record some things about the current statement."
  (let ((type (identify-statement nil)))
    (statement-set-type type)
    (statement-remember (point) type)
    (setq statement-latest-start (point)
	  navigated-latest-part 'whole)))

(defun beginning-of-statement ()
  "Move to the beginning of the statement.
Argument 1 means the current statement, 2 the next, etc."
  (interactive "p")
  (beginning-of-statement-internal)
  (establish-current-statement))

(global-set-key [ kp-subtract ] 'beginning-of-statement-internal)

(defun end-of-statement ()
  "Move to the end of the current statement.
Argument 1 means the current statement, 2 the next, etc."
  (interactive "p")
  (beginning-of-statement-internal)
  (establish-current-statement)
  (end-of-statement-internal))

(defun previous-statement (n)
  "Move to the NTH previous statement."
  (interactive "p")
  (while (> n 0)
    (message "%d left; going to beginning of current statement" n)
    (beginning-of-statement-internal)
    (message "moving into previous statement")
    (move-into-previous-statement)
    (decf n))
  (message "final move to beginning of statement")
  (beginning-of-statement-internal)
  (establish-current-statement))

(defun next-statement (n)
  "Move to the NTH next statement."
  (interactive "p")
  (while (> n 0)
    (message "%d left; going to end of current statement" n)
    (end-of-statement-internal)
    (message "skipping to code at start of next statement")
    (skip-to-actual-code)
    (when (> n 1)
      (message "moving into next statement")
      (move-into-next-statement))
    (decf n))
  (establish-current-statement))

(defun safe-scan-lists (from count depth)
  "Like scan-lists, but returns nil on error."
  (condition-case
      error-var
      (scan-lists from count depth)
    (error nil)))

(defun outward-once ()
  "Move outward one level of brackets, going backward.
Returns point, if there was a bracket to go out of, else nil."
  (interactive)
  (let ((where (safe-scan-lists (point) -1 1)))
    (if where
	(progn
	  (goto-char where)
	  where)
      (progn
	(goto-char (point-min))
	nil))))

(defmodel insert-compound-statement-open ()
  "Insert the start of a compound statement")

(defmodel insert-compound-statement-close ()
  "Insert the end of a compound statement")

(defmodel statement-container ()
  "Select the container of the current statement.")

(defmodel scope-around (whereat)
  "Return (as a list / multiple value) the start and end of the scope around WHEREAT.")

(defmodel binding-around (whereat)
  "Return (as a list / multiple value) a description of the binding around WHEREAT.
The results are:
  name                The name bound, as a string
  value               The expression giving the initial value for the variable,
                      as a string
  namestart nameend   Character positions of the start and end of the name
  valuestart valueend Character positions of the start and end of the initial
                      value expression
")

(defmodel variable-reference (varname) "")

;;;; statement-based stuff

(defmacro defstatement (statement modes &rest parts)
  "Define, STATEMENT, in MODES, to consist of PARTS."
  (defstatement0 statement modes parts))

(defun defstatement0 (statement modes parts)
  "See defstatement."
  (let ((documentation nil))
    (when (stringp (car parts))
      (setq documentation (car parts)
	    parts (cdr parts)))
    (dolist (mode modes)
      (let ((modal-statement-list (get mode 'statements)))
	(setq modal-statement-list
	      (cons (cons statement (make-statement parts))
		    modal-statement-list))
	(put mode 'statements modal-statement-list)))))

(defun make-statement (parts)
 "Make up a statement description from PARTS."
  (mapcar 'make-statement-part parts))

(defun make-statement-part (part)
  "Turn PART into a statement descriptor."
  (let ((type (car part)))
    (cond
     ((memq type '(head body tail))
      (cons type `(statement-navigate ,@(cdr part))))
     ((eq type 'create)
      (mapcar 'make-statement-create-part
	      part))
     (t
      part))))

(defun make-statement-create-part (part)
  "Process PART as a statement creation part."
  (cond
   ((consp part)
    (let ((part-op (car part)))
      (cond
       ((eq part-op 'template)
	;; (message "defining template using %S" (cdr part))
	(let* ((name (gensym))
	       (template (tempo-define-template (symbol-name name) (cdr part))))
	  (list 'template template)))
       (t part))))
   (t part)))

;;;; execute statement navigation

;;;; commands for statement navigation

(defun statement-types (&optional full)
  "Return (and display, if interactive) the list of statement types available in this mode."
  (interactive "P")
  (let ((statement-types (get major-mode 'statements)))
    (when (interactive-p)
      (with-output-to-temp-buffer "*Statement types*"
	(dolist (statement-description statement-types)
	  (let ((statement (car statement-description)))
	    (princ (format "%c %s\n" (if (eq statement statement-navigation-type)
					 ?* ? )
			   (symbol-name statement)))
	    (when full
	      (let ((blueprint (cdr (assoc 'create  (cdr statement-description)))))
		(when blueprint
		  (let ((template (cadr (assoc 'template blueprint))))
		    (princ (format "    %S\n\n" (symbol-value template)))))))))))
    statement-types))

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
  (message "Setting statement type to %S" type)
  (if type
      (progn
	(setq statement-navigation-type (if (symbolp type) type (intern type))
	      statement-navigation-type-string (if (symbolp type) (symbol-name statement-navigation-type) type))
	(force-mode-line-update t)
	t)
    (progn
      (message "No type given")
      nil)))

(defun statement-type-at-point ()
  "Return the type of statement at point."
  (save-excursion
    (beginning-of-statement 1)
    (identify-statement nil)))

(defun create-statement ()
  "Insert a statement of the current type."
  (interactive)
  (create-or-surround nil))

(defun surround-statement ()
  "Surround the region with a statement of the current type."
  (interactive)
  (create-or-surround t))

;;;; pieces for the commands

(defun precondition-require (requirement)
  "Ensure that REQUIREMENT is met."
  ;; this is lisp-specific!
  (let ((require-string (format "(require '%s)" requirement)))
    (save-excursion
      (goto-char (point-min))
      (unless (search-forward require-string (point-max) t)
	(goto-char (point-max))
	(re-search-backward "^(\\(require\\)\\|\\(provide\\)" (point-min) 'to-limit)
	(beginning-of-line 2)
	(insert require-string "\n")))))

(defun precondition-not-within (from to)
  "Make sure we are not between matches for FROM and TO."
  (let* ((from-place (save-excursion (re-search-backward from (point-min) t)))
	 (to-place (save-excursion (re-search-backward to (point-min) t))))
    (if (and from-place
	     (or
	      (and to-place (> from-place to-place))
	      (not to-place)))
	(re-search-forward to (point-max) t))))

(defvar preconditions
  '((require . precondition-require)
    (not-within . precondition-not-within)
    )
  "The possible preconditions.")

(defun languide-precondition (precondition)
  "Implement PRECONDITION."
  (message "want precondition %S" precondition)
  (let ((handler (cdr (assoc (car precondition) preconditions))))
    (if handler
	(apply handler (cdr precondition))
      (error "Unknown precondition: %S" precondition))))

(defun languide-postcondition (postcondition)
  "Implement POSTCONDITION."
  (message "want postcondition %S" postcondition)
  )

(defun create-or-surround (surrounding)
  "Insert a statement of the current type, optionally SURROUNDING the current region.
Interactively, uses the current surrounding / following status."
  (interactive (list (not (eq statement-navigation-forwards 'forwards))))
  (let ((tempo-interactive t)
	(description (get-statement-part statement-navigation-type 'create)))
    (if description
	(progn
	  (message "Handling description %s" description)
	  (when (eq (car (car description)) 'precondition)
	    (message "precondition: %S" (cdr (car description)))
	    (mapcar 'languide-precondition (cdr (car description)))
	    (setq description (cdr description)))
	  (when (eq (car (car description)) 'template)
	    (message "Inserting template %S %S" (second (car description)) surrounding)
	    (tempo-insert-template (second (car description)) surrounding)
	    (setq description (cdr description)))
	  (when (eq (car (car description)) 'postcondition)
	    (message "postcondition %S" (cdr (car description)))
	    (mapcar 'languide-postcondition (cdr (car description)))
	    (setq description (cdr description))))
      (error "No %S defined for %S for %S" part statement-navigation-type major-mode))))

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
  (statement-container)
  (versor:display-highlighted-choice "container" languide-parts)  
  )

(defvar statement-navigate-parts-cyclic nil
  "*Whether to step forwards from body (or tail if present) back round to head.")

(defvar statement-navigate-parts-include-container t
  "*Whether to step forwards from body (or tail if present) or back from head, to container.")

(defun statement-navigate-parts-next ()
  "Navigate to the next part of the statement."
  (interactive)
  (case navigated-latest-part
    ('container (navigate-this-head))
    ('whole (navigate-this-head))
    ('head (navigate-this-body))
    ('body (if (get-statement-part statement-navigation-type 'tail)
	       (navigate-this-tail)
	     (navigate-this-head)))
    ('tail
     (cond
      (statement-navigate-parts-include-container (navigate-container))
      (statement-navigate-parts-cyclic (navigate-this-head))
      (t (beginning-of-statement 1))))))

(defun statement-navigate-parts-previous ()
  "Navigate to the previous part of the statement."
  (interactive)
  (case navigated-latest-part
    ('whole (navigate-this-body))
    ('container (navigate-this-head))
    ('head
     (cond ((get-statement-part statement-navigation-type 'tail) (navigate-this-tail))
	   (statement-navigate-parts-include-container (navigate-container))
	   (statement-navigate-parts-cyclic (navigate-this-body))
	   (t (beginning-of-statement 2))))
    ('body (navigate-this-head))
    ('tail (navigate-this-body))))

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

(defun statement-remember (start type)
  "Remember that there is a statement starting at START, of TYPE.
See the variable statements-known."
  (if statements-known
      (if (< start (first (car statements-known)))
	  ;; I think I can do the rest more clearly if I get this
	  ;; case out of the way first
	  (setq statements-known
		(cons
		 (setq latest-statement-known (list start nil type))
		 statements-known))
	(let ((statements-prev statements-known)
	      (statements (cdr statements-known))
	      (done nil))
	  (while (and statements
		      (not done))
	    (let* ((statement (car statements))
		   (stastart (first statement)))
	      (cond
	       ((= start stastart)
		(rplacd statement (list nil type))
		(setq latest-statement-known statement
		      done t))
	       ((> stastart start)
		(rplacd statements-prev
			(cons (setq latest-statement-known (list start nil type))
			      statements))
		(setq done t))
	       (t
		)))
	    (setq statements-prev statements
		  statements (cdr statements)))
	  (unless done
	    (rplacd statements-prev
		    (list
		     (setq latest-statement-known (list start nil type)))))))
    (setq statements-known
	  (list
	   (setq latest-statement-known (list start nil type)))))
  latest-statement-known)

(defun statement-find (start)
  "Find the statement starting at START."
  (if (and (consp latest-statement-known)
	   (= start (car latest-statement-known)))
      latest-statement-known
    (assoc start statements-known)))

(defun statement-remember-part (statement-start type part start end)
  "Remember that for the statement starting at STATEMENT-START and of TYPE, the PART runs from START to END."
  (let ((statement (statement-find statement-start)))
    (when (null statement)
      (setq statement (statement-remember statement-start type)))
    (when (null statement)
      (error "Null statement in statement-remember-part"))
    (let ((part (assoc part (cdddr statement))))
      (message "statement-remember-part got statement=%S part=%S" statement part)
      (if part
	  (rplacd part (cons start end))
	(rplacd (cddr statement)
		(cons (cons start end)
		      (cdddr statement)))))))

(defun statement-find-part (start part)
  "For the statement at START, find cached PART as a cons of (start . end)."
  (let ((statement (statement-find start)))
    (if statement
	(cdr (assoc part (cddr statement)))
      nil)))

(defvar languide-parts '("container" "whole" "head" "body" "tail")
  "The parts we can navigate to.")

(defun navigate-to (part)
  "Navigate to PART of the current statement."
  (setq navigated-latest-part part)
  (add-hook 'after-change-functions 'languide-after-change-function nil t)
  (let ((old-position (point)))		; in case we give up
    ;; see which statement we are now on; if it is the same as the last
    ;; time we did any navigation, we can use cached navigation data
    (beginning-of-statement-internal 1)
    (message "navigate-to %S found statement begins \"%s\"" part (buffer-substring (point) (+ 20 (point))))
    (let* ((statement-start (point))
	   (remembered (statement-find-part statement-start part)))
      (if remembered
	  (progn			; we have cached data for this
	    (message "using cached statement position %d..%d" (car remembered) (cdr remembered))
	    (set-mark-candidate (cdr remembered))
	    (goto-char (car remembered))
	    (versor:display-highlighted-choice (symbol-name part) languide-parts))
	(let* ((type (identify-statement nil))
	       (directions (get-statement-part type part)))
	  ;; no cached data, really do the navigation
	  (message "not cached; navigate-to %S %S got %S" type part directions)
	  (if directions
	      (if (eq (car directions) 'statement-navigate)
		  (progn
		    (statement-navigate (cdr directions))
		    ;; cache the data in case we want it again;
		    (statement-remember-part statement-start type
					     part (point) mark-candidate)
		    (message "caching statement position %d..%d" (point) mark-candidate)
		    (versor:display-highlighted-choice (symbol-name part) languide-parts))
		(goto-char old-position)
		(error "Don't know how to handle directions like %S" directions))
	    (goto-char old-position)
	    (error "No %S defined for %S for %S" part type major-mode)))))))

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
    (set-mark mark-candidate)
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

(defun start-of-match ()
  "Move point to the start of the match just done (for statement navigation)."
  (goto-char (match-beginning 0)))

(defun preceding-expression ()
  "Select the preceding expression (for statement navigation)."
  (backward-sexp 1)
  (expression))

;;;; define statements for some common languages

(load "languide-c-like")
(load "languide-sh-like")
(load "languide-lisp-like")
(load "languide-html-like")

;;; end of languide.el
