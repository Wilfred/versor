;;;; languide.el -- language-guided editing
;;; Time-stamp: <2004-01-26 16:33:38 john>
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
(provide 'languide)

;; To use languide, you need to:
;;   add the directory containing this elisp file, and its companions, to your load-path
;;   load this file
;; This is designed to work with versor, so you may want to have loaded that too.
;; It is also designed with vr-mode in mind.

(defun safe-funcall (fn &rest args)
  "Call FN with remaining ARGS if not nil, else return first of ARGS"
  ;; (message "(safe-funcall %S %S)" fn args)
  (if fn
      (apply fn args)
    (car args)))

(defmacro defmodel (fun args doc &optional interactive)
  "Define a caller for contextual FUN with ARGS and optional INTERACTIVE.
The function is a property of that name on the symbol naming the major mode."
  (append
   (list 'defun fun args doc)
   (if interactive
       (list interactive)
     nil)
   (list (append (list 'safe-funcall
		       (list 'get 'major-mode (list 'quote fun)))

		 args))))


(defun defmodal0 (fun mode args body)
  "Define FUNCTION, for MODE with ARGS and BODY.
This is for use inside defmodal."
  (let* ((this-name (intern (concat (symbol-name mode) "-_-" (symbol-name fun)))))
    ;; (message "Defining %S to be %S for %S with args %S and body %S" this-name fun mode args body)
    (list 'progn
	  (append (list 'defun this-name args)
		  body)
	  (list 'put (list 'quote mode) (list 'quote fun) (list 'quote this-name)))))

(defmacro defmodal (fun mode args &rest body)
  "Define FUNCTION, for MODE with ARGS and BODY."
  (if (consp mode)
      (append
       '(progn)
       (mapcar (lambda (this-mode)
		 (defmodal0 fun this-mode args body))
	       mode))
    (defmodal0 fun mode args body)))

(defmodel beginning-of-statement-internal (n)
  "Move to the beginning of the statement.
Argument 1 means the current statement, 2 the next, etc.
Do not do auxiliary stuff that might be associated with this."
  (interactive "p"))

(defmodel end-of-statement-internal (n)
  "Move to the end of the current statement.
Argument 1 means the current statement, 2 the next, etc.
Do not do auxiliary stuff that might be associated with this."
  (interactive "p"))

(defmodel identify-statement (default)
  "Note what kind of statement we are at the start of.
Need not work if not at the start of a statement.
If the statement cannot be identified, return DEFAULT.")

(defun beginning-of-statement (n)
  "Move to the beginning of the statement.
Argument 1 means the current statement, 2 the next, etc."
  (interactive "p")
  (beginning-of-statement-internal n)
  (statement-set-type (identify-statement nil))
  (setq statement-latest-start (point)
	navigated-latest-part 'whole))

(defun end-of-statement (n)
  "Move to the end of the current statement.
Argument 1 means the current statement, 2 the next, etc."
  (interactive "p")
  (end-of-statement-internal n)
  (statement-set-type (identify-statement nil))
  (setq statement-latest-start (point)
	navigated-latest-part 'whole))

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

(defmodel scope-around (whereat) "")
(defmodel binding-around (whereat) "")
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

(defvar statement-navigation-forwards 'forwards
  "The current direction for statement navigation.")

(defvar statement-navigation-direction-string "following"
  "A string describing the current direction for statement navigation.")

(defvar statement-latest-start 0
  "The latest statement start that we have moved to.")

(defun following ()
  "Indicate that navigation by statement types is to be to the following one.
This means the search will start by looking forwards for the initial keyword."
  (interactive)
  (setq statement-navigation-forwards 'forwards
	statement-navigation-direction-string "following"))

(defun surrounding ()
  "Indicate that navigation by statement types is to be to the surrounding one.
This means the search will start by looking backwards for the initial keyword."
  (interactive)
  (setq statement-navigation-forwards 'backwards
	statement-navigation-direction-string "surrounding"))

(defun toggle-following/surrounding ()
  "Toggle between following and surrounding."
  (interactive)
  (if (eq statement-navigation-forwards 'forwards)
      (surrounding)
    (following)))

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

;; we indicate the current statement-navigation direction and the current statement type,
;; in the mode line
(unless (memq 'statement-navigation-type-string global-mode-string)
    (setq global-mode-string
	  (append global-mode-string
		  '(" {"
		    statement-navigation-direction-string
		    " "
		    statement-navigation-type-string
		    "} "))))

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
  (identify-statement nil))

(defun navigate-head ()
  "Navigate to the head of the relevant statement."
  (interactive)
  (navigate-to statement-navigation-type statement-navigation-forwards 'head)
  ;; take as implicit that we will now want to go to other parts of the same statement
  (surrounding))

(defun navigate-body ()
  "Navigate to the body of the relevant statement."
  (interactive)
  (navigate-to statement-navigation-type statement-navigation-forwards 'body)
  ;; take as implicit that we will now want to go to other parts of the same statement
  (surrounding))

(defun navigate-tail ()
  "Navigate to the tail of the relevant statement."
  (interactive)
  (navigate-to statement-navigation-type statement-navigation-forwards 'tail)
  ;; take as implicit that we will now want to go to other parts of the same statement
  (surrounding))

(defun statement-navigate-next-head ()
  "Navigate to the head of the next statement of the current type."
  (interactive)
  (following)
  (navigate-head))

(defun navigate-whole ()
  "Select the whole statement around point."
  (interactive)
  (setq navigated-latest-part 'whole)
  (message "Whole statements not yet implemented")
)

(defun navigate-container ()
  "Select the container of the statement around point."
  (interactive)
  (setq navigated-latest-part 'container)
  (beginning-of-statement-internal 1)
  (backward-up-list 1)
  (beginning-of-statement 1)
)

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

(defvar navigated-latest-part 'tail
  "The latest part to which we have navigated.")

(defvar statement-navigate-parts-cyclic nil
  "*Whether to step forwards from body (or tail if present) back round to head.")

(defvar statement-navigate-parts-include-container t
  "*Whether to step forwards from body (or tail if present) or back from head, to container.")

(defun statement-navigate-parts-next ()
  "Navigate to the next part of the statement."
  (interactive)
  (setq statement-navigation-forwards 'forwards)
  (case navigated-latest-part
    ('container (navigate-head))
    ('whole (navigate-head))
    ('head (navigate-body))
    ('body (if (get-statement-part statement-navigation-type 'tail)
	       (navigate-tail)
	     (navigate-head)))
    ('tail
     (cond
      (statement-navigate-parts-include-container (navigate-container))
      (statement-navigate-parts-cyclic (navigate-head))
      (t (beginning-of-statement 1))))))

(defun statement-navigate-parts-previous ()
  "Navigate to the previous part of the statement."
  (interactive)
  (setq statement-navigation-forwards 'forwards)
  (case navigated-latest-part
    ('whole (navigate-body))
    ('container (navigate-head))
    ('head
     (cond ((get-statement-part statement-navigation-type 'tail) (navigate-tail))
	   (statement-navigate-parts-include-container (navigate-container))
	   (statement-navigate-parts-cyclic (navigate-body))
	   (t (beginning-of-statement 2))))
    ('body (navigate-head))
    ('tail (navigate-body))))

(defun navigate-to (type start-direction part)
  "Navigate to a statement of TYPE, START-DIRECTION, PART thereof."
  (setq navigated-latest-part part)
  (goto-char statement-latest-start)
  (let ((directions (get-statement-part type part)))
    (message "navigate-to %S %S %S got %S" type start-direction part directions)
    (if directions
	(if (eq (car directions) 'statement-navigate)
	    (statement-navigate (cdr directions) start-direction)
	  (error "Don't know how to handle directions like %S" directions))
      (error "No %S defined for %S for %S" part type major-mode))))

(defvar mark-candidate nil
  "Where the mark will be set at the end of statement-navigate.")

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

(defun statement-navigate (directions start-direction &optional do-not-mark)
  "Take DIRECTIONS for navigating around a statement. If START-DIRECTION is 'forwards start by looking forwards.
Leave point at the start of the selected section.
Unless optional DO-NOT-MARK is set, leave the mark at the end of the selected section."
  ;; (message "Statement-Navigate %S %S" directions forwards)
  (let ((first t))
    (setq mark-candidate nil)
    (dolist (direction directions)
      ;; (message " statement-navigate: %S" direction)
      (cond
       ((stringp direction)
	;; (message "Searching for %s" direction)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; this bit needs changing
	(cond
	 ((or (not first)
	      (eq start-direction 'forwards))
	  (re-search-forward direction (point-max) t))
	 ((eq start-direction 'backwards)
	  (re-search-backward direction (point-min) t)
	  (goto-char (match-end 0)))
	 (t nil)))
       ((consp direction)
	;; (message "Evaluating direction %S" direction)
	(eval direction)))
      (setq first nil)
      ;; (message "Mark candidate now %S" mark-candidate)
      ))
  (when (and (not do-not-mark) mark-candidate)
    (set-mark mark-candidate)
    (when (and transient-transient-mark-mode
	       (not transient-mark-mode))
      (transient-mark-mode 1)
      (sit-for (if (numberp transient-transient-mark-mode)
		   transient-transient-mark-mode
		 1))
      (transient-mark-mode -1))))

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
	  (forward-char 1);; or (down-list 1) would do, but this is probably quicker
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

;;;; voice commands
;;; Part of the point of this project was to create a form of editing that is voice-friendly,
;;; so I have included these as an inherent part of it.

(defvar languide-languages-commands
  '(following
    surrounding
    statement-set-type
    statement-types
    create-statement
    surround-statement
    ("head" . navigate-head)
    ("body" . navigate-body)
    ("tail" . navigate-tail)
    ("statement head" . navigate-head)
    ("statement body" . navigate-body)
    ("statement tail" . navigate-tail))
  "Voice commands for navigating by the sense of the language.")

;;; end of languide.el
