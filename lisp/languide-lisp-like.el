;;;; languide-lisp-like.el -- Lisp, Elisp, Scheme definitions for language-guided editing
;;; Time-stamp: <2006-02-20 11:49:51 john>
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

(provide 'languide-lisp-like)

(defmodal beginning-of-statement-internal (lisp-mode
					   emacs-lisp-mode
					   lisp-interaction-mode
					   scheme-mode)
  ()
  "Move to the beginning of Lisp statement, which is a pretty nebulous concept."
  ;;;;;;;;;;;;;;;; wrong -- this effectively does previous-statement
  (unless (looking-at "(") (backward-up-list 1))
  (forward-sexp -1))

(defmodal end-of-statement-internal (lisp-mode
				     emacs-lisp-mode
				     lisp-interaction-mode
				     scheme-mode)
  ()
  "Move to the end of Lisp statement."
  ;;;;;;;;;;;;;;;; wrong -- this effectively does next-statement
  (unless (looking-at "(") (backward-up-list 1))
  (forward-sexp 1))

(defmodal identify-statement (lisp-mode emacs-lisp-mode lisp-interaction-mode) (default)
  "Identify a Lisp form or function."
  (if (looking-at "(\\([-:_a-z0-9]+\\)")
      (let ((string (match-string 1)))
	(message "Seems to be a %s" string)
	string
	)
    default))

(defmodal insert-compound-statement-open (lisp-mode
					  emacs-lisp-mode
					  lisp-interaction-mode)
  ()
  "Insert a progn."
  (insert "(progn "))

(defmodal insert-compound-statement-close (lisp-mode
					   emacs-lisp-mode
					   lisp-interaction-mode)
  ()
  "Insert a progn's closing bracket."
  (insert ")"))

(defun find-next-lisp-binding-outwards ()
  "Move to the next enclosing binding."
  (let ((binding-pattern "(let\\*?\\>"))
    (while (and (outward-once)
		(not (looking-at binding-pattern))))
    (looking-at binding-pattern)))

(defmodal variables-in-scope (lisp-mode emacs-lisp-mode lisp-interaction-mode) (whereat)
  "Return the alist list of variables in scope at WHEREAT."
  ;; todo: make this spot lambda bindings too
  ;; todo: add parameters to the list (last)
  (save-excursion
    (goto-char whereat)
    (beginning-of-defun)
    (let ((bod (point))
	  next
	  (parse-sexp-ignore-comments t)
	  (variables nil))
      (goto-char whereat)
      ;; (message "Looking for bindings between %d and %d" bod whereat)
      (while (find-next-lisp-binding-outwards)
	(save-excursion
	  (down-list 2)
	  (skip-to-actual-code)
	  ;; (message "Entering binding list got us to %d" (point))
	  (while (setq next (safe-scan-sexps (point) 1))
	    (skip-to-actual-code)
	    ;; (message "Looking at %s as binding" (buffer-substring-no-properties (point) next))
	    (when (looking-at "(") (forward-char) (skip-to-actual-code))
	    (let ((start (point)))
	      (forward-sexp)
	      (push (list (buffer-substring-no-properties start (point))) variables))
	    (goto-char next))))
      variables)))

(defmodal move-to-enclosing-scope-last-variable-definition (lisp-mode emacs-lisp-mode lisp-interaction-mode) (&optional variables-needed)
  "Move to the end of the nearest set of variable bindings.
This is the place at which you would naturally insert a new
variable, allowing for its initial value referring to any
variable already declared.
Optional arguments list names of variables needed in the definition of the new one.
This lets clever implementations put the definition as far out as possible.")

(defmodal insert-variable-declaration (lisp-mode emacs-lisp-mode lisp-interaction-mode) (name type initial-value)
  "Insert a definition for a variable called NAME, of TYPE, with INITIAL-VALUE.
Assumes we are at the obvious point to add a new variable.
TYPE and INITIAL-VALUE may be null, but the NAME is required."
  (insert "(" name " " initial-value ")"))

(defun arg-name (arg)
  "Return just the name of ARG, which is in languide's format, which may be just the name, or (name . type)."
  (cond
   ((stringp arg) arg)
   ((symbolp arg) (symbol-name arg))
   ((and (consp arg)
	 (stringp (cdr arg)))
    (cdr arg))
   ((and (consp arg)
	 (symbolp (cdr arg)))
    (symbol-name (cdr arg)))))

(defun insert-lisp-arglist-elements (arglist)
  "Insert the elements of ARGLIST.
They are in languide's format, which may be just the name, or (name . type)."
  (insert (mapconcat 'arg-name arglist " ")))

(defmodal insert-function-declaration (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  (name result-type arglist
	body &optional docstring)
  "Insert a function definition for NAME, returning RESULT-TYPE, taking ARGLIST, and implemented by BODY.
A DOCSTRING may also be given."
  (insert "(defun " name " (")
  (insert-lisp-arglist-elements arglist)
  (insert ")")
  (newline-and-indent)
  (when (and (stringp docstring)
	     (not (zerop (length docstring))))
    (insert "\"" docstring "\"")
    (newline-and-indent))
  (insert body ")\n\n")
  (beginning-of-defun)
  (indent-sexp))

(defmodal insert-function-call (lisp-mode emacs-lisp-mode lisp-interaction-mode) (name arglist)
  "Insert a function call for a function called NAME taking ARGLIST"
  (insert "(" name " ")
  (insert-lisp-arglist-elements arglist)
  (insert ")"))

(defmodal deduce-expression-type (lisp-mode emacs-lisp-mode lisp-interaction-mode) (value-text)
  "Given VALUE-TEXT, try to deduce the type of it."
  nil)					; nice and easy for dynamically typed languages!

(defmodal move-before-defun (lisp-mode emacs-lisp-mode lisp-interaction-mode) ()
  "Move to before the current function definition."
  (beginning-of-defun 1))

(defmodal static-variable-p (lisp-mode emacs-lisp-mode lisp-interaction-mode) (name where)
  "Return whether a static variable called NAME is visible at WHERE."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "(\\(\\(defvar\\)\\|\\(defconst\\)\\|\\(defcustom\\)\\) +%s" name)
		       (point-max) t)
    ;; todo: should also look in everything we load with require
    ))

(defmodal variable-bindings-in-region (lisp-mode emacs-lisp-mode lisp-interaction-mode) (from to)
  "Return a list of the bindings between FROM and TO.
Each element is a list of:
  name
  type
  scope-begins scope-ends
  initial-value-as-string"
  (save-excursion
    (let ((result nil))
      (goto-char from)
      (while (re-search-forward "\\((lambda\\>\\)\\|\\((let\\*?\\)" to t)
	(let* ((keystart (safe-scan-lists (match-beginning 0) 1 -1))
	       (keyend (safe-scan-sexps keystart 1))
	       (bindings-end (save-excursion
			       (forward-sexp 1)
			       (point)))
	       (scope-end (save-excursion
			    (backward-up-list)
			    (forward-sexp 1)
			    (1- (point)))))
	  (goto-char keystart)
	  (cond
	   ((looking-at "lambda")
	    (let* ((name-start (safe-scan-lists keyend 1 -1))
		   (name-end (safe-scan-sexps name-start 1)))
	      (push (list (buffer-substring-no-properties name-start name-end)
			  nil
			  keyend scope-end
			  nil)
		    result)))
	   ((looking-at "let\\*?\\>")
	    (let ((star (looking-at "let\\*")))
	      (goto-char keyend)
	      (down-list)		; into the bindings
	      (let* ((binding-start (point))
		     (binding-end nil))
		(while (setq binding-end (safe-scan-sexps binding-start 1))
		  (goto-char binding-start)
		  (down-list)
		  (let* ((name-start (point))
			 (name-end (safe-scan-sexps name-start 1))
			 (name-string (buffer-substring-no-properties name-start name-end))
			 (value-end (safe-scan-sexps name-end 1))
			 (value-start (if value-end
					  (safe-scan-sexps value-end -1)
					nil))
			 (value-string (if (and value-start value-end)
					   (buffer-substring-no-properties value-start value-end)
					 nil)))
		    (push (list name-string
				nil
				(if star value-end bindings-end) scope-end
				value-string)
			  result))
		  (setq binding-start binding-end))))))
	  (goto-char bindings-end)))
      (nreverse result))))

(defconst open-bracket (string-to-char "(")
  "Get this out-of-line to avoid confusing indenter when editing functions that use it.")

(defmodal variable-references-in-region (lisp-mode emacs-lisp-mode lisp-interaction-mode) (from to)
  "Return a list of the variable references between FROM and TO.
Each element is a list of:
  name
  location"
  (save-excursion
    (beginning-of-defun)
    (let ((result nil)
	  (bod (point)))
      (goto-char from)
      (while (re-search-forward "\\<[-a-z][-a-z-0-9_]*\\>" to t)
	(let* ((where (match-beginning 0))
	       (end (match-end 0))
	       (preceding-char (char-after (1- where)))
	       (pps (parse-partial-sexp bod where)))
	  (if (and (not (= preceding-char open-bracket))
		   (not (fourth pps))	; inside string
		   (not (fifth pps)))	; inside comment
	      (push (list (match-string-no-properties 0) where) result))
	  (goto-char end)))
      result)))

(defstatement comment (lisp-mode
		       emacs-lisp-mode
		       lisp-interaction-mode)
  "Comment"
  (head ";+ *")
  (body ";+ *" (upto " *$"))
  (tail "$")
  (create (template "; " r n)))

(defstatement defun (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  ""
  (keyword "defun")
  (head "(defun +" (expression-contents 2))
  (body "(defun +" (expression 2) (expressions))
  (create (template & "(defun " (p "Function name to define: ")
		    " (" (p "Argument list: ") ")" n>
		    "\"" (p "Documentation string: ") "\"" n>
		    r> ")" n)))

(defstatement defvar (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  ""
  (keyword "defvar")
  (head "(defvar +" (expression-contents 2))
  (body "(defvar +" (expression 2) (expressions))
  (create (template "(defvar " (p "Variable name to define: ")
		    "\"" (p "Documentation string: ") "\"" n>
		    r> ")" n)))

(defstatement progn (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  "General compound statement for Lisps"
  (keyword "progn")
  (head "(progn" (expression-contents))
  (body "(progn" (expressions))
  (create (template & > "(progn " p n>
		    r ")")))

(defstatement save-excursion (emacs-lisp-mode lisp-interaction-mode)
  ""
  (keyword "save-excursion")
  (head "(save-excursion" (expression-contents))
  (body "(save-excursion" (expressions))
  (create (template & > "(save-excursion " p n>
		    r ")")))

(defstatement save-window-excursion (emacs-lisp-mode lisp-interaction-mode)
  "Emacs-lisp \"save-window-excursion\" special form"
  (keyword "save-window-excursion")
  (head "(save-window-excursion" (expression-contents))
  (body "(save-window-excursion" (expressions))
  (create (template & > "(save-window-excursion " p n>
		    r ")")))

(defstatement while-do (emacs-lisp-mode lisp-interaction-mode)
  "Emacs-lisp \"while\" special form"
  (keyword "while")
  (head "(while" (expression-contents))
  (body "(while" (expression) (expressions))
  (create (template & > "(while " p n>
		    r ")"))
  (begin-end "(while " ")"))

(defstatement unless (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  "Emacs-lisp \"unless\" special form."
  (keyword "unless")
  (head "(unless" (expression-contents))
  (body "(unless" (expression) (expression-contents))
  (create (precondition (require 'cl))
	  (template & > "(unless " p n>
		    r ")"))
  (begin-end "(unless " ")"))

(defstatement condition-chain (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  ""
  (keyword "cond")
  (head "(cond *")
  (body "(cond *" (expressions))
  (create (template & > "(cond" n> "(" p ")" n> "(t (" p " " p ")))")))

(defstatement function-call (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  ""
  (head "(" (expression))
  (body "(" (expression) (expressions))
  (create (template > "(" p ")"))
  (begin-end "(" ")"))

(defstatement variable-declaration (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  ""
  (keyword "let")
  (head "(let *(" (expressions))
  (body "(let *" (expression) (expressions))
  (create
   (template & > "(let ((" (p "Variable name: ") p "))" n> r n> ")"))
  (begin-end "(let (()) " ")"))

(defstatement assignment (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  "Assignment statement"
  (keyword "setq")
  (head "(setq" (expression-contents))
  (body "(setq" (expression) (expression))
  (create (template & > "(setq " (p "Variable name: ") " "
		    r ")")))

(defstatement if-then (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  "If statement without else clause."
  (keyword "when")
  (head "(when" (expression-contents))
  (body "(when" (expression) (expressions))
  (create (precondition (require 'cl))
	  (template & > "(when " p n>
		    r ")"))
  (begin-end "(when \n" ")"))

(defstatement if-then-else (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  "If statement with else clause."
  (keyword "if")
  (head "(if" (expression-contents))
  (body "(if" (expression) (expression-contents))
  (tail "(if" (expression) (expression) (expression-contents))
  (create (template & > "(if " p n>
		    r n>
		    p ")"))
  (begin-end "(if" ")"))

(defstatement and (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  "And expression."
  (keyword "and")
  (begin-end "(and " ")"))

(defstatement or (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  "Or expression."
  (keyword "or")
  (begin-end "(or " ")"))

(defstatement not (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  "Not expression."
  (keyword "not")
  (begin-end "(not " ")"))

;;; end of languide-lisp-like.el
