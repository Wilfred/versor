;;;; languide-lisp-like.el -- Lisp, Elisp, Scheme definitions for language-guided editing
;;; Time-stamp: <2006-04-19 13:58:19 john>
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

(defvar lisp-mode-statement-identities
  '(("when" . if-then)
    ("if" . if-then-else)
    ("setq" . assignment)
    ("let" . variable-declaration)
    ("let*" . variable-declaration)
    ("progn" . progn)
    ("defun" . defun))
  "Alist of lisp functors which are recognized by languide.
Maps strings to symbols.")

(defmodal identify-statement (lisp-mode emacs-lisp-mode lisp-interaction-mode) (default)
  "Identify a Lisp form or function."
  (if (looking-at "(\\([-:_a-z0-9]+\\)")
      (let ((string (match-string-no-properties 1)))
	(message "Seems to be a %s" string)
	(or (cdr (assoc string lisp-mode-statement-identities))
	    ;; (intern string)
	    'function-call
	    )
	)
    default))

(defmodal insert-compound-statement-open (lisp-mode
					  emacs-lisp-mode
					  lisp-interaction-mode)
  ()
  "Insert a progn."
  (languide-insert "(progn "))

(defmodal insert-compound-statement-close (lisp-mode
					   emacs-lisp-mode
					   lisp-interaction-mode)
  ()
  "Insert a progn's closing bracket."
  (languide-insert ")"))

(defun find-next-lisp-binding-outwards (&optional allow-conversions)
  "Move to the next enclosing binding.
Returns the position if it found one, or nil otherwise."
  (interactive)
  (let ((binding-pattern (if allow-conversions
			     "\\((let\\*?\\\\)\\|\\((progn\\)>"
			   "(let\\*?\\>")))
    (while (and (outward-once)
		(not (looking-at binding-pattern))))
    (if (looking-at binding-pattern)
	(point)
      nil)))

(defmodal variables-in-scope (lisp-mode emacs-lisp-mode lisp-interaction-mode) (whereat)
  "Return the alist list of variables in scope at WHEREAT."
  ;; todo: make this spot lambda bindings too
  (save-excursion
    (goto-char whereat)
    (beginning-of-defun)
    (let ((bod (point))
	  next
	  (parse-sexp-ignore-comments t)
	  (variables nil))
      (goto-char whereat)
      ;; first, check we are not in the preamble of a let form:
      (when (or (looking-at "let\\*?")
		(save-excursion
		  (and (safe-backward-sexp)
		       (looking-at "let\\*?"))))
	(safe-backward-up-list 1))
      ;; now, see whether we are in the bindings list of a let* form:
      (let ((possible-current-binding (save-excursion
					(safe-backward-up-list 1)
					(safe-backward-sexp 1)
					(and (looking-at "let\\*?")
					     (match-string-no-properties 0)))))
	(cond
	 ((string= "let*" possible-current-binding)
	  (message "Got some of our own")
	  (while (setq next (safe-scan-sexps (point) -1))
	    (goto-char next)
	    (when (looking-at "(") (forward-char) (skip-to-actual-code))
	    (let ((start (point)))
	      (forward-sexp)
	      (push (list (buffer-substring-no-properties start (point)))
		    variables))
	    (goto-char next))
	  ;; avoid getting the same lot again
	  (safe-backward-up-list 2)
	  )
	 ((string= "let" possible-current-binding)
	  (message "Avoiding this bunch")
	  ;; the bindings we are among are not in scope here, so get out from among them
	  (safe-backward-up-list 2)))
	)
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
	      (push (list (buffer-substring-no-properties start (point)))
		    variables))
	    (goto-char next))))
      ;; got the let-forms, now get the funargs
      (goto-char bod)
      (down-list)
      (when (looking-at "def[um]")
	(when (looking-at "defmod[ae]l") (forward-sexp))
	(forward-sexp 2)
	(let ((limit (safe-scan-sexps (point) 1)))
	  (down-list)
	  (skip-to-actual-code)
	  (message "Arglist starts at %d" (point))
	  (let ((old (point))
		new)
	    (while (and (setq new (safe-scan-sexps old 1))
			(< new limit))
	      (push (list (buffer-substring-no-properties old new)) variables)
	      (goto-char new)
	      (skip-to-actual-code limit)
	      (setq old (point))))))
      variables)))

(defmodal adapt-binding-point (lisp-mode emacs-lisp-mode lisp-interaction-mode) (&optional allow-conversions)
  "Make a binding point suitable to receive another binding."
  (let ((binding-place (point)))
    (save-excursion
      (outward-once) 
      (if (looking-at "(defun")
	  (progn
	    (goto-char binding-place)
	    (languide-insert-before-markers "(let (")
	    (save-excursion ; so we finish at new variable insertion point
	      (languide-insert ")\n")
	      (let ((old (point))
		    new)
		(while (setq new (safe-scan-sexps old 1)) ; move over body forms
		  (setq old new))
		(goto-char old))
	      (languide-insert ")")
	      (backward-up-list)
	      (indent-sexp)))
	(outward-once)
	(cond
	 ((looking-at "\\((let\\)[^*]")
	  ;; todo: add further condition, that one of the variables needed is bound in this set?
	  (save-excursion
	    (replace-match "\\1*" t nil nil 1)))
	 ((and allow-conversions
	       (looking-at "(\\(progn))"))
	  (save-excursion
	    (replace-match "let* ()" t t nil 1))))))))

(defmodal move-to-enclosing-scope-last-variable-definition
  (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  (&optional allow-conversions)
  "Move to the end of the nearest set of variable bindings.
This is the place at which you would naturally insert a new
variable, allowing for its initial value referring to any
variable already declared.

It may create a suitable place if there is none; for example, in Lisp
it could wrap the outermost forms of a \"defun\" with a \"let\".

Optional argument ALLOW-CONVERSIONS allows conversion of possible
scoping points into actual ones. For example, in lisp, this means a
\"progn\" can be changed to a \"let\".

Returns the start of the binding if it found a binding, nil if it went to the top level
of the defun."
  (let ((binding-point (find-next-lisp-binding-outwards allow-conversions)))
    (if binding-point
	(progn
	  (down-list 2)
	  (let ((old (point))
		new)
	    (while (setq new (safe-scan-sexps old 1))
	      (setq old new))
	    (goto-char old))
	  binding-point)
      ;; no let forms, so position ourselves to make a new one at the
      ;; outer level of the defun
      (down-list)
      (forward-sexp 3)			; over "defun", name, args
      (skip-to-actual-code)
      (when (= (char-after) ?\")
	(forward-sexp 1)		; over docstring
	(skip-to-actual-code))
      (when (looking-at "(interactive")
	(forward-sexp 1)
	(skip-to-actual-code))
      nil)))

(defmodal variable-last-possibly-assigned-before (lisp-mode emacs-lisp-mode lisp-interaction-mode) (variable wherebefore)
  "Return the position of the most recent assignment to VARIABLE before WHEREBEFORE."
  (save-excursion
    (goto-char wherebefore)
    (let ((bod (save-excursion (beginning-of-defun) (point))))
      (catch 'got-assignment
	(while (search-backward "(setq" bod t)
	  (let* ((first-name (match-end 0))
		 (end (progn (forward-sexp) (1- (point)))))
	    (goto-char first-name)
	    (catch 'scan-setq
	      (while t
		(skip-to-actual-code)
		(cond
		 ((>= (point) end) (throw 'scan-setq nil))
		 ((string= (buffer-substring-no-properties (point)
							   (save-excursion (forward-sexp) (point)))
			   variable)
		  (throw 'got-assignment (point)))
		 (t (forward-sexp) (forward-sexp))))
	      nil)))
	(goto-char wherebefore)
	(when (re-search-backward
	       (format "(\\(rplaca\\|rplacd\\|incf\\|decf\\) +\\(%s\\)" variable)
	       bod t)
	  (throw 'got-assignment (point)))))))

(defmodal insert-variable-declaration (lisp-mode emacs-lisp-mode lisp-interaction-mode) (name type initial-value)
  "Insert a definition for a variable called NAME, of TYPE, with INITIAL-VALUE.
Assumes we are at the obvious point to add a new variable.
TYPE and INITIAL-VALUE may be null, but the NAME is required."
  (when (= (char-before) close-bracket) 
    (insert "\n"))
  (lisp-indent-line)
  (save-excursion
    (languide-insert "(" name " " initial-value ")"))
  (indent-sexp))

(defmodal insert-global-variable-declaration
  (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  (name type initial-value)
  "Insert a definition for a global variable called NAME, of TYPE, with INITIAL-VALUE.
TYPE and INITIAL-VALUE may be null, but the NAME is required.
In fact, TYPE is not meaningful as this is the definition for Lisp."
  (languide-insert "\n(defvar " name " " initial-value "\n  \"\")\n"))

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
  (languide-insert (mapconcat 'arg-name arglist " ")))

(defmodal insert-function-declaration (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  (name result-type arglist
	body &optional docstring)
  "Insert a function definition for NAME, returning RESULT-TYPE, taking ARGLIST, and implemented by BODY.
A DOCSTRING may also be given."
  (languide-insert "(defun " name " (")
  (insert-lisp-arglist-elements arglist)
  (languide-insert ")")
  (newline-and-indent)
  (when (and (stringp docstring)
	     (not (zerop (length docstring))))
    (languide-insert "\"" docstring "\"")
    (newline-and-indent))
  (languide-insert body ")\n\n")
  (beginning-of-defun)
  (indent-sexp))

(defmodal insert-function-call (lisp-mode emacs-lisp-mode lisp-interaction-mode) (name arglist)
  "Insert a function call for a function called NAME taking ARGLIST"
  (languide-insert "(" name " ")
  (insert-lisp-arglist-elements arglist)
  (languide-insert ")"))

(defmodal languide-find-surrounding-call (lisp-mode emacs-lisp-mode lisp-interaction-mode) ()
  "Return a list of the function call syntax around point.
Each entry is a cons of start and end positions. For most languages
there will be two or three entries, the function name, the
start-of-call or start-of-args (may be merged with the function name),
and the end-of-call or end-of-args. Separators between arguments could
also be included. The caller should treat these as
coming in any order, and being in any quantity; thus, if using them to
modify the buffer, it is usually necessary to sort them and deal with
them in descending order of character position."
  (save-excursion
    (let ((bod (save-excursion (beginning-of-defun) (point))))
      (catch 'found
	(while (and (safe-backward-up-list)
		    (>= (point) bod))
	  (let ((open-bracket (cons (point) (1+ (point)))))
	    (when (save-excursion
		    (down-list)
		    (skip-to-actual-code)
		    (looking-at "\\(\\s_\\|\\sw\\)+"))
	      (let ((function-name (cons (match-beginning 0) (match-end 0))))
		(forward-sexp)
		(throw 'found (list function-name open-bracket (cons (1- (point)) (point))))))))
	nil))))

(defmodal languide-trim-whitespace (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  (syntax-before syntax-after)
  "Trim whitespace around point, in a language-dependent way.
The syntax classes of the non-space chars around point are passed in
as SYNTAX-BEFORE and SYNTAX-AFTER."
  ;; do this in two stages, as the second can happen after the first has
  (cond
   ((save-excursion
      (beginning-of-line)
      (looking-at "^\\(\\s-*\\))"))
    (delete-region (1- (match-beginning 1)) (match-end 1))))
  (cond
   ((save-excursion
      (beginning-of-line)
      (looking-at "^\\s-*$"))
    (delete-blank-lines)
    (delete-blank-lines)
    (when (looking-at "^(")
      (open-line 1)))
   ((and (eq syntax-before close-bracket)
	 (eq syntax-after open-bracket))
    (just-one-space))
   ((or (and (eq syntax-before open-bracket)
	     (memq syntax-after
		   '(open-bracket ?w ?_)))
	(and (eq syntax-after close-bracket)
	     (memq syntax-before
		   '(close-bracket ?w ?_))))
    (delete-horizontal-space))))

;; (defmodal function-arglist-boundaries (lisp-mode emacs-lisp-mode lisp-interaction-mode)
;;   (&optional where)
;;   "Return a cons of the start and end of the argument list surrounding WHERE,
;; or surrounding point if WHERE is not given.")

(defmodal deduce-expression-type
  (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  (value-text where)
  "Given VALUE-TEXT, try to deduce the type of it."
  nil)					; nice and easy for dynamically typed languages!

(defmodal add-expression-term (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  (operator argument from to)
  "Wrap an expression with OPERATOR and ARGUMENT around the region between FROM and TO."
  (goto-char to)
  (languide-insert ")")
  (goto-char from)
  (languide-insert "(" (symbol-name operator) " " argument " "))

(defmodal move-before-defun (lisp-mode emacs-lisp-mode lisp-interaction-mode) ()
  "Move to before the current function definition."
  (beginning-of-defun 1))

(defmodal static-variable-p (lisp-mode emacs-lisp-mode lisp-interaction-mode) (name where)
  "Return whether a static variable called NAME is visible at WHERE."
  (or (member name '("nil" "t"))
      (documentation-property (intern name) 'variable-documentation)
      (save-excursion
	(goto-char (point-min))
	(re-search-forward (format "(\\(\\(defvar\\)\\|\\(defconst\\)\\|\\(defcustom\\)\\) +%s" name)
			   (point-max) t)
	;; todo: should also look in everything we load with require
	)))

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
	    (let* ((names-start (safe-scan-lists keyend 1 -1))
		   (names-end (1- (safe-scan-sexps (1- names-start) 1)))
		   (name-start names-start)
		   (name-end (safe-scan-sexps name-start 1)))
	      (while (and name-end (<= name-end names-end))
		(push (list (buffer-substring-no-properties name-start name-end)
			    nil
			    keyend scope-end
			    nil)
		      result)
		(goto-char name-end)
		(skip-to-actual-code)
		(setq name-start (point)
		      name-end (safe-scan-sexps name-start 1)))))
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
		   (not (= preceding-char ?'))
		   (not (fourth pps))	; inside string
		   (not (fifth pps)))	; inside comment
	      (push (list (match-string-no-properties 0) where) result))
	  (goto-char end)))
      result)))

(defun count-sexps (from to)
  "Return the number of sexps between FROM and TO."
  (let ((i 0))
    (while (and from
		(< from to))
      (setq from (safe-scan-sexps from 1)
	    i (1+ i)))
    (if from
	i
      (1- i))))

(defmodal languide-region-type (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  (from to)
  "Try to work out what type of thing the code between FROM and TO is.
Results can be things like if-then-body, if-then-else-tail, progn-whole,
while-do-head, defun-body, and so on. If one of these is returned, the
code must be exactly that (apart from leading and trailing
whitespace).
If it is not recognizable as anything in particular, but ends at the
same depth as it starts, and never goes below that depth in between,
that is, is something that could be made into a compound statement or
expression, return t. 
Otherwise return nil."
  (let* ((pps (save-excursion (parse-partial-sexp from to))))
    (cond
     ((and (zerop (nth 0 pps))		; same level at both ends
	   (>= (nth 6 pps) 0))		; no dip in level between ends
      (let* ((n-parts (count-sexps from to))
	     (f-start (safe-scan-lists from 1 -1))
	     (f-end (and f-start
			 (safe-scan-sexps f-start 1)))
	     (functor (and f-end
			   (intern
			    (buffer-substring-no-properties f-start f-end))))
	     (surrounding-start (safe-scan-lists from -1 1))
	     (surrounding-end (and surrounding-start
				   (safe-scan-sexps surrounding-start 1)))
	     (sf-start (and surrounding-start
			    (safe-scan-lists surrounding-start 1 -1)))
	     (sf-end (and sf-start
			  (safe-scan-sexps sf-start 1)))
	     (surrounding-functor (and sf-end
				       (intern
					(buffer-substring-no-properties sf-start sf-end))))
	     (s-members (and sf-start
			     surrounding-end
			     (count-sexps sf-start (1- surrounding-end))))
	     (which-s-member (and sf-start
				  (count-sexps sf-start from))))
	(message "functor %S; surrounding-functor %S, of which we are %d of %d" functor surrounding-functor which-s-member s-members)
	(cond
	 ((eq functor 'defun) defun-body)
	 ;; todo: lots more to do here
	 ((memq functor '(progn save-excursion save-window-excursion)) 'progn-whole)
	 ((and (eq surrounding-functor 'if-then-else)
	       (= which-s-member 4))
	  'if-then-else-tail)
	 (t t))))
     (t nil)))
  )

(defmodal adjust-binding-point (lisp-mode emacs-lisp-mode lisp-interaction-mode) (variables-needed)
  "If appropriate, move to the first point at which all of VARIABLES-NEEDED are defined.
Assumes being at the end of a group of bindings, ready to insert a binding."
  (while (and (safe-backward-sexp)
	      (let ((this (save-excursion
			    (down-list)
			    (let ((start (point)))
			      (forward-sexp)
			      (message "considering %s" (buffer-substring-no-properties start (point)))
			      (if (not (member (buffer-substring-no-properties start (point))
					       variables-needed))
				  nil
				(point))))))
		(if (null this)
		    t
		  (goto-char this)
		  (up-list))))))

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
  ;; (keyword "defun")
  (head "(defun +" (expression-contents 2))
  (body "(defun +" (expression 2) (skip-to-actual-code) (expressions))
  (create (template & "(defun " (p "Function name to define: ")
		    " (" (p "Argument list: ") ")" n>
		    "\"" (p "Documentation string: ") "\"" n>
		    r> ")" n))
  (begin-end "(defun ()\n \"\"\n" ")"))

(defstatement defvar (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  ""
  ;; (keyword "defvar")
  (head "(defvar +" (expression-contents 2))
  (body "(defvar +" (expression 2) (skip-to-actual-code) (expressions))
  (create (template "(defvar " (p "Variable name to define: ")
		    "\"" (p "Documentation string: ") "\"" n>
		    r> ")" n)))

(defstatement progn (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  "General compound statement for Lisps"
  ;; (keyword "progn")
  (head "(progn" (expression-contents))
  (body "(progn" (skip-to-actual-code) (expressions))
  (create (template & > "(progn " p n>
		    r ")")))

(defstatement save-excursion (emacs-lisp-mode lisp-interaction-mode)
  ""
  ;; (keyword "save-excursion")
  (head "(save-excursion" (expression-contents))
  (body "(save-excursion" (skip-to-actual-code) (expressions))
  (create (template & > "(save-excursion " p n>
		    r ")")))

(defstatement save-window-excursion (emacs-lisp-mode lisp-interaction-mode)
  "Emacs-lisp \"save-window-excursion\" special form"
  ;; (keyword "save-window-excursion")
  (head "(save-window-excursion" (expression-contents))
  (body "(save-window-excursion" (skip-to-actual-code) (expressions))
  (create (template & > "(save-window-excursion " p n>
		    r ")")))

(defstatement while-do (emacs-lisp-mode lisp-interaction-mode)
  "Emacs-lisp \"while\" special form"
  ;; (keyword "while")
  (head "(while" (expression-contents))
  (body "(while" (expression) (skip-to-actual-code) (expressions))
  (create (template & > "(while " p n>
		    r ")"))
  (begin-end "(while " ")")
  (begin-end-with-dummy "(while true " ")"))

(defstatement unless (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  "Emacs-lisp \"unless\" special form."
  ;; (keyword "unless")
  (head "(unless" (expression-contents))
  (body "(unless" (expression) (skip-to-actual-code) (expression-contents))
  (create (precondition (require 'cl))
	  (template & > "(unless " p n>
		    r ")"))
  (begin-end "(unless " ")")
  (begin-end-with-dummy "(unless false " ")"))

(defstatement condition-chain (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  ""
  ;; (keyword "cond")
  (head "(cond *")
  (body "(cond *" (skip-to-actual-code) (expressions))
  (create (template & > "(cond" n> "(" p ")" n> "(t (" p " " p ")))")))

(defstatement function-call (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  ""
  (head "(" (expression))
  (body "(" (expression) (skip-to-actual-code) (expressions))
  (create (template > "(" p ")"))
  (begin-end "(" ")"))

(defstatement variable-declaration (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  ""
  ;; (keyword "let")
  (head "(let *(" (expressions))
  (body "(let *" (expression) (skip-to-actual-code) (expressions))
  (framework
   (remember "(") (remember "let") (remember "(") (expressions) (remember ")")
   (expressions) (remember ")"))
  (create
   (template & > "(let ((" (p "Variable name: ") p "))" n> r n> ")"))
  (begin-end "(let (())\n " ")"))

(defstatement assignment (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  "Assignment statement"
  ;; (keyword "setq")
  (head "(setq" (expression-contents))
  (body "(setq" (expression) (skip-to-actual-code) (expression))
  (create (template & > "(setq " (p "Variable name: ") " "
		    r ")")))

(defstatement if-then (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  "If statement without else clause."
  ;; (keyword "when")
  (head "(when" (expression-contents))
  (body "(when" (expression) (skip-to-actual-code) (expressions))
  (add-head (template & > "(if " r ")" n>))
  (create (precondition (require 'cl))
	  (template & > "(when " p n>
		    r ")"))
  (begin-end "(when \n" ")")
  (begin-end-with-dummy "(when true \n" ")"))

(defstatement if-then-else (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  "If statement with else clause."
  ;; (keyword "if")
  (head "(if" (expression))
  (body "(if" (expression) (skip-to-actual-code) (expression))
  (tail "(if" (expression) (expression) (skip-to-actual-code) (expressions))
  (add-head (template & > "(if " r ")" n>))
  (create (template & > "(if " p n>
		    r n>
		    p ")"))
  (begin-end "(if" ")")
  (begin-end-with-dummy "(if true " ")"))

(defstatement and (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  "And expression."
  ;; (keyword "and")
  (begin-end "(and " ")")
  (begin-end-with-dummy "(and true " ")"))

(defstatement or (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  "Or expression."
  ;; (keyword "or")
  (begin-end "(or " ")")
  (begin-end-with-dummy "(or false " ")"))

(defstatement not (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  "Not expression."
  ;; (keyword "not")
  (begin-end "(not " ")"))

;; Now put a "framework" and a "whole" in place for each statement
;; type -- these are the same for all lisp "statements":
(mapcar (lambda (mode)
	  (let ((statements (get mode 'statements)))
	    (mapcar (lambda (statement)
		      (unless (assoc 'framework (cdr statement))
			(rplacd statement
				(cons '(framework (remember "(")
						  (remember (expression))
						  (expressions)
						  (remember ")"))
				      (cdr statement))))
		      (unless (assoc 'whole (cdr statement))
			(rplacd statement
				(cons '(whole (expression))
				      (cdr statement)))))
		    statements)))
	'(lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode))

;;; end of languide-lisp-like.el
