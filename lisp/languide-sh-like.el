;;;; languide-sh-like.el -- shell etc definitions for language-guided editing
;;; Time-stamp: <2006-03-06 16:56:27 jcgs>
;;
;; Copyright (C) 2004, 2006  John C. G. Sturdy
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

(provide 'languide-sh-like)

(defmodal move-into-previous-statement (sh-mode) ()
  "Move into the previous statement.
This need be valid only after a beginning-of-statement-internal.
It should move point back such that another beginning-of-statement-internal
will go back another statement."
)

(defmodal move-into-next-statement (sh-mode) ()
  "Move into the next statement.
This need be valid only after an end-of-statement-internal.
It should move point forward such that another end-of-statement-internal
will go forward another statement."
)

(defmodal beginning-of-statement-internal (sh-mode) ()
  "Move to the beginning of the statement.
Do not do auxiliary stuff that might be associated with this."
)

(defmodal end-of-statement-internal (sh-mode) ()
  "Move to the end of the current statement.
Do not do auxiliary stuff that might be associated with this."
)

(defmodal identify-statement (sh-mode) (default)
  "Note what kind of statement we are at the start of.
Need not work if not at the start of a statement.
If the statement cannot be identified, return DEFAULT."
)

(defmodal compound-statement-open (sh-mode) ()
  "Return a block start."
)

(defmodal compound-statement-close (sh-mode) ()
  "Return a block end."
)

(defmodal insert-compound-statement-open (sh-mode) ()
  "Insert the start of a compound statement"
)

(defmodal insert-compound-statement-close (sh-mode) ()
  "Insert the end of a compound statement"
)

(defmodal binding-around (sh-mode) (whereat)
  "Return the shell variable definition around WHEREAT."
  (save-match-data
    (save-excursion
      (goto-char whereat)
      (beginning-of-line 1)
      (if (looking-at "^\\(export +\\)?\\([^=]+\\)=\\(.+\\)$")
	  (list (match-string 2)
		(match-string 3)
		(match-beginning 2) (match-end 2)
		(match-beginning 3) (match-end 3))
	nil))))

(defmodal scope-around (sh-mode) (whereat)
  "Return the scope (start and end) around WHEREAT."
  (save-excursion
    (let* ((start (safe-scan-lists whereat -1 1))
	   (end
	    (if start
		(progn
		  (goto-char start)
		  (forward-sexp 1)
		  (point))
	      (point-max))))
      (list (if start start (point-min))
	    end))))

(defmodal variable-reference (sh-mode) (varname)
  "Make a variable reference from VARNAME"
  (concat "$" varname))

(defstatement comment (c-mode)
  "Comment"
  (head "#")
  (body "#" (upto "\n"))
  (create (template "# " r "\n")))

(defstatement if-then (sh-mode)
  "If statement without else clause."
  (head "if" (upto "then"))
  (body "if" (upto "then") "then" (statements))
  (framework (remember "if") (remember "then")
	     (skip-to-actual-code) (continue-if "{")
	     (remember "{") (expressions) (remember "}"))
  (create (template & > "if " p n>
		    "then\n"
		    r n> "\nfi\n"))
  (begin-end "if \nthen\n" "\nfi\n")
  (begin-end-with-dummy "if true\nthen\n" end "\nfi\n"))

(defstatement if-then-else (sh-mode)
  "If statement with else clause."
  (head "if" (expression-contents))
  (body "if" (expression) (statement-contents))
  (tail "if" (expression) (statement) "else" (statement-contents))
  (framework (remember "if") (remember "(") (expressions) (remember ")")
	     (skip-to-actual-code)
	     (if "{"
		 ((remember "{") (statements) (remember "}"))
	       (statement))
	     (remember "else")
	     (skip-to-actual-code)
	     (continue-if "{")
	     (remember "{") (statements) (remember "}"))
  (create (template & > "if (" p ") {" n>
		    r "} else {"n>
		    p "}"))
  (begin-end "if () {" "} else {}")
  (begin-end-with-dummy "if (1) {" "} else {}"))

(defstatement while-do (sh-mode)
  "While statement."
  (head "while" (expression-contents))
  (body "while" (expression) (statement-contents))
  (create (template & > "while (" p ") {" n>
		    r "}"))
  (begin-end "while () {" "}")
  (begin-end-with-dummy "while (1) {" "}"))

(defstatement for (sh-mode)
  "For statement."
  (head "for" (expression-contents))
  (body "for" (expression) (statement-contents))
  (framework (remember "for") (remember "(") (expressions) (remember ")")
	     (remember "{") (statements) (remember "}"))
  (create (template & > "for (" p ";" p ";" p ") {" n>
		    r "}")))

(defstatement switch (sh-mode)
  "Switch statement"
  (head "switch" (expression-contents))
  (body "switch" (expression) (statement-contents))
  (framework (remember "switch") (remember "(") (expressions) (remember ")")
	     (remember "{") (statements) (remember "}"))
  (create (template & > "switch (" p ";" p ";" p ") {" n>
		    r "}")))

(defstatement variable-declaration (sh-mode)
  "Local variable"
  ;; todo: recognize local variables with and without initial values, as separate statement types
  (head "=" (start-of-match) (from-start-of-statement))
  (body "=" (upto ";"))
  (framework (remember "=") (remember ";"))
  (create (template & > " " p ";" n)))

(defstatement assignment (sh-mode)
  "Assignment"
  (head "=" (start-of-match) (from-start-of-statement))
  (body "=" (upto ";"))
  (framework (remember "=") (remember ";"))
  (create (template & > (p "Variable: ") " = " r ";")))

;;; end of languide-sh-like.el
