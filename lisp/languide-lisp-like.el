;;;; languide-lisp-like.el -- Lisp, Elisp, Scheme definitions for language-guided editing
;;; Time-stamp: <2004-05-20 11:39:14 john>
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
  (head "(defun +" (expression-contents 2))
  (body "(defun +" (expression 2) (expressions))
  (create (template & "(defun " (p "Function name to define: ")
		    " (" (p "Argument list: ") ")" n>
		    "\"" (p "Documentation string: ") "\"" n>
		    r> ")" n)))

(defstatement defvar (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  ""
  (head "(defvar +" (expression-contents 2))
  (body "(defvar +" (expression 2) (expressions))
  (create (template "(defvar " (p "Variable name to define: ")
		    "\"" (p "Documentation string: ") "\"" n>
		    r> ")" n)))

(defstatement progn (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  "General compound statement for Lisps"
  (head "(progn" (expression-contents))
  (body "(progn" (expressions))
  (create (template & > "(progn " p n>
		    r ")")))

(defstatement save-excursion (emacs-lisp-mode lisp-interaction-mode)
  ""
  (head "(save-excursion" (expression-contents))
  (body "(save-excursion" (expressions))
  (create (template & > "(save-excursion " p n>
		    r ")")))

(defstatement save-window-excursion (emacs-lisp-mode lisp-interaction-mode)
  "Emacs-lisp \"save-window-excursion\" special form"
  (head "(save-window-excursion" (expression-contents))
  (body "(save-window-excursion" (expressions))
  (create (template & > "(save-window-excursion " p n>
		    r ")")))

(defstatement while (emacs-lisp-mode lisp-interaction-mode)
  "Emacs-lisp \"while\" special form"
  (head "(while" (expression-contents))
  (body "(while" (expression) (expressions))
  (create (template & > "(while " p n>
		    r ")")))

(defstatement unless (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  ""
  (head "(unless" (expression-contents))
  (body "(unless" (expression) (expression-contents))
  (create (precondition (require 'cl))
	  (template & > "(unless " p n>
		    r ")")))

(defstatement condition-chain (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  ""
  (head "(cond *")
  (body "(cond *" (expressions))
  (create (template & > "(cond" n> "(" p ")" n> "(t (" p " " p ")))")))

(defstatement function-call (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  ""
  (head "(" (expression))
  (body "(" (expression) (expressions))
  (create (template > "(" p ")")))

(defstatement variable-declaration (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  ""
  (head "(let *(" (expressions))
  (body "(let *" (expression) (expressions))
  (create
   (template & > "(let ((" (p "Variable name: ") p "))" n> r n> ")")))

(defstatement assignment (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  "Assignment statement"
  (head "(setq" (expression-contents))
  (body "(setq" (expression) (expression))
  (create (template & > "(setq " (p "Variable name: ") " "
		    r ")")))

(defstatement if-then (lisp-mode emacs-lisp-mode lisp-interaction-mode)
  "If statement without else clause."
  (head "(when" (expression-contents))
  (body "(when" (expression) (expressions))
  (create (precondition (require 'cl))
	  (template & > "(when " p n>
		    r ")")))

(defstatement if-then-else (lisp-mode emacs-lisp-mode lisp-interaction-mode scheme-mode)
  "If statement with else clause."
  (head "(if" (expression-contents))
  (body "(if" (expression) (expression-contents))
  (tail "(if" (expression) (expression) (expression-contents))
  (create (template & > "(if " p n>
		    r n>
		    p ")")))
