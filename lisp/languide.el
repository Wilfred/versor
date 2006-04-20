;;;; languide.el -- language-guided editing
;;; Time-stamp: <2006-04-19 14:12:01 john>
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

;; This program provides language guided editing for a variety of
;; languages.  It aims to provide as single commands many of the
;; routine high-level editing actions that normally take many manual
;; editing operations, for example, taking a block of code and turning
;; it into a function, leaving a call to that function where the code
;; originally was. Although complex, and these actions are completely
;; stereotyped, and therefore programmable, as they do not really
;; required human intelligence.

;; The design of this package is built around the ideas of statements,
;; compound statements, and expressions. We have a general model of
;; statements, in which a statement can have head, body, and
;; optionally tail parts -- for example, the three parts of an
;; if-then-else statement. We also use the idea that each statement
;; (except for a top-level definition) is in a container.

;; To provide equivalent functionality across the range of supported
;; language modes, we use a modal functions package which lets us give
;; a function separate definitions in each major mode.

;; There are two levels to this package:
;;   The lower level is the mode-specific definitions,
;;     which come in separate files for the groups of modes
;;   The upper level is mode-independent functions, which are in:
;;     this file
;;     statement-navigation.el
;;     statement-definition.el
;;     languide-bindings.el
;;
;; Between the two are the model function definitions, which mostly
;; are in this file.

;; languide provides two groups of commands:
;;   high-level editing
;;   movement by statements (which you can use directly, or through
;;     versor (see emacs-versor on sourceforge), and which are used by
;;     the high-level editing commands)

;; In an attempt to break the tie with keyboard and mouse, on the
;; whole the commands are designed to work well with voice input. It
;; should be possible to work with very little need to type or
;; pronounce syntactic punctuation characters; these are detail that
;; the computer should be able to deal with, leaving the programmer to
;; get on with the abstract thinking. The need to issue a sequence of
;; commands with no punctuation leads naturally to an RPN-style
;; semantics, hence, for example, provision for selecting a statement
;; type and then searching for it, or inserting a template for it.

;; To use languide, you need to:
;;   add the directory containing this elisp file, and its companions, to your load-path
;;   load this file
;; This is designed to work with versor, so you may want to have that loaded too.
;; It is also designed with vr-mode in mind.

(provide 'languide)
(require 'cl)
(require 'modal-functions)
(require 'languide-custom)
(require 'versor-commands)		; for versor-as-motion-command
(require 'versor-base-moves)		; for safe-scan-lists
(require 'statement-navigation)
(require 'statement-definition)
(require 'languide-bindings)

;; todo: use Info-file-list-for-emacs

;;;; debugging

(defun languide-debug-message (function format &rest args)
  (when (and languide-debug-messages
	     (or (null languide-debug-functions)
		 (memq function languide-debug-functions)))
    (message "%s: %s (press key)" function (apply 'format format args))
    (read-char))
  (when (numberp languide-debug-messages)
    (sit-for languide-debug-messages)))

(defun outward-once ()
  "move outward one level of brackets, going backward.
returns point, if there was a bracket to go out of, else nil."
  (interactive)
  (let ((where (safe-scan-lists (point) -1 1)))
    (if where
	(progn
	  (goto-char where)
	  where)
      (progn
	;; todo: find why i thought i wanted to do this
	;; (goto-char (point-min))
	nil))))

(defun all-variables-in-scope-p (where variables)
  "return whether at where, all of variables are in scope."
  (message "looking for whether %s are all in scope at %d;" variables where)
  (if variables
      (let ((variables-in-scope (variables-in-scope where)))
	(message "variables in scope are %s" variables-in-scope)
	(catch 'done
	  (while variables
	    (if (assoc (car variables) variables-in-scope)
		(setq variables (cdr variables))
	      (message "%s was not in scope" (car variables))
	      (throw 'done nil)))
	  t))
    t))

(defmodel insert-compound-statement-open ()
  "insert the start of a compound statement")

(defmodel compound-statement-open ()
  "return a block start.")

(defmodel insert-compound-statement-close ()
  "insert the end of a compound statement")

(defmodel compound-statement-close ()
  "return a block end.")

(defmodel statement-container ()
  "select the container of the current statement.")

(defmodel insert-function-declaration (name result-type arglist body &optional docstring)
  "insert a function definition for name, returning result-type, taking arglist, and implemented by body.
a docstring may also be given.")

(defmodel insert-function-call (name arglist)
  "insert a function call for a function called name taking arglist.")

(defmodel function-arglist-boundaries (&optional where)
  "return a cons of the start and end of the argument list surrounding where,
or surrounding point if where is not given.")

(defmodel deduce-expression-type (value-text where)
  "given value-text, try to deduce the type of it.
second arg where gives the position, for context.")

(defmodel add-expression-term (operator argument from to)
  "wrap an expression with operator and argument around the region between from and to.")

(defmodel move-before-defun ()
  "move to before the current function definition.")

(defmodel languide-trim-whitespace (syntax-before syntax-after)
  "trim whitespace around point, in a language-dependent way.
the syntax classes of the non-space chars around point are passed in
as syntax-before and syntax-after.")

(defmodel languide-region-type (from to)
  "try to work out what type of thing the code between from and to is.
results can be things like if-then-body, if-then-else-tail, progn-whole,
while-do-head, defun-body, and so on. if one of these is returned, the
code must be exactly that (apart from leading and trailing
whitespace).
if it is not recognizable as anything in particular, but ends at the
same depth as it starts, and never goes below that depth in between,
that is, is something that could be made into a compound statement or
expression, return t. 
otherwise return nil.")

(defun show-region-type (start end)
  "for debugging languide-region-type"
  (interactive "r")
  (message "region type %S"
	   (languide-region-type (or start (region-beginning))
				 (or end (region-end)))))


(defvar languide-supported-modes
  '(c-mode java-mode perl-mode lisp-mode emacs-lisp-mode)
  "Modes for which languide has support.")

(defun versor-show-region-type-hook-function ()
  (when (memq major-mode languide-supported-modes)
    (let ((item (versor-get-current-item)))
      (show-region-type (car item) (cdr item)))))

;; (add-hook 'versor-post-command-hook 'versor-show-region-type-hook-function)
;; (remove-hook 'versor-post-command-hook 'versor-show-region-type-hook-function)

(defun backward-out-of-comment ()
  "if in a comment, move to just before it, else do nothing..
returns whether it did anything."
  (unless (memq major-mode '(texinfo-mode html-mode html-helper-mode))
    (let* ((bod (save-excursion (beginning-of-defun) (point)))
	   (parse-results (parse-partial-sexp bod (point)
					      0
					      nil
					      nil
					      nil))
	   (in-comment-or-string (nth 8 parse-results)))
      (if in-comment-or-string
	  (goto-char in-comment-or-string)
	nil))))

(defun skip-to-actual-code (&optional limit)
  "skip forward, over any whitespace or comments, to the next actual code.
this assumes that we start in actual code too.
limit, if given, limits the movement.
returns the new point."
  (interactive)
  (backward-out-of-comment)
  (while (progn
	   (skip-syntax-forward "->")
	   (languide-debug-message 'skip-to-actual-code "now at %d" (point))
	   (if (looking-at "\\s<")
	       (progn
		 (languide-debug-message 'skip-to-actual-code "at comment start at %d" (point))
		 (re-search-forward "\\s>" limit t))
	     (if (and (stringp comment-start-skip)
		      (stringp comment-end)
		      (looking-at comment-start-skip))
		 (progn
		   (languide-debug-message 'skip-to-actual-code "at comment-start-skip at %d" (point))
		   (goto-char (match-end 0))
		   (search-forward comment-end limit t))
	       nil))))
  (point))

(defun skip-to-actual-code-backwards (&optional limit)
  "Skip backward, over any whitespace or comments, to the next actual code.
This assumes that we start in actual code too.
LIMIT, if given, limits the movement.
Returns the new point."
  (interactive)
  (while (progn
	   (skip-syntax-backward "->")
	   (backward-out-of-comment)))
  (point))

;;;; define statements for some common languages

(require 'languide-c-like)
(require 'languide-sh-like)
(require 'languide-lisp-like)
(require 'languide-html-like)

(require 'languide-bindings)

;;;; Keymap

(defvar languide-keymap (make-sparse-keymap "Languide")
  "Keymap binding the languide operations.")

(define-key languide-keymap "g" 'languide-unify-statements)
(define-key languide-keymap "s" 'languide-enclosing-scoping-point)
(define-key languide-keymap "i" 'languide-enclosing-decision-point)
(define-key languide-keymap "u" 'languide-employ-variable)
(define-key languide-keymap "v" 'languide-convert-region-to-variable)
(define-key languide-keymap "d" 'languide-convert-region-to-function)
(define-key languide-keymap "(" 'surround-region-with-call)
(define-key languide-keymap "x" 'remove-surrounding-call)

;;; end of languide.el
