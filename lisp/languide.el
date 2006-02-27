;;;; languide.el -- language-guided editing
;;; Time-stamp: <2006-02-21 15:17:24 jcgs>
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
(require 'versor-commands) ; for versor:as-motion-command
(require 'statement-navigation)
(require 'statement-definition)
(require 'languide-bindings)

;; todo: use Info-file-list-for-emacs

;;;; debugging

(defvar languide:debug-messages t)
(defvar debug-functions '(beginning-of-statement-internal continue-back-past-curly-ket previous-statement navigate-to))

(defun languide:debug-message (function format &rest args)
  (when (and languide:debug-messages
	     (or (null debug-functions)
		 (memq function debug-functions)))
    (message "%S: %s" function (apply 'format format args)))
  (when (numberp languide:debug-messages)
    (sit-for languide:debug-messages)))

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
	;; todo: find why I thought I wanted to do this
	;; (goto-char (point-min))
	nil))))

(defun all-variables-in-scope-p (where variables)
  "Return whether at WHERE, all of VARIABLES are in scope."
  (let ((variables-in-scope (variables-in-scope where)))
    (message "Looking for whether %S are all in scope; variables in scope are %S" variables variables-in-scope)
    (catch 'done
      (while variables
	(if (assoc (car variables) variables-in-scope)
	    (setq variables (cdr variables))
	  (message "%s was not in scope" (car variables))
	  (throw 'done nil)))
      t)))

(defmodel insert-compound-statement-open ()
  "Insert the start of a compound statement")

(defmodel insert-compound-statement-close ()
  "Insert the end of a compound statement")

(defmodel statement-container ()
  "Select the container of the current statement.")

(defmodel insert-function-declaration (name result-type arglist body &optional docstring)
  "Insert a function definition for NAME, returning RESULT-TYPE, taking ARGLIST, and implemented by BODY.
A DOCSTRING may also be given.")

(defmodel insert-function-call (name arglist)
  "Insert a function call for a function called NAME taking ARGLIST")

(defmodel deduce-expression-type (value-text)
  "Given VALUE-TEXT, try to deduce the type of it.")

(defmodel move-before-defun ()
  "Move to before the current function definition.")

(defun skip-to-actual-code (&optional limit)
  "Skip forward, over any whitespace or comments, to the next actual code.
This assumes that we start in actual code too.
LIMIT, if given, limits the movement."
  (interactive)
  (parse-partial-sexp (point) (if limit limit (point-max))
		      0			; target-depth
		      t			; stop-before
		      nil		; state
		      nil		; stop-comment
		      )
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
