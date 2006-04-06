;;;; versor-voice.el
;;; Time-stamp: <2006-04-06 08:34:27 jcgs>
;;
;; emacs-versor -- versatile cursors for GNUemacs
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

(provide 'versor-voice)
(require 'versor)

(defvar vr-versor-dimension-command-list nil
  "Vocal movement commands for versor.
Generated automatically from the movement dimensions table, moves-moves.")

(defun singular (plural)
  "Try to make a plausible singular form of PLURAL."
  (cond
   ((string-match "\\([a-z]+\\)s" plural)
    (match-string 1 plural))
   ((string-match "\\([a-z]+\\)ren" plural)
    (match-string 1 plural))
   ((string-match "\\([a-z]+\\)en" plural)
    (match-string 1 plural))
   (t plural)))

(defun define-versor-vocal ()
  "Define the vocal commands."
  (interactive)
  (dolist (level (versor-all-level-names))
    (let* ((selector-function-name (intern (concat "versor-select-" level)))
	   (selector-function-body `(lambda ()
				      ,(format "Set the versor dimension to %s." level)
				      (interactive)
				      (versor-select-named-level ,level)))
	   )
      ;; (message "Defining %S to be %S" selector-function-name selector-function-body)
      (fset selector-function-name selector-function-body)
      (pushnew (cons level selector-function-name)
	       vr-versor-dimension-command-list
	       :test 'equal)
      (pushnew (cons (format "move by %s" level) selector-function-name)
	       vr-versor-dimension-command-list
	       :test 'equal))

    (dolist (action '("first" "previous" "next" "last"))
      (let* ((unit (singular level))
	     (action-function-name (intern (format "versor-voice-%s-%s" action unit)))
	     (action-voice-command (format "%s %s" action unit))
	     (action-function-body `(lambda (n)
				      ,(format "Move to the %s %s" action level)
				      (interactive "p")
				      (let* ((level-pair (versor-find-level-by-single-name ,level))
					     (versor-meta-level-shadow (car level-pair))
					     (versor-level-shadow (cdr level-pair))
					     (action (versor-get-action ',(intern action))))
					(funcall action n)))))
	(unless (boundp action-function-name)
	  (fset action-function-name action-function-body))
	(pushnew (cons action-voice-command action-function-name)
		 vr-versor-dimension-command-list
		 :test 'equal))))

  (dolist (meta-level (versor-meta-level-names))
    (let* ((name (car meta-level))
	   (symbol (intern (concat "versor-select-meta-" name)))
	   (body `(lambda ()
		    ,(format "Set the versor meta-dimension to %s." name)
		    (interactive)
		    (versor-select-named-meta-level ,name))))
      (fset symbol body)
      (pushnew (cons (format "%s movements" name) symbol)
	       vr-versor-dimension-command-list
	       :test 'equal))))

(if (null vr-versor-dimension-command-list) (define-versor-vocal))

(defvar vr-versor-command-list
  '(
    ("next one" . versor-next)
    ("onwards" . versor-next)
    ("back one" . versor-prev)
    ("previous one" . versor-prev)
    ("extend" . versor-extend-item-forwards)
    ("retract" . versor-extend-item-backwards)
    ;; ("out" . versor-out)
    ;; ("in" . versor-in)
    ("dimension in" . versor-in)
    ("dimension out" . versor-out)
    ("zoom movements in" . versor-in)
    ("zoom movements out" . versor-out)
    ("meta dimension in" . versor-prev-meta-level)
    ("meta dimension out" . versor-next-meta-level)
    ("first one" . versor-start)
    ("initial one" . versor-start)
    ("last one" . versor-end)
    ("final one" . versor-end)
    ("over" . versor-out-briefly)
    ("depth" . versor-select-in/out)
    ("expressions" . versor-select-exprs)
    ("characters" . versor-select-chars)
    ("end" . versor-end-of-item)
    ("reverse" . versor-reverse)
    ("beginning" . versor-start-of-item)
    ("other end" . versor-other-end-of-item)
    ("copy this" . versor-copy)
    ("cut this" . versor-kill)
    ("mark this" . versor-mark)
    ("transpose" . versor-transpose)
    ("valof" . wander-yank)
    ("result is" . pick-up-sexp-at-point)
    ("return" . exit-recursive-edit)
    ("alter this" . versor-begin-altering-item)
    ("that's it" . versor-end-altering-item)
    ("bingo" . versor-end-altering-item)
    ("accept" . versor-end-altering-item)
    ("revert to original" . versor-abandon-altering-item)
    ("container" . versor-select-container-contents)
    )
  "Individually defined versor voice commands (and friends).")

;;; end of versor-voice.el
